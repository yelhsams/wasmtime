use anyhow::Context as _;
use clap::Parser;
use cranelift_codegen::isa::aarch64::inst::*;
use cranelift_codegen::settings;
use cranelift_codegen::MachBuffer;
use cranelift_codegen::MachInstEmit;
use crossbeam::queue::SegQueue;
use isla_lib::bitvector::{b129::B129, BV};
use isla_lib::config::ISAConfig;
use isla_lib::error::IslaError;
use isla_lib::executor::{self, LocalFrame, TaskState};
use isla_lib::init::initialize_architecture;
use isla_lib::ir::{AssertionMode, Def, IRTypeInfo, Name, Symtab, Val};
use isla_lib::ir_lexer::new_ir_lexer;
use isla_lib::ir_parser;
use isla_lib::memory::Memory;
use isla_lib::simplify::{self, WriteOpts};
use isla_lib::smt::{self, Checkpoint, Event, Solver};
use sha2::{Digest, Sha256};
use std::fs::File;
use std::io::prelude::*;
use std::io::{self, BufWriter, Read};
use std::path::{Path, PathBuf};
use std::sync::Arc;

#[derive(Parser)]
struct Options {
    /// Architecture definition.
    #[clap(long)]
    arch: PathBuf,

    /// ISA config file.
    #[clap(long)]
    isa_config: PathBuf,
}

fn main() -> anyhow::Result<()> {
    let options = Options::parse();

    // Parse ISLA Architecture.
    let contents = read_to_string(&options.arch)?;
    let mut symtab = Symtab::new();
    let mut arch = parse_ir::<B129>(&contents, &mut symtab)?;

    // ISLA ISA Config.
    let mut hasher = Sha256::new();
    let type_info = IRTypeInfo::new(&arch);
    let isa_config = match ISAConfig::<B129>::from_file(
        &mut hasher,
        &options.isa_config,
        None,
        &symtab,
        &type_info,
    ) {
        Ok(isa_config) => isa_config,
        Err(msg) => anyhow::bail!(msg),
    };

    // Assemble an instruction.
    let inst = Inst::AluRRR {
        alu_op: ALUOp::Add,
        size: OperandSize::Size64,
        rd: writable_xreg(4),
        rn: xreg(5),
        rm: xreg(6),
    };

    let opcodes = opcodes(&inst);
    println!("opcode = {:?}", opcodes);

    // ISLA trace.
    assert_eq!(opcodes.len(), 1);
    trace_opcode(
        opcodes[0],
        &mut arch,
        symtab.clone(),
        type_info.clone(),
        &isa_config,
    )?;

    Ok(())
}

fn assemble(inst: &Inst) -> Vec<u8> {
    let flags = settings::Flags::new(settings::builder());
    let emit_info = EmitInfo::new(flags);
    let mut buffer = MachBuffer::new();
    inst.emit(&[], &mut buffer, &emit_info, &mut Default::default());
    let buffer = buffer.finish(&Default::default(), &mut Default::default());
    return buffer.data().to_vec();
}

fn opcodes(inst: &Inst) -> Vec<u32> {
    let machine_code = assemble(&inst);
    let mut opcodes = Vec::new();
    for opcode_bytes in machine_code.chunks(4) {
        assert_eq!(opcode_bytes.len(), 4);
        opcodes.push(u32::from_le_bytes(opcode_bytes.try_into().unwrap()));
    }
    opcodes
}

fn trace_opcode<B: BV>(
    opcode: u32,
    arch: &mut Vec<Def<Name, B>>,
    symtab: Symtab,
    type_info: IRTypeInfo,
    isa_config: &ISAConfig<B>,
) -> anyhow::Result<()> {
    let use_model_reg_init = true;
    let iarch = initialize_architecture(
        arch,
        symtab,
        type_info,
        &isa_config,
        AssertionMode::Optimistic,
        use_model_reg_init,
    );
    let shared_state = &&iarch.shared_state;

    let initial_checkpoint = Checkpoint::new();
    let solver_cfg = smt::Config::new();
    let solver_ctx = smt::Context::new(solver_cfg);
    let mut solver = Solver::from_checkpoint(&solver_ctx, initial_checkpoint);
    let checkpoint = smt::checkpoint(&mut solver);

    let opcode_val = Val::Bits(B::from_u32(opcode));

    let footprint_function = "zisla_footprint";
    let function_id = shared_state.symtab.lookup(&footprint_function);
    let (args, ret_ty, instrs) = shared_state.functions.get(&function_id).unwrap();
    let memory = Memory::new();
    let task_state = TaskState::<B>::new();
    let task = LocalFrame::new(function_id, args, ret_ty, Some(&[opcode_val]), instrs)
        .add_lets(&iarch.lets)
        .add_regs(&iarch.regs)
        .set_memory(memory)
        .task_with_checkpoint(0, &task_state, checkpoint);

    let num_threads = 1;
    let queue = Arc::new(SegQueue::new());
    executor::start_multi(
        num_threads,
        None,
        vec![task],
        shared_state,
        queue.clone(),
        &executor::trace_collector,
    );

    loop {
        match queue.pop() {
            Some(Ok((_, mut events))) => {
                simplify::hide_initialization(&mut events);
                simplify::remove_unused(&mut events);
                simplify::propagate_forwards_used_once(&mut events);
                simplify::commute_extract(&mut events);
                simplify::eval(&mut events);

                let events: Vec<Event<B>> = events.drain(..).rev().collect();
                let stdout = std::io::stdout();
                let mut handle = BufWriter::with_capacity(5 * usize::pow(2, 20), stdout.lock());
                let write_opts = WriteOpts::default();
                simplify::write_events_with_opts(&mut handle, &events, &shared_state, &write_opts)
                    .unwrap();
                handle.flush().unwrap()
            }

            // Error during execution
            Some(Err(err)) => {
                let msg = format!("{}", err);
                eprintln!(
                    "{}",
                    err.source_loc().message::<PathBuf>(
                        None,
                        shared_state.symtab.files(),
                        &msg,
                        true,
                        true
                    )
                );
                anyhow::bail!("{}", err);
            }
            // Empty queue
            None => break,
        }
    }

    Ok(())
}

fn parse_ir<'a, 'input, B: BV>(
    contents: &'input str,
    symtab: &'a mut Symtab<'input>,
) -> anyhow::Result<Vec<Def<Name, B>>> {
    match ir_parser::IrParser::new().parse(symtab, new_ir_lexer(&contents)) {
        Ok(ir) => Ok(ir),
        Err(_) => Err(anyhow::Error::msg("bad")),
    }
}

/// Read an entire file into a string.
fn read_to_string<P: AsRef<Path>>(path: P) -> anyhow::Result<String> {
    let mut buffer = String::new();
    let path = path.as_ref();
    if path == Path::new("-") {
        let stdin = io::stdin();
        let mut stdin = stdin.lock();
        stdin
            .read_to_string(&mut buffer)
            .context("failed to read stdin to string")?;
    } else {
        let mut file = File::open(path)?;
        file.read_to_string(&mut buffer)
            .with_context(|| format!("failed to read {} to string", path.display()))?;
    }
    Ok(buffer)
}
