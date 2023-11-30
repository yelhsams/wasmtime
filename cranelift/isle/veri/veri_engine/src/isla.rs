use anyhow::Context as _;
use clap::Parser;
use cranelift_codegen::print_errors::pretty_error;
use cranelift_codegen::Context;
use cranelift_reader::{parse_sets_and_triple, parse_test, ParseOptions};
use isla_lib::config::ISAConfig;
use isla_lib::smt::Checkpoint;
use sha2::{Digest, Sha256};
use std::fs::File;
use std::io::{self, Read};
use std::path::{Path, PathBuf};

use crossbeam::queue::SegQueue;
use isla_lib::bitvector::b64::B64;
use isla_lib::bitvector::BV;
use isla_lib::executor::{self, LocalFrame, TaskState};
use isla_lib::init::initialize_architecture;
use isla_lib::ir::{AssertionMode, Def, IRTypeInfo, Name, Symtab, Val};
use isla_lib::ir_lexer::new_ir_lexer;
use isla_lib::ir_parser;
use isla_lib::memory::Memory;
use std::sync::Arc;

#[derive(Parser)]
struct Options {
    /// Configure Cranelift settings.
    #[clap(long = "set")]
    settings: Vec<String>,

    /// Specify the Cranelift target.
    #[clap(long = "target")]
    target: String,

    /// Architecture definition.
    #[clap(long)]
    arch: PathBuf,

    /// ISA config file.
    #[clap(long)]
    isa_config: PathBuf,

    /// Specify an input file to be used. Use '-' for stdin.
    file: PathBuf,
}

fn main() -> anyhow::Result<()> {
    let options = Options::parse();

    // Parse input CLIF test file.
    let buffer = read_to_string(&options.file)?;
    let test_file = parse_test(&buffer, ParseOptions::default())
        .with_context(|| format!("failed to parse {}", options.file.to_string_lossy()))?;

    // Determine ISA settings.
    let parsed = parse_sets_and_triple(&options.settings, &options.target)?;
    let fisa = parsed.as_fisa();
    let isa = fisa.isa.or(test_file.isa_spec.unique_isa());

    let isa = match isa {
        None => anyhow::bail!("compilation requires a target isa"),
        Some(isa) => isa,
    };

    // Parse ISLA Architecture.
    let contents = read_to_string(&options.arch)?;
    let mut symtab = Symtab::new();
    let mut arch = parse_ir::<B64>(&contents, &mut symtab)?;

    // ISLA ISA Config.
    let mut hasher = Sha256::new();
    let type_info = IRTypeInfo::new(&arch);
    let isa_config = match ISAConfig::<B64>::from_file(
        &mut hasher,
        &options.isa_config,
        None,
        &symtab,
        &type_info,
    ) {
        Ok(isa_config) => isa_config,
        Err(msg) => return Err(anyhow::Error::msg(msg)),
    };

    // Compile functions.
    for (func, _) in test_file.functions {
        let mut context = Context::for_function(func);
        let mut mem = vec![];
        let _compiled_code = context
            .compile_and_emit(isa, &mut mem, &mut Default::default())
            .map_err(|err| anyhow::anyhow!("{}", pretty_error(&err.func, err.inner)))?;
        trace_function(
            &mem,
            &mut arch,
            symtab.clone(),
            type_info.clone(),
            &isa_config,
        );
    }

    Ok(())
}

fn trace_function<B: BV>(
    mc: &[u8],
    arch: &mut Vec<Def<Name, B>>,
    symtab: Symtab,
    type_info: IRTypeInfo,
    isa_config: &ISAConfig<B>,
) {
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

    let opcode = Val::Bits(B::from_bytes(mc));

    let footprint_function = "zisla_footprint";
    let function_id = shared_state.symtab.lookup(&footprint_function);
    let (args, ret_ty, instrs) = shared_state.functions.get(&function_id).unwrap();
    let memory = Memory::new();
    let task_state = TaskState::<B>::new();
    let initial_checkpoint = Checkpoint::new();
    let task = LocalFrame::new(function_id, args, ret_ty, Some(&[opcode]), instrs)
        .add_lets(&iarch.lets)
        .add_regs(&iarch.regs)
        .set_memory(memory)
        .task_with_checkpoint(0, &task_state, initial_checkpoint);
    // task.set_stop_conditions(&stop_conditions);

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
