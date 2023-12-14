use anyhow::Context as _;
use clap::Parser;
use cranelift_codegen::isa::aarch64::inst::*;
use cranelift_codegen::settings;
use cranelift_codegen::MachBuffer;
use cranelift_codegen::MachInstEmit;
use crossbeam::queue::SegQueue;
use isla_lib::bitvector::{b64::B64, BV};
use isla_lib::config::ISAConfig;
use isla_lib::error::IslaError;
use isla_lib::executor::{self, LocalFrame, TaskState};
use isla_lib::init::{initialize_architecture, Initialized};
use isla_lib::ir::{AssertionMode, Def, IRTypeInfo, Name, Symtab, Val};
use isla_lib::ir_lexer::new_ir_lexer;
use isla_lib::ir_parser;
use isla_lib::memory::Memory;
use isla_lib::simplify::{self, WriteOpts};
use isla_lib::smt::smtlib;
use isla_lib::smt::{self, Checkpoint, Event, Solver};
use sha2::{Digest, Sha256};
use std::collections::{HashMap, HashSet};
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
        Err(msg) => anyhow::bail!(msg),
    };

    let use_model_reg_init = true;
    let iarch = initialize_architecture(
        &mut arch,
        symtab,
        type_info,
        &isa_config,
        AssertionMode::Optimistic,
        use_model_reg_init,
    );

    // Assemble an instruction.
    let inst = Inst::AluRRR {
        alu_op: ALUOp::Add,
        size: OperandSize::Size64,
        rd: writable_xreg(4),
        rn: xreg(5),
        rm: xreg(6),
    };

    let opcodes = opcodes(&inst);
    assert_eq!(opcodes.len(), 1);

    // ISLA trace.
    let paths = trace_opcode(opcodes[0], &iarch)?;

    // Dump.
    for events in paths {
        let filtered = tree_shake(&events);
        write_events(&filtered, &iarch)?;
    }

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

fn trace_opcode<'ir, B: BV>(
    opcode: u32,
    iarch: &'ir Initialized<'ir, B>,
) -> anyhow::Result<Vec<Vec<Event<B>>>> {
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

    let mut paths = Vec::new();
    loop {
        match queue.pop() {
            Some(Ok((_, mut events))) => {
                simplify::hide_initialization(&mut events);
                simplify::remove_extra_register_fields(&mut events);
                simplify::remove_repeated_register_reads(&mut events);
                simplify::remove_unused_register_assumptions(&mut events);
                simplify::remove_unused(&mut events);
                simplify::propagate_forwards_used_once(&mut events);
                simplify::commute_extract(&mut events);
                simplify::eval(&mut events);

                let events: Vec<Event<B>> = events.drain(..).rev().collect();
                paths.push(events);
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

    Ok(paths)
}

fn event_writes<B: BV>(event: &Event<B>) -> Option<smt::Sym> {
    match event {
        Event::Smt(def, _, _) => match def {
            smtlib::Def::DefineConst(v, _) => Some(*v),
            _ => None,
        },
        Event::ReadReg(_, _, val) => match val {
            Val::Symbolic(sym) => Some(*sym),
            _ => None,
        },
        _ => None,
    }
}

fn defns<B: BV>(events: &Vec<Event<B>>) -> HashMap<smt::Sym, usize> {
    let mut defn_idx = HashMap::new();
    for (i, event) in events.iter().enumerate() {
        if let Some(sym) = event_writes(event) {
            defn_idx.insert(sym, i);
        }
    }
    defn_idx
}

fn exp_uses(exp: &smtlib::Exp<smt::Sym>) -> HashSet<smt::Sym> {
    use smtlib::Exp::*;
    match exp {
        Var(sym) => HashSet::from([*sym]),
        //Bits(_) | Bits64(_) | Enum(_) | Bool(_) | FPConstant(..) | FPRoundingMode(_) => (),
        Not(exp)
        | Bvnot(exp)
        | Bvneg(exp)
        | Extract(_, _, exp)
        | ZeroExtend(_, exp)
        | SignExtend(_, exp)
        | FPUnary(_, exp) => exp_uses(exp),
        Eq(lhs, rhs)
        | Neq(lhs, rhs)
        | And(lhs, rhs)
        | Or(lhs, rhs)
        | Bvand(lhs, rhs)
        | Bvor(lhs, rhs)
        | Bvxor(lhs, rhs)
        | Bvnand(lhs, rhs)
        | Bvnor(lhs, rhs)
        | Bvxnor(lhs, rhs)
        | Bvadd(lhs, rhs)
        | Bvsub(lhs, rhs)
        | Bvmul(lhs, rhs)
        | Bvudiv(lhs, rhs)
        | Bvsdiv(lhs, rhs)
        | Bvurem(lhs, rhs)
        | Bvsrem(lhs, rhs)
        | Bvsmod(lhs, rhs)
        | Bvult(lhs, rhs)
        | Bvslt(lhs, rhs)
        | Bvule(lhs, rhs)
        | Bvsle(lhs, rhs)
        | Bvuge(lhs, rhs)
        | Bvsge(lhs, rhs)
        | Bvugt(lhs, rhs)
        | Bvsgt(lhs, rhs)
        | Bvshl(lhs, rhs)
        | Bvlshr(lhs, rhs)
        | Bvashr(lhs, rhs)
        | Concat(lhs, rhs)
        | FPBinary(_, lhs, rhs) => {
            let lhs_uses = exp_uses(lhs);
            let rhs_uses = exp_uses(rhs);
            &lhs_uses | &rhs_uses
        }
        //Ite(cond, then_exp, else_exp) => {
        //    uses_in_exp(uses, cond);
        //    uses_in_exp(uses, then_exp);
        //    uses_in_exp(uses, else_exp)
        //}
        //App(f, args) => {
        //    uses.insert(*f, uses.get(f).unwrap_or(&0) + 1);
        //    for arg in args {
        //        uses_in_exp(uses, arg);
        //    }
        //}
        //Select(array, index) => {
        //    uses_in_exp(uses, array);
        //    uses_in_exp(uses, index)
        //}
        //Store(array, index, val) => {
        //    uses_in_exp(uses, array);
        //    uses_in_exp(uses, index);
        //    uses_in_exp(uses, val)
        //}
        //Distinct(exps) => {
        //    for exp in exps {
        //        uses_in_exp(uses, exp);
        //    }
        //}
        //FPRoundingUnary(_, rm, exp) => {
        //    uses_in_exp(uses, rm);
        //    uses_in_exp(uses, exp);
        //}
        //FPRoundingBinary(_, rm, lhs, rhs) => {
        //    uses_in_exp(uses, rm);
        //    uses_in_exp(uses, lhs);
        //    uses_in_exp(uses, rhs)
        //}
        //FPfma(rm, x, y, z) => {
        //    uses_in_exp(uses, rm);
        //    uses_in_exp(uses, x);
        //    uses_in_exp(uses, y);
        //    uses_in_exp(uses, z)
        //}
        _ => HashSet::new(),
    }
}

fn smt_def_uses(def: &smtlib::Def) -> HashSet<smt::Sym> {
    match def {
        // DeclareConst(Sym, Ty),
        // DeclareFun(Sym, Vec<Ty>, Ty),
        smtlib::Def::DefineConst(_, exp) => exp_uses(&exp),
        // DefineEnum(Name, usize),
        // Assert(Exp<Sym>),
        _ => HashSet::new(),
    }
}

fn val_uses<B: BV>(val: &Val<B>) -> HashSet<smt::Sym> {
    // See: simplify::uses_in_value
    match val {
        Val::Symbolic(sym) => HashSet::from([*sym]),
        // MixedBits(segments) => segments.iter().for_each(|segment| match segment {
        //     BitsSegment::Symbolic(v) => {
        //         uses.insert(*v, uses.get(v).unwrap_or(&0) + 1);
        //     }
        //     BitsSegment::Concrete(_) => (),
        // }),
        // I64(_) | I128(_) | Bool(_) | Bits(_) | Enum(_) | String(_) | Unit | Ref(_) | Poison => (),
        // List(vals) | Vector(vals) => vals.iter().for_each(|val| uses_in_value(uses, val)),
        // Struct(fields) => fields.iter().for_each(|(_, val)| uses_in_value(uses, val)),
        // Ctor(_, val) => uses_in_value(uses, val),
        // SymbolicCtor(v, possibilities) => {
        //     uses.insert(*v, uses.get(v).unwrap_or(&0) + 1);
        //     possibilities
        //         .iter()
        //         .for_each(|(_, val)| uses_in_value(uses, val))
        // }
        _ => HashSet::new(),
    }
}

fn uses<B: BV>(event: &Event<B>) -> HashSet<smt::Sym> {
    match event {
        Event::Smt(def, _, _) => smt_def_uses(&def),
        Event::WriteReg(_, _, val) => val_uses(val),
        _ => HashSet::new(),
    }
}

fn tree_shake<B: BV>(events: &Vec<Event<B>>) -> Vec<Event<B>> {
    // Definitions.
    let defn_idx = defns(events);

    // Work list: populate with register writes.
    let mut work_list = Vec::new();
    let mut live = HashSet::new();
    for (i, event) in events.iter().enumerate() {
        match event {
            Event::WriteReg(_, _, val) => match val {
                Val::Symbolic(sym) => {
                    // Mark live.
                    live.insert(i);

                    // Push the variable to be visited.
                    let d = defn_idx[&sym];
                    live.insert(d);
                    work_list.push(d);
                }
                _ => continue,
            },
            _ => continue,
        };
    }

    // Process.
    while !work_list.is_empty() {
        let i = work_list.pop().unwrap();
        assert!(live.contains(&i), "visited events should be live");
        let event = &events[i];

        // Mark uses live.
        for u in uses(&event) {
            // Lookup definition of this dependency.
            let ui = defn_idx[&u];
            if live.contains(&ui) {
                continue;
            }

            live.insert(ui);
            work_list.push(ui);
        }
    }

    events
        .iter()
        .enumerate()
        .filter_map(|(i, event)| if live.contains(&i) { Some(event) } else { None })
        .cloned()
        .collect()
}

fn write_events<'ir, B: BV>(
    events: &Vec<Event<B>>,
    iarch: &'ir Initialized<'ir, B>,
) -> anyhow::Result<()> {
    // Print.
    let stdout = std::io::stdout();
    let mut handle = BufWriter::with_capacity(5 * usize::pow(2, 20), stdout.lock());
    let write_opts = WriteOpts::default();
    simplify::write_events_with_opts(&mut handle, &events, &iarch.shared_state, &write_opts)
        .unwrap();
    handle.flush().unwrap();

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
