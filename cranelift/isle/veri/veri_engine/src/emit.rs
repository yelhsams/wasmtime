use cranelift_codegen::isa::aarch64::inst::*;
use cranelift_codegen::settings;
use cranelift_codegen::MachBuffer;
use cranelift_codegen::MachInstEmit;

fn main() -> anyhow::Result<()> {
    let inst = Inst::AluRRR {
        alu_op: ALUOp::Add,
        size: OperandSize::Size64,
        rd: writable_xreg(4),
        rn: xreg(5),
        rm: xreg(6),
    };

    let opcodes = opcodes(&inst);
    println!("opcode = {:?}", opcodes);

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
