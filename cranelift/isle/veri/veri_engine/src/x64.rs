use cranelift_codegen::isa::x64::{
    self,
    inst::{args::*, *},
};
use cranelift_codegen::{settings, MachBuffer, MachInstEmit, Reg, Writable};

fn main() -> anyhow::Result<()> {
    let rdx = regs::rdx();
    let r13 = regs::r13();
    let w_rdx = Writable::<Reg>::from_reg(rdx);
    let inst = Inst::AluRmiR {
        size: OperandSize::Size64,
        op: AluRmiROpcode::Add,
        src1: Gpr::new(rdx).unwrap(),
        src2: GprMemImm::new(RegMemImm::reg(r13)).unwrap(),
        dst: WritableGpr::from_writable_reg(w_rdx).unwrap(),
    };

    let machine_code = assemble(&inst);
    println!("{:02x?}", machine_code);

    Ok(())
}

fn assemble(inst: &Inst) -> Vec<u8> {
    let flags = settings::Flags::new(settings::builder());
    let isa_flag_builder = x64::settings::builder();
    let isa_flags = x64::settings::Flags::new(&flags, &isa_flag_builder);
    let emit_info = EmitInfo::new(flags, isa_flags);
    let mut buffer = MachBuffer::new();
    inst.emit(&[], &mut buffer, &emit_info, &mut Default::default());
    let buffer = buffer.finish(&Default::default(), &mut Default::default());
    return buffer.data().to_vec();
}
