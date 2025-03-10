//! Riscv64 ISA: binary code emission.

use crate::binemit::StackMap;
use crate::ir::{self, LibCall, RelSourceLoc, TrapCode};
use crate::isa::riscv64::inst::*;
use crate::isa::riscv64::lower::isle::generated_code::{
    CaOp, CbOp, CiOp, CiwOp, ClOp, CrOp, CsOp, CssOp, CsznOp, ZcbMemOp,
};
use crate::machinst::{AllocationConsumer, Reg, Writable};
use crate::trace;
use cranelift_control::ControlPlane;
use regalloc2::Allocation;

pub struct EmitInfo {
    shared_flag: settings::Flags,
    isa_flags: super::super::riscv_settings::Flags,
}

impl EmitInfo {
    pub(crate) fn new(
        shared_flag: settings::Flags,
        isa_flags: super::super::riscv_settings::Flags,
    ) -> Self {
        Self {
            shared_flag,
            isa_flags,
        }
    }
}

pub(crate) fn reg_to_gpr_num(m: Reg) -> u32 {
    u32::try_from(m.to_real_reg().unwrap().hw_enc() & 31).unwrap()
}

pub(crate) fn reg_to_compressed_gpr_num(m: Reg) -> u32 {
    let real_reg = m.to_real_reg().unwrap().hw_enc();
    debug_assert!(real_reg >= 8 && real_reg < 16);
    let compressed_reg = real_reg - 8;
    u32::try_from(compressed_reg).unwrap()
}

#[derive(Clone, Debug, PartialEq, Default)]
pub enum EmitVState {
    #[default]
    Unknown,
    Known(VState),
}

/// State carried between emissions of a sequence of instructions.
#[derive(Default, Clone, Debug)]
pub struct EmitState {
    pub(crate) virtual_sp_offset: i64,
    pub(crate) nominal_sp_to_fp: i64,
    /// Safepoint stack map for upcoming instruction, as provided to `pre_safepoint()`.
    stack_map: Option<StackMap>,
    /// Current source-code location corresponding to instruction to be emitted.
    cur_srcloc: RelSourceLoc,
    /// Only used during fuzz-testing. Otherwise, it is a zero-sized struct and
    /// optimized away at compiletime. See [cranelift_control].
    ctrl_plane: ControlPlane,
    /// Vector State
    /// Controls the current state of the vector unit at the emission point.
    vstate: EmitVState,
}

impl EmitState {
    fn take_stack_map(&mut self) -> Option<StackMap> {
        self.stack_map.take()
    }

    fn clear_post_insn(&mut self) {
        self.stack_map = None;
    }

    fn cur_srcloc(&self) -> RelSourceLoc {
        self.cur_srcloc
    }
}

impl MachInstEmitState<Inst> for EmitState {
    fn new(
        abi: &Callee<crate::isa::riscv64::abi::Riscv64MachineDeps>,
        ctrl_plane: ControlPlane,
    ) -> Self {
        EmitState {
            virtual_sp_offset: 0,
            nominal_sp_to_fp: abi.frame_size() as i64,
            stack_map: None,
            cur_srcloc: RelSourceLoc::default(),
            ctrl_plane,
            vstate: EmitVState::Unknown,
        }
    }

    fn pre_safepoint(&mut self, stack_map: StackMap) {
        self.stack_map = Some(stack_map);
    }

    fn pre_sourceloc(&mut self, srcloc: RelSourceLoc) {
        self.cur_srcloc = srcloc;
    }

    fn ctrl_plane_mut(&mut self) -> &mut ControlPlane {
        &mut self.ctrl_plane
    }

    fn take_ctrl_plane(self) -> ControlPlane {
        self.ctrl_plane
    }

    fn on_new_block(&mut self) {
        // Reset the vector state.
        self.vstate = EmitVState::Unknown;
    }
}

impl Inst {
    /// Load int mask.
    /// If ty is int then 0xff in rd.
    pub(crate) fn load_int_mask(rd: Writable<Reg>, ty: Type) -> SmallInstVec<Inst> {
        let mut insts = SmallInstVec::new();
        assert!(ty.is_int() && ty.bits() <= 64);
        match ty {
            I64 => {
                insts.push(Inst::load_imm12(rd, Imm12::from_i16(-1)));
            }
            I32 | I16 => {
                insts.push(Inst::load_imm12(rd, Imm12::from_i16(-1)));
                insts.push(Inst::Extend {
                    rd: rd,
                    rn: rd.to_reg(),
                    signed: false,
                    from_bits: ty.bits() as u8,
                    to_bits: 64,
                });
            }
            I8 => {
                insts.push(Inst::load_imm12(rd, Imm12::from_i16(255)));
            }
            _ => unreachable!("ty:{:?}", ty),
        }
        insts
    }
    ///  inverse all bit
    pub(crate) fn construct_bit_not(rd: Writable<Reg>, rs: Reg) -> Inst {
        Inst::AluRRImm12 {
            alu_op: AluOPRRI::Xori,
            rd,
            rs,
            imm12: Imm12::from_i16(-1),
        }
    }

    // emit a float is not a nan.
    pub(crate) fn emit_not_nan(rd: Writable<Reg>, rs: Reg, ty: Type) -> Inst {
        Inst::FpuRRR {
            alu_op: if ty == F32 {
                FpuOPRRR::FeqS
            } else {
                FpuOPRRR::FeqD
            },
            frm: None,
            rd: rd,
            rs1: rs,
            rs2: rs,
        }
    }

    pub(crate) fn emit_fabs(rd: Writable<Reg>, rs: Reg, ty: Type) -> Inst {
        Inst::FpuRRR {
            alu_op: if ty == F32 {
                FpuOPRRR::FsgnjxS
            } else {
                FpuOPRRR::FsgnjxD
            },
            frm: None,
            rd: rd,
            rs1: rs,
            rs2: rs,
        }
    }
    /// If a float is zero.
    pub(crate) fn emit_if_float_not_zero(
        tmp: Writable<Reg>,
        rs: Reg,
        ty: Type,
        taken: CondBrTarget,
        not_taken: CondBrTarget,
    ) -> SmallInstVec<Inst> {
        let mut insts = SmallInstVec::new();
        let class_op = if ty == F32 {
            FpuOPRR::FclassS
        } else {
            FpuOPRR::FclassD
        };
        insts.push(Inst::FpuRR {
            alu_op: class_op,
            frm: None,
            rd: tmp,
            rs: rs,
        });
        insts.push(Inst::AluRRImm12 {
            alu_op: AluOPRRI::Andi,
            rd: tmp,
            rs: tmp.to_reg(),
            imm12: Imm12::from_i16(FClassResult::is_zero_bits() as i16),
        });
        insts.push(Inst::CondBr {
            taken,
            not_taken,
            kind: IntegerCompare {
                kind: IntCC::Equal,
                rs1: tmp.to_reg(),
                rs2: zero_reg(),
            },
        });
        insts
    }

    pub(crate) fn lower_br_icmp(
        cc: IntCC,
        a: ValueRegs<Reg>,
        b: ValueRegs<Reg>,
        taken: CondBrTarget,
        not_taken: CondBrTarget,
        ty: Type,
    ) -> SmallInstVec<Inst> {
        let mut insts = SmallInstVec::new();
        if ty.bits() <= 64 {
            let rs1 = a.only_reg().unwrap();
            let rs2 = b.only_reg().unwrap();
            let inst = Inst::CondBr {
                taken,
                not_taken,
                kind: IntegerCompare { kind: cc, rs1, rs2 },
            };
            insts.push(inst);
            return insts;
        }
        // compare i128
        let low = |cc: IntCC| -> IntegerCompare {
            IntegerCompare {
                rs1: a.regs()[0],
                rs2: b.regs()[0],
                kind: cc,
            }
        };
        let high = |cc: IntCC| -> IntegerCompare {
            IntegerCompare {
                rs1: a.regs()[1],
                rs2: b.regs()[1],
                kind: cc,
            }
        };
        match cc {
            IntCC::Equal => {
                // if high part not equal,
                // then we can go to not_taken otherwise fallthrough.
                insts.push(Inst::CondBr {
                    taken: not_taken,
                    not_taken: CondBrTarget::Fallthrough,
                    kind: high(IntCC::NotEqual),
                });
                // the rest part.
                insts.push(Inst::CondBr {
                    taken,
                    not_taken,
                    kind: low(IntCC::Equal),
                });
            }

            IntCC::NotEqual => {
                // if the high part not equal ,
                // we know the whole must be not equal,
                // we can goto the taken part , otherwise fallthrought.
                insts.push(Inst::CondBr {
                    taken,
                    not_taken: CondBrTarget::Fallthrough, //  no branch
                    kind: high(IntCC::NotEqual),
                });

                insts.push(Inst::CondBr {
                    taken,
                    not_taken,
                    kind: low(IntCC::NotEqual),
                });
            }
            IntCC::SignedGreaterThanOrEqual
            | IntCC::SignedLessThanOrEqual
            | IntCC::UnsignedGreaterThanOrEqual
            | IntCC::UnsignedLessThanOrEqual
            | IntCC::SignedGreaterThan
            | IntCC::SignedLessThan
            | IntCC::UnsignedLessThan
            | IntCC::UnsignedGreaterThan => {
                //
                insts.push(Inst::CondBr {
                    taken,
                    not_taken: CondBrTarget::Fallthrough,
                    kind: high(cc.without_equal()),
                });
                //
                insts.push(Inst::CondBr {
                    taken: not_taken,
                    not_taken: CondBrTarget::Fallthrough,
                    kind: high(IntCC::NotEqual),
                });
                insts.push(Inst::CondBr {
                    taken,
                    not_taken,
                    kind: low(cc.unsigned()),
                });
            }
        }
        insts
    }

    /// Returns Some(VState) if this insturction is expecting a specific vector state
    /// before emission.
    fn expected_vstate(&self) -> Option<&VState> {
        match self {
            Inst::Nop0
            | Inst::Nop4
            | Inst::BrTable { .. }
            | Inst::Auipc { .. }
            | Inst::Lui { .. }
            | Inst::LoadInlineConst { .. }
            | Inst::AluRRR { .. }
            | Inst::FpuRRR { .. }
            | Inst::AluRRImm12 { .. }
            | Inst::CsrReg { .. }
            | Inst::CsrImm { .. }
            | Inst::Load { .. }
            | Inst::Store { .. }
            | Inst::Args { .. }
            | Inst::Rets { .. }
            | Inst::Ret { .. }
            | Inst::Extend { .. }
            | Inst::AdjustSp { .. }
            | Inst::Call { .. }
            | Inst::CallInd { .. }
            | Inst::ReturnCall { .. }
            | Inst::ReturnCallInd { .. }
            | Inst::Jal { .. }
            | Inst::CondBr { .. }
            | Inst::LoadExtName { .. }
            | Inst::ElfTlsGetAddr { .. }
            | Inst::LoadAddr { .. }
            | Inst::VirtualSPOffsetAdj { .. }
            | Inst::Mov { .. }
            | Inst::MovFromPReg { .. }
            | Inst::Fence { .. }
            | Inst::EBreak
            | Inst::Udf { .. }
            | Inst::FpuRR { .. }
            | Inst::FpuRRRR { .. }
            | Inst::Jalr { .. }
            | Inst::Atomic { .. }
            | Inst::Select { .. }
            | Inst::AtomicCas { .. }
            | Inst::Icmp { .. }
            | Inst::FcvtToInt { .. }
            | Inst::RawData { .. }
            | Inst::AtomicStore { .. }
            | Inst::AtomicLoad { .. }
            | Inst::AtomicRmwLoop { .. }
            | Inst::TrapIf { .. }
            | Inst::Unwind { .. }
            | Inst::DummyUse { .. }
            | Inst::FloatRound { .. }
            | Inst::FloatSelect { .. }
            | Inst::Popcnt { .. }
            | Inst::Rev8 { .. }
            | Inst::Cltz { .. }
            | Inst::Brev8 { .. }
            | Inst::StackProbeLoop { .. } => None,

            // VecSetState does not expect any vstate, rather it updates it.
            Inst::VecSetState { .. } => None,

            // `vmv` instructions copy a set of registers and ignore vstate.
            Inst::VecAluRRImm5 { op: VecAluOpRRImm5::VmvrV, .. } => None,

            Inst::VecAluRR { vstate, .. } |
            Inst::VecAluRRR { vstate, .. } |
            Inst::VecAluRRRR { vstate, .. } |
            Inst::VecAluRImm5 { vstate, .. } |
            Inst::VecAluRRImm5 { vstate, .. } |
            Inst::VecAluRRRImm5 { vstate, .. } |
            // TODO: Unit-stride loads and stores only need the AVL to be correct, not
            // the full vtype. A future optimization could be to decouple these two when
            // updating vstate. This would allow us to avoid emitting a VecSetState in
            // some cases.
            Inst::VecLoad { vstate, .. }
            | Inst::VecStore { vstate, .. } => Some(vstate),
        }
    }
}

impl MachInstEmit for Inst {
    type State = EmitState;
    type Info = EmitInfo;

    fn emit(
        &self,
        allocs: &[Allocation],
        sink: &mut MachBuffer<Inst>,
        emit_info: &Self::Info,
        state: &mut EmitState,
    ) {
        // Transform this into a instruction with all the physical regs
        let mut allocs = AllocationConsumer::new(allocs);
        let inst = self.clone().allocate(&mut allocs);

        // Check if we need to update the vector state before emitting this instruction
        if let Some(expected) = inst.expected_vstate() {
            if state.vstate != EmitVState::Known(expected.clone()) {
                // Update the vector state.
                Inst::VecSetState {
                    rd: writable_zero_reg(),
                    vstate: expected.clone(),
                }
                .emit(&[], sink, emit_info, state);
            }
        }

        // N.B.: we *must* not exceed the "worst-case size" used to compute
        // where to insert islands, except when islands are explicitly triggered
        // (with an `EmitIsland`). We check this in debug builds. This is `mut`
        // to allow disabling the check for `JTSequence`, which is always
        // emitted following an `EmitIsland`.
        let mut start_off = sink.cur_offset();

        // First try to emit this as a compressed instruction
        let res = inst.try_emit_compressed(sink, emit_info, state, &mut start_off);
        if res.is_none() {
            // If we can't lets emit it as a normal instruction
            inst.emit_uncompressed(sink, emit_info, state, &mut start_off);
        }

        let end_off = sink.cur_offset();
        assert!(
            (end_off - start_off) <= Inst::worst_case_size(),
            "Inst:{:?} length:{} worst_case_size:{}",
            self,
            end_off - start_off,
            Inst::worst_case_size()
        );
    }

    fn pretty_print_inst(&self, allocs: &[Allocation], state: &mut Self::State) -> String {
        let mut allocs = AllocationConsumer::new(allocs);
        self.print_with_state(state, &mut allocs)
    }
}

impl Inst {
    /// Tries to emit an instruction as compressed, if we can't return false.
    fn try_emit_compressed(
        &self,
        sink: &mut MachBuffer<Inst>,
        emit_info: &EmitInfo,
        state: &mut EmitState,
        start_off: &mut u32,
    ) -> Option<()> {
        let has_m = emit_info.isa_flags.has_m();
        let has_zba = emit_info.isa_flags.has_zba();
        let has_zbb = emit_info.isa_flags.has_zbb();
        let has_zca = emit_info.isa_flags.has_zca();
        let has_zcb = emit_info.isa_flags.has_zcb();
        let has_zcd = emit_info.isa_flags.has_zcd();

        // Currently all compressed extensions (Zcb, Zcd, Zcmp, Zcmt, etc..) require Zca
        // to be enabled, so check it early.
        if !has_zca {
            return None;
        }

        fn reg_is_compressible(r: Reg) -> bool {
            r.to_real_reg()
                .map(|r| r.hw_enc() >= 8 && r.hw_enc() < 16)
                .unwrap_or(false)
        }

        match *self {
            // C.ADD
            Inst::AluRRR {
                alu_op: AluOPRRR::Add,
                rd,
                rs1,
                rs2,
            } if rd.to_reg() == rs1 && rs1 != zero_reg() && rs2 != zero_reg() => {
                sink.put2(encode_cr_type(CrOp::CAdd, rd, rs2));
            }

            // C.MV
            Inst::AluRRImm12 {
                alu_op: AluOPRRI::Addi | AluOPRRI::Ori,
                rd,
                rs,
                imm12,
            } if rd.to_reg() != rs
                && rd.to_reg() != zero_reg()
                && rs != zero_reg()
                && imm12.as_i16() == 0 =>
            {
                sink.put2(encode_cr_type(CrOp::CMv, rd, rs));
            }

            // CA Ops
            Inst::AluRRR {
                alu_op:
                    alu_op @ (AluOPRRR::And
                    | AluOPRRR::Or
                    | AluOPRRR::Xor
                    | AluOPRRR::Sub
                    | AluOPRRR::Addw
                    | AluOPRRR::Subw
                    | AluOPRRR::Mul),
                rd,
                rs1,
                rs2,
            } if rd.to_reg() == rs1 && reg_is_compressible(rs1) && reg_is_compressible(rs2) => {
                let op = match alu_op {
                    AluOPRRR::And => CaOp::CAnd,
                    AluOPRRR::Or => CaOp::COr,
                    AluOPRRR::Xor => CaOp::CXor,
                    AluOPRRR::Sub => CaOp::CSub,
                    AluOPRRR::Addw => CaOp::CAddw,
                    AluOPRRR::Subw => CaOp::CSubw,
                    AluOPRRR::Mul if has_zcb && has_m => CaOp::CMul,
                    _ => return None,
                };

                sink.put2(encode_ca_type(op, rd, rs2));
            }

            // c.j
            //
            // We don't have a separate JAL as that is only availabile in RV32C
            Inst::Jal { label } => {
                sink.use_label_at_offset(*start_off, label, LabelUse::RVCJump);
                sink.add_uncond_branch(*start_off, *start_off + 2, label);
                sink.put2(encode_cj_type(CjOp::CJ, Imm12::ZERO));
            }

            // c.jr
            Inst::Jalr { rd, base, offset }
                if rd.to_reg() == zero_reg() && base != zero_reg() && offset.as_i16() == 0 =>
            {
                sink.put2(encode_cr2_type(CrOp::CJr, base));
            }

            // c.jalr
            Inst::Jalr { rd, base, offset }
                if rd.to_reg() == link_reg() && base != zero_reg() && offset.as_i16() == 0 =>
            {
                sink.put2(encode_cr2_type(CrOp::CJalr, base));
            }

            // c.ebreak
            Inst::EBreak => {
                sink.put2(encode_cr_type(
                    CrOp::CEbreak,
                    writable_zero_reg(),
                    zero_reg(),
                ));
            }

            // c.unimp
            Inst::Udf { trap_code } => {
                sink.add_trap(trap_code);
                if let Some(s) = state.take_stack_map() {
                    sink.add_stack_map(StackMapExtent::UpcomingBytes(2), s);
                }
                sink.put2(0x0000);
            }

            // c.addi16sp
            //
            // c.addi16sp shares the opcode with c.lui, but has a destination field of x2.
            // c.addi16sp adds the non-zero sign-extended 6-bit immediate to the value in the stack pointer (sp=x2),
            // where the immediate is scaled to represent multiples of 16 in the range (-512,496). c.addi16sp is used
            // to adjust the stack pointer in procedure prologues and epilogues. It expands into addi x2, x2, nzimm. c.addi16sp
            // is only valid when nzimm≠0; the code point with nzimm=0 is reserved.
            Inst::AluRRImm12 {
                alu_op: AluOPRRI::Addi,
                rd,
                rs,
                imm12,
            } if rd.to_reg() == rs
                && rs == stack_reg()
                && imm12.as_i16() != 0
                && (imm12.as_i16() % 16) == 0
                && Imm6::maybe_from_i16(imm12.as_i16() / 16).is_some() =>
            {
                let imm6 = Imm6::maybe_from_i16(imm12.as_i16() / 16).unwrap();
                sink.put2(encode_c_addi16sp(imm6));
            }

            // c.addi4spn
            //
            // c.addi4spn is a CIW-format instruction that adds a zero-extended non-zero
            // immediate, scaled by 4, to the stack pointer, x2, and writes the result to
            // rd. This instruction is used to generate pointers to stack-allocated variables
            // and expands to addi rd, x2, nzuimm. c.addi4spn is only valid when nzuimm≠0;
            // the code points with nzuimm=0 are reserved.
            Inst::AluRRImm12 {
                alu_op: AluOPRRI::Addi,
                rd,
                rs,
                imm12,
            } if reg_is_compressible(rd.to_reg())
                && rs == stack_reg()
                && imm12.as_i16() != 0
                && (imm12.as_i16() % 4) == 0
                && u8::try_from(imm12.as_i16() / 4).is_ok() =>
            {
                let imm = u8::try_from(imm12.as_i16() / 4).unwrap();
                sink.put2(encode_ciw_type(CiwOp::CAddi4spn, rd, imm));
            }

            // c.li
            Inst::AluRRImm12 {
                alu_op: AluOPRRI::Addi,
                rd,
                rs,
                imm12,
            } if rd.to_reg() != zero_reg() && rs == zero_reg() && imm12.as_i16() != 0 => {
                let imm6 = Imm6::maybe_from_imm12(imm12)?;
                sink.put2(encode_ci_type(CiOp::CLi, rd, imm6));
            }

            // c.addi
            Inst::AluRRImm12 {
                alu_op: AluOPRRI::Addi,
                rd,
                rs,
                imm12,
            } if rd.to_reg() == rs && rs != zero_reg() && imm12.as_i16() != 0 => {
                let imm6 = Imm6::maybe_from_imm12(imm12)?;
                sink.put2(encode_ci_type(CiOp::CAddi, rd, imm6));
            }

            // c.addiw
            Inst::AluRRImm12 {
                alu_op: AluOPRRI::Addiw,
                rd,
                rs,
                imm12,
            } if rd.to_reg() == rs && rs != zero_reg() => {
                let imm6 = Imm6::maybe_from_imm12(imm12)?;
                sink.put2(encode_ci_type(CiOp::CAddiw, rd, imm6));
            }

            // c.lui
            //
            // c.lui loads the non-zero 6-bit immediate field into bits 17–12
            // of the destination register, clears the bottom 12 bits, and
            // sign-extends bit 17 into all higher bits of the destination.
            Inst::Lui { rd, imm: imm20 }
                if rd.to_reg() != zero_reg()
                    && rd.to_reg() != stack_reg()
                    && imm20.as_i32() != 0 =>
            {
                // Check that the top bits are sign extended
                let imm = imm20.as_i32() << 14 >> 14;
                if imm != imm20.as_i32() {
                    return None;
                }
                let imm6 = Imm6::maybe_from_i32(imm)?;
                sink.put2(encode_ci_type(CiOp::CLui, rd, imm6));
            }

            // c.slli
            Inst::AluRRImm12 {
                alu_op: AluOPRRI::Slli,
                rd,
                rs,
                imm12,
            } if rd.to_reg() == rs && rs != zero_reg() && imm12.as_i16() != 0 => {
                // The shift amount is unsigned, but we encode it as signed.
                let shift = imm12.as_i16() & 0x3f;
                let imm6 = Imm6::maybe_from_i16(shift << 10 >> 10).unwrap();
                sink.put2(encode_ci_type(CiOp::CSlli, rd, imm6));
            }

            // c.srli / c.srai
            Inst::AluRRImm12 {
                alu_op: op @ (AluOPRRI::Srli | AluOPRRI::Srai),
                rd,
                rs,
                imm12,
            } if rd.to_reg() == rs && reg_is_compressible(rs) && imm12.as_i16() != 0 => {
                let op = match op {
                    AluOPRRI::Srli => CbOp::CSrli,
                    AluOPRRI::Srai => CbOp::CSrai,
                    _ => unreachable!(),
                };

                // The shift amount is unsigned, but we encode it as signed.
                let shift = imm12.as_i16() & 0x3f;
                let imm6 = Imm6::maybe_from_i16(shift << 10 >> 10).unwrap();
                sink.put2(encode_cb_type(op, rd, imm6));
            }

            // c.zextb
            //
            // This is an alias for `andi rd, rd, 0xff`
            Inst::AluRRImm12 {
                alu_op: AluOPRRI::Andi,
                rd,
                rs,
                imm12,
            } if has_zcb
                && rd.to_reg() == rs
                && reg_is_compressible(rs)
                && imm12.as_i16() == 0xff =>
            {
                sink.put2(encode_cszn_type(CsznOp::CZextb, rd));
            }

            // c.andi
            Inst::AluRRImm12 {
                alu_op: AluOPRRI::Andi,
                rd,
                rs,
                imm12,
            } if rd.to_reg() == rs && reg_is_compressible(rs) => {
                let imm6 = Imm6::maybe_from_imm12(imm12)?;
                sink.put2(encode_cb_type(CbOp::CAndi, rd, imm6));
            }

            // Stack Based Loads
            Inst::Load {
                rd,
                op: op @ (LoadOP::Lw | LoadOP::Ld | LoadOP::Fld),
                from,
                flags,
            } if from.get_base_register() == Some(stack_reg())
                && (from.get_offset_with_state(state) % op.size()) == 0 =>
            {
                // We encode the offset in multiples of the load size.
                let offset = from.get_offset_with_state(state);
                let imm6 = u8::try_from(offset / op.size())
                    .ok()
                    .and_then(Uimm6::maybe_from_u8)?;

                // Some additional constraints on these instructions.
                //
                // Integer loads are not allowed to target x0, but floating point loads
                // are, since f0 is not a special register.
                //
                // Floating point loads are not included in the base Zca extension
                // but in a separate Zcd extension. Both of these are part of the C Extension.
                let rd_is_zero = rd.to_reg() == zero_reg();
                let op = match op {
                    LoadOP::Lw if !rd_is_zero => CiOp::CLwsp,
                    LoadOP::Ld if !rd_is_zero => CiOp::CLdsp,
                    LoadOP::Fld if has_zcd => CiOp::CFldsp,
                    _ => return None,
                };

                let srcloc = state.cur_srcloc();
                if !srcloc.is_default() && !flags.notrap() {
                    // Register the offset at which the actual load instruction starts.
                    sink.add_trap(TrapCode::HeapOutOfBounds);
                }
                sink.put2(encode_ci_sp_load(op, rd, imm6));
            }

            // Regular Loads
            Inst::Load {
                rd,
                op:
                    op
                    @ (LoadOP::Lw | LoadOP::Ld | LoadOP::Fld | LoadOP::Lbu | LoadOP::Lhu | LoadOP::Lh),
                from,
                flags,
            } if reg_is_compressible(rd.to_reg())
                && from
                    .get_base_register()
                    .map(reg_is_compressible)
                    .unwrap_or(false)
                && (from.get_offset_with_state(state) % op.size()) == 0 =>
            {
                let base = from.get_base_register().unwrap();

                // We encode the offset in multiples of the store size.
                let offset = from.get_offset_with_state(state);
                let offset = u8::try_from(offset / op.size()).ok()?;

                // We mix two different formats here.
                //
                // c.lw / c.ld / c.fld instructions are available in the standard Zca
                // extension using the CL format.
                //
                // c.lbu / c.lhu / c.lh are only available in the Zcb extension and
                // are also encoded differently. Technically they each have a different
                // format, but they are similar enough that we can group them.
                let is_zcb_load = matches!(op, LoadOP::Lbu | LoadOP::Lhu | LoadOP::Lh);
                let encoded = if is_zcb_load {
                    if !has_zcb {
                        return None;
                    }

                    let op = match op {
                        LoadOP::Lbu => ZcbMemOp::CLbu,
                        LoadOP::Lhu => ZcbMemOp::CLhu,
                        LoadOP::Lh => ZcbMemOp::CLh,
                        _ => unreachable!(),
                    };

                    // Byte stores & loads have 2 bits of immediate offset. Halfword stores
                    // and loads only have 1 bit.
                    let imm2 = Uimm2::maybe_from_u8(offset)?;
                    if (offset & !((1 << op.imm_bits()) - 1)) != 0 {
                        return None;
                    }

                    encode_zcbmem_load(op, rd, base, imm2)
                } else {
                    // Floating point loads are not included in the base Zca extension
                    // but in a separate Zcd extension. Both of these are part of the C Extension.
                    let op = match op {
                        LoadOP::Lw => ClOp::CLw,
                        LoadOP::Ld => ClOp::CLd,
                        LoadOP::Fld if has_zcd => ClOp::CFld,
                        _ => return None,
                    };
                    let imm5 = Uimm5::maybe_from_u8(offset)?;

                    encode_cl_type(op, rd, base, imm5)
                };

                let srcloc = state.cur_srcloc();
                if !srcloc.is_default() && !flags.notrap() {
                    // Register the offset at which the actual load instruction starts.
                    sink.add_trap(TrapCode::HeapOutOfBounds);
                }
                sink.put2(encoded);
            }

            // Stack Based Stores
            Inst::Store {
                src,
                op: op @ (StoreOP::Sw | StoreOP::Sd | StoreOP::Fsd),
                to,
                flags,
            } if to.get_base_register() == Some(stack_reg())
                && (to.get_offset_with_state(state) % op.size()) == 0 =>
            {
                // We encode the offset in multiples of the store size.
                let offset = to.get_offset_with_state(state);
                let imm6 = u8::try_from(offset / op.size())
                    .ok()
                    .and_then(Uimm6::maybe_from_u8)?;

                // Floating point stores are not included in the base Zca extension
                // but in a separate Zcd extension. Both of these are part of the C Extension.
                let op = match op {
                    StoreOP::Sw => CssOp::CSwsp,
                    StoreOP::Sd => CssOp::CSdsp,
                    StoreOP::Fsd if has_zcd => CssOp::CFsdsp,
                    _ => return None,
                };

                let srcloc = state.cur_srcloc();
                if !srcloc.is_default() && !flags.notrap() {
                    // Register the offset at which the actual load instruction starts.
                    sink.add_trap(TrapCode::HeapOutOfBounds);
                }
                sink.put2(encode_css_type(op, src, imm6));
            }

            // Regular Stores
            Inst::Store {
                src,
                op: op @ (StoreOP::Sw | StoreOP::Sd | StoreOP::Fsd | StoreOP::Sh | StoreOP::Sb),
                to,
                flags,
            } if reg_is_compressible(src)
                && to
                    .get_base_register()
                    .map(reg_is_compressible)
                    .unwrap_or(false)
                && (to.get_offset_with_state(state) % op.size()) == 0 =>
            {
                let base = to.get_base_register().unwrap();

                // We encode the offset in multiples of the store size.
                let offset = to.get_offset_with_state(state);
                let offset = u8::try_from(offset / op.size()).ok()?;

                // We mix two different formats here.
                //
                // c.sw / c.sd / c.fsd instructions are available in the standard Zca
                // extension using the CL format.
                //
                // c.sb / c.sh are only available in the Zcb extension and are also
                // encoded differently.
                let is_zcb_store = matches!(op, StoreOP::Sh | StoreOP::Sb);
                let encoded = if is_zcb_store {
                    if !has_zcb {
                        return None;
                    }

                    let op = match op {
                        StoreOP::Sh => ZcbMemOp::CSh,
                        StoreOP::Sb => ZcbMemOp::CSb,
                        _ => unreachable!(),
                    };

                    // Byte stores & loads have 2 bits of immediate offset. Halfword stores
                    // and loads only have 1 bit.
                    let imm2 = Uimm2::maybe_from_u8(offset)?;
                    if (offset & !((1 << op.imm_bits()) - 1)) != 0 {
                        return None;
                    }

                    encode_zcbmem_store(op, src, base, imm2)
                } else {
                    // Floating point stores are not included in the base Zca extension
                    // but in a separate Zcd extension. Both of these are part of the C Extension.
                    let op = match op {
                        StoreOP::Sw => CsOp::CSw,
                        StoreOP::Sd => CsOp::CSd,
                        StoreOP::Fsd if has_zcd => CsOp::CFsd,
                        _ => return None,
                    };
                    let imm5 = Uimm5::maybe_from_u8(offset)?;

                    encode_cs_type(op, src, base, imm5)
                };

                let srcloc = state.cur_srcloc();
                if !srcloc.is_default() && !flags.notrap() {
                    // Register the offset at which the actual load instruction starts.
                    sink.add_trap(TrapCode::HeapOutOfBounds);
                }
                sink.put2(encoded);
            }

            // c.not
            //
            // This is an alias for `xori rd, rd, -1`
            Inst::AluRRImm12 {
                alu_op: AluOPRRI::Xori,
                rd,
                rs,
                imm12,
            } if has_zcb
                && rd.to_reg() == rs
                && reg_is_compressible(rs)
                && imm12.as_i16() == -1 =>
            {
                sink.put2(encode_cszn_type(CsznOp::CNot, rd));
            }

            // c.sext.b / c.sext.h / c.zext.h
            //
            // These are all the extend instructions present in `Zcb`, they
            // also require `Zbb` since they aren't available in the base ISA.
            Inst::AluRRImm12 {
                alu_op: alu_op @ (AluOPRRI::Sextb | AluOPRRI::Sexth | AluOPRRI::Zexth),
                rd,
                rs,
                imm12,
            } if has_zcb
                && has_zbb
                && rd.to_reg() == rs
                && reg_is_compressible(rs)
                && imm12.as_i16() == 0 =>
            {
                let op = match alu_op {
                    AluOPRRI::Sextb => CsznOp::CSextb,
                    AluOPRRI::Sexth => CsznOp::CSexth,
                    AluOPRRI::Zexth => CsznOp::CZexth,
                    _ => unreachable!(),
                };
                sink.put2(encode_cszn_type(op, rd));
            }

            // c.zext.w
            //
            // This is an alias for `add.uw rd, rd, zero`
            Inst::AluRRR {
                alu_op: AluOPRRR::Adduw,
                rd,
                rs1,
                rs2,
            } if has_zcb
                && has_zba
                && rd.to_reg() == rs1
                && reg_is_compressible(rs1)
                && rs2 == zero_reg() =>
            {
                sink.put2(encode_cszn_type(CsznOp::CZextw, rd));
            }

            _ => return None,
        }

        return Some(());
    }

    fn emit_uncompressed(
        &self,
        sink: &mut MachBuffer<Inst>,
        emit_info: &EmitInfo,
        state: &mut EmitState,
        start_off: &mut u32,
    ) {
        match self {
            &Inst::Nop0 => {
                // do nothing
            }
            // Addi x0, x0, 0
            &Inst::Nop4 => {
                let x = Inst::AluRRImm12 {
                    alu_op: AluOPRRI::Addi,
                    rd: Writable::from_reg(zero_reg()),
                    rs: zero_reg(),
                    imm12: Imm12::ZERO,
                };
                x.emit(&[], sink, emit_info, state)
            }
            &Inst::RawData { ref data } => {
                // Right now we only put a u32 or u64 in this instruction.
                // It is not very long, no need to check if need `emit_island`.
                // If data is very long , this is a bug because RawData is typecial
                // use to load some data and rely on some positon in the code stream.
                // and we may exceed `Inst::worst_case_size`.
                // for more information see https://github.com/bytecodealliance/wasmtime/pull/5612.
                sink.put_data(&data[..]);
            }
            &Inst::Lui { rd, ref imm } => {
                let x: u32 = 0b0110111 | reg_to_gpr_num(rd.to_reg()) << 7 | (imm.bits() << 12);
                sink.put4(x);
            }
            &Inst::LoadInlineConst { rd, ty, imm } => {
                let data = &imm.to_le_bytes()[..ty.bytes() as usize];

                let label_data: MachLabel = sink.get_label();
                let label_end: MachLabel = sink.get_label();

                // Load into rd
                Inst::Load {
                    rd,
                    op: LoadOP::from_type(ty),
                    flags: MemFlags::new(),
                    from: AMode::Label(label_data),
                }
                .emit(&[], sink, emit_info, state);

                // Jump over the inline pool
                Inst::gen_jump(label_end).emit(&[], sink, emit_info, state);

                // Emit the inline data
                sink.bind_label(label_data, &mut state.ctrl_plane);
                Inst::RawData { data: data.into() }.emit(&[], sink, emit_info, state);

                sink.bind_label(label_end, &mut state.ctrl_plane);
            }
            &Inst::FpuRR {
                frm,
                alu_op,
                rd,
                rs,
            } => {
                let x = alu_op.op_code()
                    | reg_to_gpr_num(rd.to_reg()) << 7
                    | alu_op.funct3(frm) << 12
                    | reg_to_gpr_num(rs) << 15
                    | alu_op.rs2_funct5() << 20
                    | alu_op.funct7() << 25;
                let srcloc = state.cur_srcloc();
                if !srcloc.is_default() && alu_op.is_convert_to_int() {
                    sink.add_trap(TrapCode::BadConversionToInteger);
                }
                sink.put4(x);
            }
            &Inst::FpuRRRR {
                alu_op,
                rd,
                rs1,
                rs2,
                rs3,
                frm,
            } => {
                let x = alu_op.op_code()
                    | reg_to_gpr_num(rd.to_reg()) << 7
                    | alu_op.funct3(frm) << 12
                    | reg_to_gpr_num(rs1) << 15
                    | reg_to_gpr_num(rs2) << 20
                    | alu_op.funct2() << 25
                    | reg_to_gpr_num(rs3) << 27;

                sink.put4(x);
            }
            &Inst::FpuRRR {
                alu_op,
                frm,
                rd,
                rs1,
                rs2,
            } => {
                let x: u32 = alu_op.op_code()
                    | reg_to_gpr_num(rd.to_reg()) << 7
                    | (alu_op.funct3(frm)) << 12
                    | reg_to_gpr_num(rs1) << 15
                    | reg_to_gpr_num(rs2) << 20
                    | alu_op.funct7() << 25;
                sink.put4(x);
            }
            &Inst::Unwind { ref inst } => {
                sink.add_unwind(inst.clone());
            }
            &Inst::DummyUse { .. } => {
                // This has already been handled by Inst::allocate.
            }
            &Inst::AluRRR {
                alu_op,
                rd,
                rs1,
                rs2,
            } => {
                let (rs1, rs2) = if alu_op.reverse_rs() {
                    (rs2, rs1)
                } else {
                    (rs1, rs2)
                };

                sink.put4(encode_r_type(
                    alu_op.op_code(),
                    rd,
                    alu_op.funct3(),
                    rs1,
                    rs2,
                    alu_op.funct7(),
                ));
            }
            &Inst::AluRRImm12 {
                alu_op,
                rd,
                rs,
                imm12,
            } => {
                let x = alu_op.op_code()
                    | reg_to_gpr_num(rd.to_reg()) << 7
                    | alu_op.funct3() << 12
                    | reg_to_gpr_num(rs) << 15
                    | alu_op.imm12(imm12) << 20;
                sink.put4(x);
            }
            &Inst::CsrReg { op, rd, rs, csr } => {
                sink.put4(encode_csr_reg(op, rd, rs, csr));
            }
            &Inst::CsrImm { op, rd, csr, imm } => {
                sink.put4(encode_csr_imm(op, rd, csr, imm));
            }
            &Inst::Load {
                rd,
                op,
                from,
                flags,
            } => {
                let base = from.get_base_register();
                let offset = from.get_offset_with_state(state);
                let offset_imm12 = Imm12::maybe_from_i64(offset);
                let label = from.get_label_with_sink(sink);

                let (addr, imm12) = match (base, offset_imm12, label) {
                    // When loading from a Reg+Offset, if the offset fits into an imm12 we can directly encode it.
                    (Some(base), Some(imm12), None) => (base, imm12),

                    // Otherwise, if the offset does not fit into a imm12, we need to materialize it into a
                    // register and load from that.
                    (Some(_), None, None) => {
                        let tmp = writable_spilltmp_reg();
                        Inst::LoadAddr { rd: tmp, mem: from }.emit(&[], sink, emit_info, state);
                        (tmp.to_reg(), Imm12::ZERO)
                    }

                    // If the AMode contains a label we can emit an internal relocation that gets
                    // resolved with the correct address later.
                    (None, Some(imm), Some(label)) => {
                        debug_assert_eq!(imm.as_i16(), 0);

                        // Get the current PC.
                        sink.use_label_at_offset(sink.cur_offset(), label, LabelUse::PCRelHi20);
                        Inst::Auipc {
                            rd,
                            imm: Imm20::ZERO,
                        }
                        .emit_uncompressed(sink, emit_info, state, start_off);

                        // Emit a relocation for the load. This patches the offset into the instruction.
                        sink.use_label_at_offset(sink.cur_offset(), label, LabelUse::PCRelLo12I);

                        // Imm12 here is meaningless since it's going to get replaced.
                        (rd.to_reg(), Imm12::ZERO)
                    }

                    // These cases are impossible with the current AModes that we have. We either
                    // always have a register, or always have a label. Never both, and never neither.
                    (None, None, None)
                    | (None, Some(_), None)
                    | (Some(_), None, Some(_))
                    | (Some(_), Some(_), Some(_))
                    | (None, None, Some(_)) => {
                        unreachable!("Invalid load address")
                    }
                };

                let srcloc = state.cur_srcloc();
                if !srcloc.is_default() && !flags.notrap() {
                    // Register the offset at which the actual load instruction starts.
                    sink.add_trap(TrapCode::HeapOutOfBounds);
                }

                sink.put4(encode_i_type(op.op_code(), rd, op.funct3(), addr, imm12));
            }
            &Inst::Store { op, src, flags, to } => {
                let base = to.get_base_register();
                let offset = to.get_offset_with_state(state);
                let offset_imm12 = Imm12::maybe_from_i64(offset);

                let (addr, imm12) = match (base, offset_imm12) {
                    // If the offset fits into an imm12 we can directly encode it.
                    (Some(base), Some(imm12)) => (base, imm12),
                    // Otherwise load the address it into a reg and load from it.
                    _ => {
                        let tmp = writable_spilltmp_reg();
                        Inst::LoadAddr { rd: tmp, mem: to }.emit(&[], sink, emit_info, state);
                        (tmp.to_reg(), Imm12::ZERO)
                    }
                };

                let srcloc = state.cur_srcloc();
                if !srcloc.is_default() && !flags.notrap() {
                    // Register the offset at which the actual load instruction starts.
                    sink.add_trap(TrapCode::HeapOutOfBounds);
                }

                sink.put4(encode_s_type(op.op_code(), op.funct3(), addr, src, imm12));
            }
            &Inst::Args { .. } | &Inst::Rets { .. } => {
                // Nothing: this is a pseudoinstruction that serves
                // only to constrain registers at a certain point.
            }
            &Inst::Ret {} => {
                // RISC-V does not have a dedicated ret instruction, instead we emit the equivalent
                // `jalr x0, x1, 0` that jumps to the return address.
                Inst::Jalr {
                    rd: writable_zero_reg(),
                    base: link_reg(),
                    offset: Imm12::ZERO,
                }
                .emit(&[], sink, emit_info, state);
            }

            &Inst::Extend {
                rd,
                rn,
                signed,
                from_bits,
                to_bits: _to_bits,
            } => {
                let mut insts = SmallInstVec::new();
                let shift_bits = (64 - from_bits) as i16;
                let is_u8 = || from_bits == 8 && signed == false;
                if is_u8() {
                    // special for u8.
                    insts.push(Inst::AluRRImm12 {
                        alu_op: AluOPRRI::Andi,
                        rd,
                        rs: rn,
                        imm12: Imm12::from_i16(255),
                    });
                } else {
                    insts.push(Inst::AluRRImm12 {
                        alu_op: AluOPRRI::Slli,
                        rd,
                        rs: rn,
                        imm12: Imm12::from_i16(shift_bits),
                    });
                    insts.push(Inst::AluRRImm12 {
                        alu_op: if signed {
                            AluOPRRI::Srai
                        } else {
                            AluOPRRI::Srli
                        },
                        rd,
                        rs: rd.to_reg(),
                        imm12: Imm12::from_i16(shift_bits),
                    });
                }
                insts
                    .into_iter()
                    .for_each(|i| i.emit(&[], sink, emit_info, state));
            }
            &Inst::AdjustSp { amount } => {
                if let Some(imm) = Imm12::maybe_from_i64(amount) {
                    Inst::AluRRImm12 {
                        alu_op: AluOPRRI::Addi,
                        rd: writable_stack_reg(),
                        rs: stack_reg(),
                        imm12: imm,
                    }
                    .emit(&[], sink, emit_info, state);
                } else {
                    let tmp = writable_spilltmp_reg();
                    let mut insts = Inst::load_constant_u64(tmp, amount as u64);
                    insts.push(Inst::AluRRR {
                        alu_op: AluOPRRR::Add,
                        rd: writable_stack_reg(),
                        rs1: tmp.to_reg(),
                        rs2: stack_reg(),
                    });
                    insts
                        .into_iter()
                        .for_each(|i| i.emit(&[], sink, emit_info, state));
                }
            }
            &Inst::Call { ref info } => {
                // call
                match info.dest {
                    ExternalName::User { .. } => {
                        if info.opcode.is_call() {
                            sink.add_call_site(info.opcode);
                        }
                        sink.add_reloc(Reloc::RiscvCall, &info.dest, 0);
                        if let Some(s) = state.take_stack_map() {
                            sink.add_stack_map(StackMapExtent::UpcomingBytes(8), s);
                        }
                        Inst::construct_auipc_and_jalr(
                            Some(writable_link_reg()),
                            writable_link_reg(),
                            0,
                        )
                        .into_iter()
                        .for_each(|i| i.emit_uncompressed(sink, emit_info, state, start_off));
                    }
                    ExternalName::LibCall(..)
                    | ExternalName::TestCase { .. }
                    | ExternalName::KnownSymbol(..) => {
                        // use indirect call. it is more simple.
                        // load ext name.
                        Inst::LoadExtName {
                            rd: writable_spilltmp_reg2(),
                            name: Box::new(info.dest.clone()),
                            offset: 0,
                        }
                        .emit(&[], sink, emit_info, state);

                        // call
                        Inst::CallInd {
                            info: Box::new(CallIndInfo {
                                rn: spilltmp_reg2(),
                                // This doesen't really matter but we might as well send
                                // the correct info.
                                uses: info.uses.clone(),
                                defs: info.defs.clone(),
                                clobbers: info.clobbers,
                                opcode: Opcode::CallIndirect,
                                caller_callconv: info.caller_callconv,
                                callee_callconv: info.callee_callconv,
                                // Send this as 0 to avoid updating the pop size twice.
                                callee_pop_size: 0,
                            }),
                        }
                        .emit(&[], sink, emit_info, state);
                    }
                }

                let callee_pop_size = i64::from(info.callee_pop_size);
                state.virtual_sp_offset -= callee_pop_size;
                trace!(
                    "call adjusts virtual sp offset by {callee_pop_size} -> {}",
                    state.virtual_sp_offset
                );
            }
            &Inst::CallInd { ref info } => {
                let start_offset = sink.cur_offset();

                Inst::Jalr {
                    rd: writable_link_reg(),
                    base: info.rn,
                    offset: Imm12::ZERO,
                }
                .emit(&[], sink, emit_info, state);

                if let Some(s) = state.take_stack_map() {
                    sink.add_stack_map(StackMapExtent::StartedAtOffset(start_offset), s);
                }

                if info.opcode.is_call() {
                    sink.add_call_site(info.opcode);
                }

                let callee_pop_size = i64::from(info.callee_pop_size);
                state.virtual_sp_offset -= callee_pop_size;
                trace!(
                    "call adjusts virtual sp offset by {callee_pop_size} -> {}",
                    state.virtual_sp_offset
                );
            }

            &Inst::ReturnCall {
                ref callee,
                ref info,
            } => {
                emit_return_call_common_sequence(
                    sink,
                    emit_info,
                    state,
                    info.new_stack_arg_size,
                    info.old_stack_arg_size,
                );

                sink.add_call_site(ir::Opcode::ReturnCall);
                sink.add_reloc(Reloc::RiscvCall, &**callee, 0);
                Inst::construct_auipc_and_jalr(None, writable_spilltmp_reg(), 0)
                    .into_iter()
                    .for_each(|i| i.emit_uncompressed(sink, emit_info, state, start_off));

                // `emit_return_call_common_sequence` emits an island if
                // necessary, so we can safely disable the worst-case-size check
                // in this case.
                *start_off = sink.cur_offset();
            }

            &Inst::ReturnCallInd { callee, ref info } => {
                emit_return_call_common_sequence(
                    sink,
                    emit_info,
                    state,
                    info.new_stack_arg_size,
                    info.old_stack_arg_size,
                );

                Inst::Jalr {
                    rd: writable_zero_reg(),
                    base: callee,
                    offset: Imm12::ZERO,
                }
                .emit(&[], sink, emit_info, state);

                // `emit_return_call_common_sequence` emits an island if
                // necessary, so we can safely disable the worst-case-size check
                // in this case.
                *start_off = sink.cur_offset();
            }
            &Inst::Jal { label } => {
                sink.use_label_at_offset(*start_off, label, LabelUse::Jal20);
                sink.add_uncond_branch(*start_off, *start_off + 4, label);
                sink.put4(0b1101111);
            }
            &Inst::CondBr {
                taken,
                not_taken,
                kind,
            } => {
                match taken {
                    CondBrTarget::Label(label) => {
                        let code = kind.emit();
                        let code_inverse = kind.inverse().emit().to_le_bytes();
                        sink.use_label_at_offset(*start_off, label, LabelUse::B12);
                        sink.add_cond_branch(*start_off, *start_off + 4, label, &code_inverse);
                        sink.put4(code);
                    }
                    CondBrTarget::Fallthrough => panic!("Cannot fallthrough in taken target"),
                }

                match not_taken {
                    CondBrTarget::Label(label) => {
                        Inst::gen_jump(label).emit(&[], sink, emit_info, state)
                    }
                    CondBrTarget::Fallthrough => {}
                };
            }

            &Inst::Mov { rd, rm, ty } => {
                debug_assert_eq!(rd.to_reg().class(), rm.class());
                if rd.to_reg() == rm {
                    return;
                }

                match rm.class() {
                    RegClass::Int => Inst::AluRRImm12 {
                        alu_op: AluOPRRI::Addi,
                        rd: rd,
                        rs: rm,
                        imm12: Imm12::ZERO,
                    },
                    RegClass::Float => Inst::FpuRRR {
                        alu_op: if ty == F32 {
                            FpuOPRRR::FsgnjS
                        } else {
                            FpuOPRRR::FsgnjD
                        },
                        frm: None,
                        rd: rd,
                        rs1: rm,
                        rs2: rm,
                    },
                    RegClass::Vector => Inst::VecAluRRImm5 {
                        op: VecAluOpRRImm5::VmvrV,
                        vd: rd,
                        vs2: rm,
                        // Imm 0 means copy 1 register.
                        imm: Imm5::maybe_from_i8(0).unwrap(),
                        mask: VecOpMasking::Disabled,
                        // Vstate for this instruction is ignored.
                        vstate: VState::from_type(ty),
                    },
                }
                .emit(&[], sink, emit_info, state);
            }

            &Inst::MovFromPReg { rd, rm } => {
                Inst::gen_move(rd, Reg::from(rm), I64).emit(&[], sink, emit_info, state);
            }

            &Inst::BrTable {
                index,
                tmp1,
                tmp2,
                ref targets,
            } => {
                let ext_index = writable_spilltmp_reg();

                let label_compute_target = sink.get_label();

                // The default target is passed in as the 0th element of `targets`
                // separate it here for clarity.
                let default_target = targets[0];
                let targets = &targets[1..];

                // We are going to potentially emit a large amount of instructions, so ensure that we emit an island
                // now if we need one.
                //
                // The worse case PC calculations are 12 instructions. And each entry in the jump table is 2 instructions.
                // Check if we need to emit a jump table here to support that jump.
                let inst_count = 12 + (targets.len() * 2);
                let distance = (inst_count * Inst::UNCOMPRESSED_INSTRUCTION_SIZE as usize) as u32;
                if sink.island_needed(distance) {
                    let jump_around_label = sink.get_label();
                    Inst::gen_jump(jump_around_label).emit(&[], sink, emit_info, state);
                    sink.emit_island(distance + 4, &mut state.ctrl_plane);
                    sink.bind_label(jump_around_label, &mut state.ctrl_plane);
                }

                // We emit a bounds check on the index, if the index is larger than the number of
                // jump table entries, we jump to the default block.  Otherwise we compute a jump
                // offset by multiplying the index by 8 (the size of each entry) and then jump to
                // that offset. Each jump table entry is a regular auipc+jalr which we emit sequentially.
                //
                // Build the following sequence:
                //
                // extend_index:
                //     zext.w  ext_index, index
                // bounds_check:
                //     li      tmp, n_labels
                //     bltu    ext_index, tmp, compute_target
                // jump_to_default_block:
                //     auipc   pc, 0
                //     jalr    zero, pc, default_block
                // compute_target:
                //     auipc   pc, 0
                //     slli    tmp, ext_index, 3
                //     add     pc, pc, tmp
                //     jalr    zero, pc, 0x10
                // jump_table:
                //     ; This repeats for each entry in the jumptable
                //     auipc   pc, 0
                //     jalr    zero, pc, block_target

                // Extend the index to 64 bits.
                //
                // This prevents us branching on the top 32 bits of the index, which
                // are undefined.
                Inst::Extend {
                    rd: ext_index,
                    rn: index,
                    signed: false,
                    from_bits: 32,
                    to_bits: 64,
                }
                .emit(&[], sink, emit_info, state);

                // Bounds check.
                //
                // Check if the index passed in is larger than the number of jumptable
                // entries that we have. If it is, we fallthrough to a jump into the
                // default block.
                Inst::load_constant_u32(tmp2, targets.len() as u64)
                    .iter()
                    .for_each(|i| i.emit(&[], sink, emit_info, state));
                Inst::CondBr {
                    taken: CondBrTarget::Label(label_compute_target),
                    not_taken: CondBrTarget::Fallthrough,
                    kind: IntegerCompare {
                        kind: IntCC::UnsignedLessThan,
                        rs1: ext_index.to_reg(),
                        rs2: tmp2.to_reg(),
                    },
                }
                .emit(&[], sink, emit_info, state);

                sink.use_label_at_offset(sink.cur_offset(), default_target, LabelUse::PCRel32);
                Inst::construct_auipc_and_jalr(None, tmp2, 0)
                    .iter()
                    .for_each(|i| i.emit_uncompressed(sink, emit_info, state, start_off));

                // Compute the jump table offset.
                // We need to emit a PC relative offset,
                sink.bind_label(label_compute_target, &mut state.ctrl_plane);

                // Get the current PC.
                Inst::Auipc {
                    rd: tmp1,
                    imm: Imm20::ZERO,
                }
                .emit_uncompressed(sink, emit_info, state, start_off);

                // These instructions must be emitted as uncompressed since we
                // are manually computing the offset from the PC.

                // Multiply the index by 8, since that is the size in
                // bytes of each jump table entry
                Inst::AluRRImm12 {
                    alu_op: AluOPRRI::Slli,
                    rd: tmp2,
                    rs: ext_index.to_reg(),
                    imm12: Imm12::from_i16(3),
                }
                .emit_uncompressed(sink, emit_info, state, start_off);

                // Calculate the base of the jump, PC + the offset from above.
                Inst::AluRRR {
                    alu_op: AluOPRRR::Add,
                    rd: tmp1,
                    rs1: tmp1.to_reg(),
                    rs2: tmp2.to_reg(),
                }
                .emit_uncompressed(sink, emit_info, state, start_off);

                // Jump to the middle of the jump table.
                // We add a 16 byte offset here, since we used 4 instructions
                // since the AUIPC that was used to get the PC.
                Inst::Jalr {
                    rd: writable_zero_reg(),
                    base: tmp1.to_reg(),
                    offset: Imm12::from_i16((4 * Inst::UNCOMPRESSED_INSTRUCTION_SIZE) as i16),
                }
                .emit_uncompressed(sink, emit_info, state, start_off);

                // Emit the jump table.
                //
                // Each entry is a auipc + jalr to the target block. We also start with a island
                // if necessary.

                // Emit the jumps back to back
                for target in targets.iter() {
                    sink.use_label_at_offset(sink.cur_offset(), *target, LabelUse::PCRel32);

                    Inst::construct_auipc_and_jalr(None, tmp2, 0)
                        .iter()
                        .for_each(|i| i.emit_uncompressed(sink, emit_info, state, start_off));
                }

                // We've just emitted an island that is safe up to *here*.
                // Mark it as such so that we don't needlessly emit additional islands.
                *start_off = sink.cur_offset();
            }

            &Inst::VirtualSPOffsetAdj { amount } => {
                crate::trace!(
                    "virtual sp offset adjusted by {} -> {}",
                    amount,
                    state.virtual_sp_offset + amount
                );
                state.virtual_sp_offset += amount;
            }
            &Inst::Atomic {
                op,
                rd,
                addr,
                src,
                amo,
            } => {
                let srcloc = state.cur_srcloc();
                if !srcloc.is_default() {
                    sink.add_trap(TrapCode::HeapOutOfBounds);
                }
                let x = op.op_code()
                    | reg_to_gpr_num(rd.to_reg()) << 7
                    | op.funct3() << 12
                    | reg_to_gpr_num(addr) << 15
                    | reg_to_gpr_num(src) << 20
                    | op.funct7(amo) << 25;

                sink.put4(x);
            }
            &Inst::Fence { pred, succ } => {
                let x = 0b0001111
                    | 0b00000 << 7
                    | 0b000 << 12
                    | 0b00000 << 15
                    | (succ as u32) << 20
                    | (pred as u32) << 24;

                sink.put4(x);
            }
            &Inst::Auipc { rd, imm } => {
                sink.put4(enc_auipc(rd, imm));
            }

            &Inst::LoadAddr { rd, mem } => {
                let base = mem.get_base_register();
                let offset = mem.get_offset_with_state(state);
                let offset_imm12 = Imm12::maybe_from_i64(offset);

                match (mem, base, offset_imm12) {
                    (_, Some(rs), Some(imm12)) => {
                        Inst::AluRRImm12 {
                            alu_op: AluOPRRI::Addi,
                            rd,
                            rs,
                            imm12,
                        }
                        .emit(&[], sink, emit_info, state);
                    }
                    (_, Some(rs), None) => {
                        let mut insts = Inst::load_constant_u64(rd, offset as u64);
                        insts.push(Inst::AluRRR {
                            alu_op: AluOPRRR::Add,
                            rd,
                            rs1: rd.to_reg(),
                            rs2: rs,
                        });
                        insts
                            .into_iter()
                            .for_each(|inst| inst.emit(&[], sink, emit_info, state));
                    }
                    (AMode::Const(addr), None, _) => {
                        // Get an address label for the constant and recurse.
                        let label = sink.get_label_for_constant(addr);
                        Inst::LoadAddr {
                            rd,
                            mem: AMode::Label(label),
                        }
                        .emit(&[], sink, emit_info, state);
                    }
                    (AMode::Label(label), None, _) => {
                        // Get the current PC.
                        sink.use_label_at_offset(sink.cur_offset(), label, LabelUse::PCRelHi20);
                        let inst = Inst::Auipc {
                            rd,
                            imm: Imm20::ZERO,
                        };
                        inst.emit_uncompressed(sink, emit_info, state, start_off);

                        // Emit an add to the address with a relocation.
                        // This later gets patched up with the correct offset.
                        sink.use_label_at_offset(sink.cur_offset(), label, LabelUse::PCRelLo12I);
                        Inst::AluRRImm12 {
                            alu_op: AluOPRRI::Addi,
                            rd,
                            rs: rd.to_reg(),
                            imm12: Imm12::ZERO,
                        }
                        .emit_uncompressed(sink, emit_info, state, start_off);
                    }
                    (amode, _, _) => {
                        unimplemented!("LoadAddr: {:?}", amode);
                    }
                }
            }

            &Inst::Select {
                ref dst,
                condition,
                ref x,
                ref y,
            } => {
                let label_true = sink.get_label();
                let label_false = sink.get_label();
                let label_end = sink.get_label();
                Inst::CondBr {
                    taken: CondBrTarget::Label(label_true),
                    not_taken: CondBrTarget::Label(label_false),
                    kind: condition,
                }
                .emit(&[], sink, emit_info, state);
                sink.bind_label(label_true, &mut state.ctrl_plane);
                // here is the true
                // select the first value
                for i in gen_moves(dst.regs(), x.regs()) {
                    i.emit(&[], sink, emit_info, state);
                }
                Inst::gen_jump(label_end).emit(&[], sink, emit_info, state);

                sink.bind_label(label_false, &mut state.ctrl_plane);
                for i in gen_moves(dst.regs(), y.regs()) {
                    i.emit(&[], sink, emit_info, state);
                }

                sink.bind_label(label_end, &mut state.ctrl_plane);
            }
            &Inst::Jalr { rd, base, offset } => {
                sink.put4(enc_jalr(rd, base, offset));
            }
            &Inst::EBreak => {
                sink.put4(0x00100073);
            }
            &Inst::Icmp { cc, rd, a, b, ty } => {
                let label_true = sink.get_label();
                let label_false = sink.get_label();
                let label_end = sink.get_label();

                Inst::lower_br_icmp(
                    cc,
                    a,
                    b,
                    CondBrTarget::Label(label_true),
                    CondBrTarget::Label(label_false),
                    ty,
                )
                .into_iter()
                .for_each(|i| i.emit(&[], sink, emit_info, state));

                sink.bind_label(label_true, &mut state.ctrl_plane);
                Inst::load_imm12(rd, Imm12::ONE).emit(&[], sink, emit_info, state);
                Inst::gen_jump(label_end).emit(&[], sink, emit_info, state);
                sink.bind_label(label_false, &mut state.ctrl_plane);
                Inst::load_imm12(rd, Imm12::ZERO).emit(&[], sink, emit_info, state);
                sink.bind_label(label_end, &mut state.ctrl_plane);
            }
            &Inst::AtomicCas {
                offset,
                t0,
                dst,
                e,
                addr,
                v,
                ty,
            } => {
                //     # addr holds address of memory location
                //     # e holds expected value
                //     # v holds desired value
                //     # dst holds return value
                // cas:
                //     lr.w dst, (addr)       # Load original value.
                //     bne dst, e, fail       # Doesn’t match, so fail.
                //     sc.w t0, v, (addr)     # Try to update.
                //     bnez t0 , cas          # if store not ok,retry.
                // fail:
                let fail_label = sink.get_label();
                let cas_lebel = sink.get_label();
                sink.bind_label(cas_lebel, &mut state.ctrl_plane);
                Inst::Atomic {
                    op: AtomicOP::load_op(ty),
                    rd: dst,
                    addr,
                    src: zero_reg(),
                    amo: AMO::SeqCst,
                }
                .emit(&[], sink, emit_info, state);
                if ty.bits() < 32 {
                    AtomicOP::extract(dst, offset, dst.to_reg(), ty)
                        .iter()
                        .for_each(|i| i.emit(&[], sink, emit_info, state));
                } else if ty.bits() == 32 {
                    Inst::Extend {
                        rd: dst,
                        rn: dst.to_reg(),
                        signed: false,
                        from_bits: 32,
                        to_bits: 64,
                    }
                    .emit(&[], sink, emit_info, state);
                }
                Inst::CondBr {
                    taken: CondBrTarget::Label(fail_label),
                    not_taken: CondBrTarget::Fallthrough,
                    kind: IntegerCompare {
                        kind: IntCC::NotEqual,
                        rs1: e,
                        rs2: dst.to_reg(),
                    },
                }
                .emit(&[], sink, emit_info, state);
                let store_value = if ty.bits() < 32 {
                    // reload value to t0.
                    Inst::Atomic {
                        op: AtomicOP::load_op(ty),
                        rd: t0,
                        addr,
                        src: zero_reg(),
                        amo: AMO::SeqCst,
                    }
                    .emit(&[], sink, emit_info, state);
                    // set reset part.
                    AtomicOP::merge(t0, writable_spilltmp_reg(), offset, v, ty)
                        .iter()
                        .for_each(|i| i.emit(&[], sink, emit_info, state));
                    t0.to_reg()
                } else {
                    v
                };
                Inst::Atomic {
                    op: AtomicOP::store_op(ty),
                    rd: t0,
                    addr,
                    src: store_value,
                    amo: AMO::SeqCst,
                }
                .emit(&[], sink, emit_info, state);
                // check is our value stored.
                Inst::CondBr {
                    taken: CondBrTarget::Label(cas_lebel),
                    not_taken: CondBrTarget::Fallthrough,
                    kind: IntegerCompare {
                        kind: IntCC::NotEqual,
                        rs1: t0.to_reg(),
                        rs2: zero_reg(),
                    },
                }
                .emit(&[], sink, emit_info, state);
                sink.bind_label(fail_label, &mut state.ctrl_plane);
            }
            &Inst::AtomicRmwLoop {
                offset,
                op,
                dst,
                ty,
                p,
                x,
                t0,
            } => {
                let retry = sink.get_label();
                sink.bind_label(retry, &mut state.ctrl_plane);
                // load old value.
                Inst::Atomic {
                    op: AtomicOP::load_op(ty),
                    rd: dst,
                    addr: p,
                    src: zero_reg(),
                    amo: AMO::SeqCst,
                }
                .emit(&[], sink, emit_info, state);
                //

                let store_value: Reg = match op {
                    crate::ir::AtomicRmwOp::Add
                    | crate::ir::AtomicRmwOp::Sub
                    | crate::ir::AtomicRmwOp::And
                    | crate::ir::AtomicRmwOp::Or
                    | crate::ir::AtomicRmwOp::Xor => {
                        AtomicOP::extract(dst, offset, dst.to_reg(), ty)
                            .iter()
                            .for_each(|i| i.emit(&[], sink, emit_info, state));
                        Inst::AluRRR {
                            alu_op: match op {
                                crate::ir::AtomicRmwOp::Add => AluOPRRR::Add,
                                crate::ir::AtomicRmwOp::Sub => AluOPRRR::Sub,
                                crate::ir::AtomicRmwOp::And => AluOPRRR::And,
                                crate::ir::AtomicRmwOp::Or => AluOPRRR::Or,
                                crate::ir::AtomicRmwOp::Xor => AluOPRRR::Xor,
                                _ => unreachable!(),
                            },
                            rd: t0,
                            rs1: dst.to_reg(),
                            rs2: x,
                        }
                        .emit(&[], sink, emit_info, state);
                        Inst::Atomic {
                            op: AtomicOP::load_op(ty),
                            rd: writable_spilltmp_reg2(),
                            addr: p,
                            src: zero_reg(),
                            amo: AMO::SeqCst,
                        }
                        .emit(&[], sink, emit_info, state);
                        AtomicOP::merge(
                            writable_spilltmp_reg2(),
                            writable_spilltmp_reg(),
                            offset,
                            t0.to_reg(),
                            ty,
                        )
                        .iter()
                        .for_each(|i| i.emit(&[], sink, emit_info, state));
                        spilltmp_reg2()
                    }
                    crate::ir::AtomicRmwOp::Nand => {
                        if ty.bits() < 32 {
                            AtomicOP::extract(dst, offset, dst.to_reg(), ty)
                                .iter()
                                .for_each(|i| i.emit(&[], sink, emit_info, state));
                        }
                        Inst::AluRRR {
                            alu_op: AluOPRRR::And,
                            rd: t0,
                            rs1: x,
                            rs2: dst.to_reg(),
                        }
                        .emit(&[], sink, emit_info, state);
                        Inst::construct_bit_not(t0, t0.to_reg()).emit(&[], sink, emit_info, state);
                        if ty.bits() < 32 {
                            Inst::Atomic {
                                op: AtomicOP::load_op(ty),
                                rd: writable_spilltmp_reg2(),
                                addr: p,
                                src: zero_reg(),
                                amo: AMO::SeqCst,
                            }
                            .emit(&[], sink, emit_info, state);
                            AtomicOP::merge(
                                writable_spilltmp_reg2(),
                                writable_spilltmp_reg(),
                                offset,
                                t0.to_reg(),
                                ty,
                            )
                            .iter()
                            .for_each(|i| i.emit(&[], sink, emit_info, state));
                            spilltmp_reg2()
                        } else {
                            t0.to_reg()
                        }
                    }

                    crate::ir::AtomicRmwOp::Umin
                    | crate::ir::AtomicRmwOp::Umax
                    | crate::ir::AtomicRmwOp::Smin
                    | crate::ir::AtomicRmwOp::Smax => {
                        let label_select_dst = sink.get_label();
                        let label_select_done = sink.get_label();
                        if op == crate::ir::AtomicRmwOp::Umin || op == crate::ir::AtomicRmwOp::Umax
                        {
                            AtomicOP::extract(dst, offset, dst.to_reg(), ty)
                        } else {
                            AtomicOP::extract_sext(dst, offset, dst.to_reg(), ty)
                        }
                        .iter()
                        .for_each(|i| i.emit(&[], sink, emit_info, state));
                        Inst::lower_br_icmp(
                            match op {
                                crate::ir::AtomicRmwOp::Umin => IntCC::UnsignedLessThan,
                                crate::ir::AtomicRmwOp::Umax => IntCC::UnsignedGreaterThan,
                                crate::ir::AtomicRmwOp::Smin => IntCC::SignedLessThan,
                                crate::ir::AtomicRmwOp::Smax => IntCC::SignedGreaterThan,
                                _ => unreachable!(),
                            },
                            ValueRegs::one(dst.to_reg()),
                            ValueRegs::one(x),
                            CondBrTarget::Label(label_select_dst),
                            CondBrTarget::Fallthrough,
                            ty,
                        )
                        .iter()
                        .for_each(|i| i.emit(&[], sink, emit_info, state));
                        // here we select x.
                        Inst::gen_move(t0, x, I64).emit(&[], sink, emit_info, state);
                        Inst::gen_jump(label_select_done).emit(&[], sink, emit_info, state);
                        sink.bind_label(label_select_dst, &mut state.ctrl_plane);
                        Inst::gen_move(t0, dst.to_reg(), I64).emit(&[], sink, emit_info, state);
                        sink.bind_label(label_select_done, &mut state.ctrl_plane);
                        Inst::Atomic {
                            op: AtomicOP::load_op(ty),
                            rd: writable_spilltmp_reg2(),
                            addr: p,
                            src: zero_reg(),
                            amo: AMO::SeqCst,
                        }
                        .emit(&[], sink, emit_info, state);
                        AtomicOP::merge(
                            writable_spilltmp_reg2(),
                            writable_spilltmp_reg(),
                            offset,
                            t0.to_reg(),
                            ty,
                        )
                        .iter()
                        .for_each(|i| i.emit(&[], sink, emit_info, state));
                        spilltmp_reg2()
                    }
                    crate::ir::AtomicRmwOp::Xchg => {
                        AtomicOP::extract(dst, offset, dst.to_reg(), ty)
                            .iter()
                            .for_each(|i| i.emit(&[], sink, emit_info, state));
                        Inst::Atomic {
                            op: AtomicOP::load_op(ty),
                            rd: writable_spilltmp_reg2(),
                            addr: p,
                            src: zero_reg(),
                            amo: AMO::SeqCst,
                        }
                        .emit(&[], sink, emit_info, state);
                        AtomicOP::merge(
                            writable_spilltmp_reg2(),
                            writable_spilltmp_reg(),
                            offset,
                            x,
                            ty,
                        )
                        .iter()
                        .for_each(|i| i.emit(&[], sink, emit_info, state));
                        spilltmp_reg2()
                    }
                };

                Inst::Atomic {
                    op: AtomicOP::store_op(ty),
                    rd: t0,
                    addr: p,
                    src: store_value,
                    amo: AMO::SeqCst,
                }
                .emit(&[], sink, emit_info, state);

                // if store is not ok,retry.
                Inst::CondBr {
                    taken: CondBrTarget::Label(retry),
                    not_taken: CondBrTarget::Fallthrough,
                    kind: IntegerCompare {
                        kind: IntCC::NotEqual,
                        rs1: t0.to_reg(),
                        rs2: zero_reg(),
                    },
                }
                .emit(&[], sink, emit_info, state);
            }

            &Inst::FcvtToInt {
                is_sat,
                rd,
                rs,
                is_signed,
                in_type,
                out_type,
                tmp,
            } => {
                let label_nan = sink.get_label();
                let label_jump_over = sink.get_label();
                // get if nan.
                Inst::emit_not_nan(rd, rs, in_type).emit(&[], sink, emit_info, state);
                // jump to nan.
                Inst::CondBr {
                    taken: CondBrTarget::Label(label_nan),
                    not_taken: CondBrTarget::Fallthrough,
                    kind: IntegerCompare {
                        kind: IntCC::Equal,
                        rs2: zero_reg(),
                        rs1: rd.to_reg(),
                    },
                }
                .emit(&[], sink, emit_info, state);

                if !is_sat {
                    let f32_bounds = f32_cvt_to_int_bounds(is_signed, out_type.bits() as u8);
                    let f64_bounds = f64_cvt_to_int_bounds(is_signed, out_type.bits() as u8);
                    if in_type == F32 {
                        Inst::load_fp_constant32(tmp, f32_bits(f32_bounds.0), |_| {
                            writable_spilltmp_reg()
                        })
                    } else {
                        Inst::load_fp_constant64(tmp, f64_bits(f64_bounds.0), |_| {
                            writable_spilltmp_reg()
                        })
                    }
                    .iter()
                    .for_each(|i| i.emit(&[], sink, emit_info, state));

                    let le_op = if in_type == F32 {
                        FpuOPRRR::FleS
                    } else {
                        FpuOPRRR::FleD
                    };

                    // rd := rs <= tmp
                    Inst::FpuRRR {
                        alu_op: le_op,
                        frm: None,
                        rd,
                        rs1: rs,
                        rs2: tmp.to_reg(),
                    }
                    .emit(&[], sink, emit_info, state);
                    Inst::TrapIf {
                        cc: IntCC::NotEqual,
                        rs1: rd.to_reg(),
                        rs2: zero_reg(),
                        trap_code: TrapCode::IntegerOverflow,
                    }
                    .emit(&[], sink, emit_info, state);

                    if in_type == F32 {
                        Inst::load_fp_constant32(tmp, f32_bits(f32_bounds.1), |_| {
                            writable_spilltmp_reg()
                        })
                    } else {
                        Inst::load_fp_constant64(tmp, f64_bits(f64_bounds.1), |_| {
                            writable_spilltmp_reg()
                        })
                    }
                    .iter()
                    .for_each(|i| i.emit(&[], sink, emit_info, state));

                    // rd := rs >= tmp
                    Inst::FpuRRR {
                        alu_op: le_op,
                        frm: None,
                        rd,
                        rs1: tmp.to_reg(),
                        rs2: rs,
                    }
                    .emit(&[], sink, emit_info, state);

                    Inst::TrapIf {
                        cc: IntCC::NotEqual,
                        rs1: rd.to_reg(),
                        rs2: zero_reg(),
                        trap_code: TrapCode::IntegerOverflow,
                    }
                    .emit(&[], sink, emit_info, state);
                }
                // convert to int normally.
                Inst::FpuRR {
                    frm: Some(FRM::RTZ),
                    alu_op: FpuOPRR::float_convert_2_int_op(in_type, is_signed, out_type),
                    rd,
                    rs,
                }
                .emit(&[], sink, emit_info, state);
                if out_type.bits() < 32 && is_signed {
                    // load value part mask.
                    Inst::load_constant_u32(
                        writable_spilltmp_reg(),
                        if 16 == out_type.bits() {
                            (u16::MAX >> 1) as u64
                        } else {
                            // I8
                            (u8::MAX >> 1) as u64
                        },
                    )
                    .into_iter()
                    .for_each(|x| x.emit(&[], sink, emit_info, state));
                    // keep value part.
                    Inst::AluRRR {
                        alu_op: AluOPRRR::And,
                        rd: writable_spilltmp_reg(),
                        rs1: rd.to_reg(),
                        rs2: spilltmp_reg(),
                    }
                    .emit(&[], sink, emit_info, state);
                    // extact sign bit.
                    Inst::AluRRImm12 {
                        alu_op: AluOPRRI::Srli,
                        rd: rd,
                        rs: rd.to_reg(),
                        imm12: Imm12::from_i16(31),
                    }
                    .emit(&[], sink, emit_info, state);
                    Inst::AluRRImm12 {
                        alu_op: AluOPRRI::Slli,
                        rd: rd,
                        rs: rd.to_reg(),
                        imm12: Imm12::from_i16(if 16 == out_type.bits() {
                            15
                        } else {
                            // I8
                            7
                        }),
                    }
                    .emit(&[], sink, emit_info, state);
                    // make result,sign bit and value part.
                    Inst::AluRRR {
                        alu_op: AluOPRRR::Or,
                        rd: rd,
                        rs1: rd.to_reg(),
                        rs2: spilltmp_reg(),
                    }
                    .emit(&[], sink, emit_info, state);
                }

                // I already have the result,jump over.
                Inst::gen_jump(label_jump_over).emit(&[], sink, emit_info, state);
                // here is nan , move 0 into rd register
                sink.bind_label(label_nan, &mut state.ctrl_plane);
                if is_sat {
                    Inst::load_imm12(rd, Imm12::ZERO).emit(&[], sink, emit_info, state);
                } else {
                    // here is ud2.
                    Inst::Udf {
                        trap_code: TrapCode::BadConversionToInteger,
                    }
                    .emit(&[], sink, emit_info, state);
                }
                // bind jump_over
                sink.bind_label(label_jump_over, &mut state.ctrl_plane);
            }

            &Inst::LoadExtName {
                rd,
                ref name,
                offset,
            } => {
                let label_data = sink.get_label();
                let label_end = sink.get_label();

                // Load the value from a label
                Inst::Load {
                    rd,
                    op: LoadOP::Ld,
                    flags: MemFlags::trusted(),
                    from: AMode::Label(label_data),
                }
                .emit(&[], sink, emit_info, state);

                // Jump over the data
                Inst::gen_jump(label_end).emit(&[], sink, emit_info, state);

                sink.bind_label(label_data, &mut state.ctrl_plane);
                sink.add_reloc(Reloc::Abs8, name.as_ref(), offset);
                sink.put8(0);

                sink.bind_label(label_end, &mut state.ctrl_plane);
            }

            &Inst::ElfTlsGetAddr { rd, ref name } => {
                // RISC-V's TLS GD model is slightly different from other arches.
                //
                // We have a relocation (R_RISCV_TLS_GD_HI20) that loads the high 20 bits
                // of the address relative to the GOT entry. This relocation points to
                // the symbol as usual.
                //
                // However when loading the bottom 12bits of the address, we need to
                // use a label that points to the previous AUIPC instruction.
                //
                // label:
                //    auipc a0,0                    # R_RISCV_TLS_GD_HI20 (symbol)
                //    addi  a0,a0,0                 # R_RISCV_PCREL_LO12_I (label)
                //
                // https://github.com/riscv-non-isa/riscv-elf-psabi-doc/blob/master/riscv-elf.adoc#global-dynamic

                // Create the lable that is going to be published to the final binary object.
                let auipc_label = sink.get_label();
                sink.bind_label(auipc_label, &mut state.ctrl_plane);

                // Get the current PC.
                sink.add_reloc(Reloc::RiscvTlsGdHi20, &**name, 0);
                Inst::Auipc {
                    rd: rd,
                    imm: Imm20::from_i32(0),
                }
                .emit_uncompressed(sink, emit_info, state, start_off);

                // The `addi` here, points to the `auipc` label instead of directly to the symbol.
                sink.add_reloc(Reloc::RiscvPCRelLo12I, &auipc_label, 0);
                Inst::AluRRImm12 {
                    alu_op: AluOPRRI::Addi,
                    rd: rd,
                    rs: rd.to_reg(),
                    imm12: Imm12::from_i16(0),
                }
                .emit_uncompressed(sink, emit_info, state, start_off);

                Inst::Call {
                    info: Box::new(CallInfo {
                        dest: ExternalName::LibCall(LibCall::ElfTlsGetAddr),
                        uses: smallvec![],
                        defs: smallvec![],
                        opcode: crate::ir::Opcode::TlsValue,
                        caller_callconv: CallConv::SystemV,
                        callee_callconv: CallConv::SystemV,
                        callee_pop_size: 0,
                        clobbers: PRegSet::empty(),
                    }),
                }
                .emit_uncompressed(sink, emit_info, state, start_off);
            }

            &Inst::TrapIf {
                rs1,
                rs2,
                cc,
                trap_code,
            } => {
                let label_end = sink.get_label();
                let cond = IntegerCompare { kind: cc, rs1, rs2 };

                // Jump over the trap if we the condition is false.
                Inst::CondBr {
                    taken: CondBrTarget::Label(label_end),
                    not_taken: CondBrTarget::Fallthrough,
                    kind: cond.inverse(),
                }
                .emit(&[], sink, emit_info, state);
                Inst::Udf { trap_code }.emit(&[], sink, emit_info, state);

                sink.bind_label(label_end, &mut state.ctrl_plane);
            }
            &Inst::Udf { trap_code } => {
                sink.add_trap(trap_code);
                if let Some(s) = state.take_stack_map() {
                    sink.add_stack_map(
                        StackMapExtent::UpcomingBytes(Inst::TRAP_OPCODE.len() as u32),
                        s,
                    );
                }
                sink.put_data(Inst::TRAP_OPCODE);
            }
            &Inst::AtomicLoad { rd, ty, p } => {
                // emit the fence.
                Inst::Fence {
                    pred: Inst::FENCE_REQ_R | Inst::FENCE_REQ_W,
                    succ: Inst::FENCE_REQ_R | Inst::FENCE_REQ_W,
                }
                .emit(&[], sink, emit_info, state);
                // load.
                Inst::Load {
                    rd: rd,
                    op: LoadOP::from_type(ty),
                    flags: MemFlags::new(),
                    from: AMode::RegOffset(p, 0, ty),
                }
                .emit(&[], sink, emit_info, state);
                Inst::Fence {
                    pred: Inst::FENCE_REQ_R,
                    succ: Inst::FENCE_REQ_R | Inst::FENCE_REQ_W,
                }
                .emit(&[], sink, emit_info, state);
            }
            &Inst::AtomicStore { src, ty, p } => {
                Inst::Fence {
                    pred: Inst::FENCE_REQ_R | Inst::FENCE_REQ_W,
                    succ: Inst::FENCE_REQ_W,
                }
                .emit(&[], sink, emit_info, state);
                Inst::Store {
                    to: AMode::RegOffset(p, 0, ty),
                    op: StoreOP::from_type(ty),
                    flags: MemFlags::new(),
                    src,
                }
                .emit(&[], sink, emit_info, state);
            }
            &Inst::FloatRound {
                op,
                rd,
                int_tmp,
                f_tmp,
                rs,
                ty,
            } => {
                // this code is port from glibc ceil floor ... implementation.
                let label_nan = sink.get_label();
                let label_x = sink.get_label();
                let label_jump_over = sink.get_label();
                // check if is nan.
                Inst::emit_not_nan(int_tmp, rs, ty).emit(&[], sink, emit_info, state);
                Inst::CondBr {
                    taken: CondBrTarget::Label(label_nan),
                    not_taken: CondBrTarget::Fallthrough,
                    kind: IntegerCompare {
                        kind: IntCC::Equal,
                        rs1: int_tmp.to_reg(),
                        rs2: zero_reg(),
                    },
                }
                .emit(&[], sink, emit_info, state);
                fn max_value_need_round(ty: Type) -> u64 {
                    match ty {
                        F32 => {
                            let x: u64 = 1 << f32::MANTISSA_DIGITS;
                            let x = x as f32;
                            let x = u32::from_le_bytes(x.to_le_bytes());
                            x as u64
                        }
                        F64 => {
                            let x: u64 = 1 << f64::MANTISSA_DIGITS;
                            let x = x as f64;
                            u64::from_le_bytes(x.to_le_bytes())
                        }
                        _ => unreachable!(),
                    }
                }
                // load max value need to round.
                if ty == F32 {
                    Inst::load_fp_constant32(f_tmp, max_value_need_round(ty) as u32, &mut |_| {
                        writable_spilltmp_reg()
                    })
                } else {
                    Inst::load_fp_constant64(f_tmp, max_value_need_round(ty), &mut |_| {
                        writable_spilltmp_reg()
                    })
                }
                .into_iter()
                .for_each(|i| i.emit(&[], sink, emit_info, state));

                // get abs value.
                Inst::emit_fabs(rd, rs, ty).emit(&[], sink, emit_info, state);

                // branch if f_tmp < rd
                Inst::FpuRRR {
                    frm: None,
                    alu_op: if ty == F32 {
                        FpuOPRRR::FltS
                    } else {
                        FpuOPRRR::FltD
                    },
                    rd: int_tmp,
                    rs1: f_tmp.to_reg(),
                    rs2: rd.to_reg(),
                }
                .emit(&[], sink, emit_info, state);

                Inst::CondBr {
                    taken: CondBrTarget::Label(label_x),
                    not_taken: CondBrTarget::Fallthrough,
                    kind: IntegerCompare {
                        kind: IntCC::NotEqual,
                        rs1: int_tmp.to_reg(),
                        rs2: zero_reg(),
                    },
                }
                .emit(&[], sink, emit_info, state);

                //convert to int.
                Inst::FpuRR {
                    alu_op: FpuOPRR::float_convert_2_int_op(ty, true, I64),
                    frm: Some(op.to_frm()),
                    rd: int_tmp,
                    rs: rs,
                }
                .emit(&[], sink, emit_info, state);
                //convert back.
                Inst::FpuRR {
                    alu_op: if ty == F32 {
                        FpuOPRR::FcvtSL
                    } else {
                        FpuOPRR::FcvtDL
                    },
                    frm: Some(op.to_frm()),
                    rd,
                    rs: int_tmp.to_reg(),
                }
                .emit(&[], sink, emit_info, state);
                // copy sign.
                Inst::FpuRRR {
                    alu_op: if ty == F32 {
                        FpuOPRRR::FsgnjS
                    } else {
                        FpuOPRRR::FsgnjD
                    },
                    frm: None,
                    rd,
                    rs1: rd.to_reg(),
                    rs2: rs,
                }
                .emit(&[], sink, emit_info, state);
                // jump over.
                Inst::gen_jump(label_jump_over).emit(&[], sink, emit_info, state);
                // here is nan.
                sink.bind_label(label_nan, &mut state.ctrl_plane);
                Inst::FpuRRR {
                    alu_op: if ty == F32 {
                        FpuOPRRR::FaddS
                    } else {
                        FpuOPRRR::FaddD
                    },
                    frm: None,
                    rd: rd,
                    rs1: rs,
                    rs2: rs,
                }
                .emit(&[], sink, emit_info, state);
                Inst::gen_jump(label_jump_over).emit(&[], sink, emit_info, state);
                // here select origin x.
                sink.bind_label(label_x, &mut state.ctrl_plane);
                Inst::gen_move(rd, rs, ty).emit(&[], sink, emit_info, state);
                sink.bind_label(label_jump_over, &mut state.ctrl_plane);
            }

            &Inst::FloatSelect {
                op,
                rd,
                tmp,
                rs1,
                rs2,
                ty,
            } => {
                let label_nan = sink.get_label();
                let label_jump_over = sink.get_label();
                // check if rs1 is nan.
                Inst::emit_not_nan(tmp, rs1, ty).emit(&[], sink, emit_info, state);
                Inst::CondBr {
                    taken: CondBrTarget::Label(label_nan),
                    not_taken: CondBrTarget::Fallthrough,
                    kind: IntegerCompare {
                        kind: IntCC::Equal,
                        rs1: tmp.to_reg(),
                        rs2: zero_reg(),
                    },
                }
                .emit(&[], sink, emit_info, state);
                // check if rs2 is nan.
                Inst::emit_not_nan(tmp, rs2, ty).emit(&[], sink, emit_info, state);
                Inst::CondBr {
                    taken: CondBrTarget::Label(label_nan),
                    not_taken: CondBrTarget::Fallthrough,
                    kind: IntegerCompare {
                        kind: IntCC::Equal,
                        rs1: tmp.to_reg(),
                        rs2: zero_reg(),
                    },
                }
                .emit(&[], sink, emit_info, state);
                // here rs1 and rs2 is not nan.
                Inst::FpuRRR {
                    alu_op: op.to_fpuoprrr(ty),
                    frm: None,
                    rd: rd,
                    rs1: rs1,
                    rs2: rs2,
                }
                .emit(&[], sink, emit_info, state);
                // special handle for +0 or -0.
                {
                    // check is rs1 and rs2 all equal to zero.
                    let label_done = sink.get_label();
                    {
                        // if rs1 == 0
                        let mut insts = Inst::emit_if_float_not_zero(
                            tmp,
                            rs1,
                            ty,
                            CondBrTarget::Label(label_done),
                            CondBrTarget::Fallthrough,
                        );
                        insts.extend(Inst::emit_if_float_not_zero(
                            tmp,
                            rs2,
                            ty,
                            CondBrTarget::Label(label_done),
                            CondBrTarget::Fallthrough,
                        ));
                        insts
                            .iter()
                            .for_each(|i| i.emit(&[], sink, emit_info, state));
                    }
                    Inst::FpuRR {
                        alu_op: FpuOPRR::move_f_to_x_op(ty),
                        frm: None,
                        rd: tmp,
                        rs: rs1,
                    }
                    .emit(&[], sink, emit_info, state);
                    Inst::FpuRR {
                        alu_op: FpuOPRR::move_f_to_x_op(ty),
                        frm: None,
                        rd: writable_spilltmp_reg(),
                        rs: rs2,
                    }
                    .emit(&[], sink, emit_info, state);
                    Inst::AluRRR {
                        alu_op: if op == FloatSelectOP::Max {
                            AluOPRRR::And
                        } else {
                            AluOPRRR::Or
                        },
                        rd: tmp,
                        rs1: tmp.to_reg(),
                        rs2: spilltmp_reg(),
                    }
                    .emit(&[], sink, emit_info, state);
                    // move back to rd.
                    Inst::FpuRR {
                        alu_op: FpuOPRR::move_x_to_f_op(ty),
                        frm: None,
                        rd,
                        rs: tmp.to_reg(),
                    }
                    .emit(&[], sink, emit_info, state);
                    //
                    sink.bind_label(label_done, &mut state.ctrl_plane);
                }
                // we have the reuslt,jump over.
                Inst::gen_jump(label_jump_over).emit(&[], sink, emit_info, state);
                // here is nan.
                sink.bind_label(label_nan, &mut state.ctrl_plane);
                op.snan_bits(tmp, ty)
                    .into_iter()
                    .for_each(|i| i.emit(&[], sink, emit_info, state));
                // move to rd.
                Inst::FpuRR {
                    alu_op: FpuOPRR::move_x_to_f_op(ty),
                    frm: None,
                    rd,
                    rs: tmp.to_reg(),
                }
                .emit(&[], sink, emit_info, state);
                sink.bind_label(label_jump_over, &mut state.ctrl_plane);
            }
            &Inst::Popcnt {
                sum,
                tmp,
                step,
                rs,
                ty,
            } => {
                // load 0 to sum , init.
                Inst::gen_move(sum, zero_reg(), I64).emit(&[], sink, emit_info, state);
                // load
                Inst::load_imm12(step, Imm12::from_i16(ty.bits() as i16)).emit(
                    &[],
                    sink,
                    emit_info,
                    state,
                );
                //
                Inst::load_imm12(tmp, Imm12::ONE).emit(&[], sink, emit_info, state);
                Inst::AluRRImm12 {
                    alu_op: AluOPRRI::Slli,
                    rd: tmp,
                    rs: tmp.to_reg(),
                    imm12: Imm12::from_i16((ty.bits() - 1) as i16),
                }
                .emit(&[], sink, emit_info, state);
                let label_done = sink.get_label();
                let label_loop = sink.get_label();
                sink.bind_label(label_loop, &mut state.ctrl_plane);
                Inst::CondBr {
                    taken: CondBrTarget::Label(label_done),
                    not_taken: CondBrTarget::Fallthrough,
                    kind: IntegerCompare {
                        kind: IntCC::SignedLessThanOrEqual,
                        rs1: step.to_reg(),
                        rs2: zero_reg(),
                    },
                }
                .emit(&[], sink, emit_info, state);
                // test and add sum.
                {
                    Inst::AluRRR {
                        alu_op: AluOPRRR::And,
                        rd: writable_spilltmp_reg2(),
                        rs1: tmp.to_reg(),
                        rs2: rs,
                    }
                    .emit(&[], sink, emit_info, state);
                    let label_over = sink.get_label();
                    Inst::CondBr {
                        taken: CondBrTarget::Label(label_over),
                        not_taken: CondBrTarget::Fallthrough,
                        kind: IntegerCompare {
                            kind: IntCC::Equal,
                            rs1: zero_reg(),
                            rs2: spilltmp_reg2(),
                        },
                    }
                    .emit(&[], sink, emit_info, state);
                    Inst::AluRRImm12 {
                        alu_op: AluOPRRI::Addi,
                        rd: sum,
                        rs: sum.to_reg(),
                        imm12: Imm12::ONE,
                    }
                    .emit(&[], sink, emit_info, state);
                    sink.bind_label(label_over, &mut state.ctrl_plane);
                }
                // set step and tmp.
                {
                    Inst::AluRRImm12 {
                        alu_op: AluOPRRI::Addi,
                        rd: step,
                        rs: step.to_reg(),
                        imm12: Imm12::from_i16(-1),
                    }
                    .emit(&[], sink, emit_info, state);
                    Inst::AluRRImm12 {
                        alu_op: AluOPRRI::Srli,
                        rd: tmp,
                        rs: tmp.to_reg(),
                        imm12: Imm12::ONE,
                    }
                    .emit(&[], sink, emit_info, state);
                    Inst::gen_jump(label_loop).emit(&[], sink, emit_info, state);
                }
                sink.bind_label(label_done, &mut state.ctrl_plane);
            }
            &Inst::Rev8 { rs, rd, tmp, step } => {
                // init.
                Inst::gen_move(rd, zero_reg(), I64).emit(&[], sink, emit_info, state);
                Inst::gen_move(tmp, rs, I64).emit(&[], sink, emit_info, state);
                // load 56 to step.
                Inst::load_imm12(step, Imm12::from_i16(56)).emit(&[], sink, emit_info, state);
                let label_done = sink.get_label();
                let label_loop = sink.get_label();
                sink.bind_label(label_loop, &mut state.ctrl_plane);
                Inst::CondBr {
                    taken: CondBrTarget::Label(label_done),
                    not_taken: CondBrTarget::Fallthrough,
                    kind: IntegerCompare {
                        kind: IntCC::SignedLessThan,
                        rs1: step.to_reg(),
                        rs2: zero_reg(),
                    },
                }
                .emit(&[], sink, emit_info, state);
                Inst::AluRRImm12 {
                    alu_op: AluOPRRI::Andi,
                    rd: writable_spilltmp_reg(),
                    rs: tmp.to_reg(),
                    imm12: Imm12::from_i16(255),
                }
                .emit(&[], sink, emit_info, state);
                Inst::AluRRR {
                    alu_op: AluOPRRR::Sll,
                    rd: writable_spilltmp_reg(),
                    rs1: spilltmp_reg(),
                    rs2: step.to_reg(),
                }
                .emit(&[], sink, emit_info, state);

                Inst::AluRRR {
                    alu_op: AluOPRRR::Or,
                    rd: rd,
                    rs1: rd.to_reg(),
                    rs2: spilltmp_reg(),
                }
                .emit(&[], sink, emit_info, state);

                {
                    // reset step
                    Inst::AluRRImm12 {
                        alu_op: AluOPRRI::Addi,
                        rd: step,
                        rs: step.to_reg(),
                        imm12: Imm12::from_i16(-8),
                    }
                    .emit(&[], sink, emit_info, state);
                    //reset tmp.
                    Inst::AluRRImm12 {
                        alu_op: AluOPRRI::Srli,
                        rd: tmp,
                        rs: tmp.to_reg(),
                        imm12: Imm12::from_i16(8),
                    }
                    .emit(&[], sink, emit_info, state);
                    // loop.
                    Inst::gen_jump(label_loop).emit(&[], sink, emit_info, state);
                }

                sink.bind_label(label_done, &mut state.ctrl_plane);
            }
            &Inst::Cltz {
                sum,
                tmp,
                step,
                rs,
                leading,
                ty,
            } => {
                // load 0 to sum , init.
                Inst::gen_move(sum, zero_reg(), I64).emit(&[], sink, emit_info, state);
                // load
                Inst::load_imm12(step, Imm12::from_i16(ty.bits() as i16)).emit(
                    &[],
                    sink,
                    emit_info,
                    state,
                );
                //
                Inst::load_imm12(tmp, Imm12::ONE).emit(&[], sink, emit_info, state);
                if leading {
                    Inst::AluRRImm12 {
                        alu_op: AluOPRRI::Slli,
                        rd: tmp,
                        rs: tmp.to_reg(),
                        imm12: Imm12::from_i16((ty.bits() - 1) as i16),
                    }
                    .emit(&[], sink, emit_info, state);
                }
                let label_done = sink.get_label();
                let label_loop = sink.get_label();
                sink.bind_label(label_loop, &mut state.ctrl_plane);
                Inst::CondBr {
                    taken: CondBrTarget::Label(label_done),
                    not_taken: CondBrTarget::Fallthrough,
                    kind: IntegerCompare {
                        kind: IntCC::SignedLessThanOrEqual,
                        rs1: step.to_reg(),
                        rs2: zero_reg(),
                    },
                }
                .emit(&[], sink, emit_info, state);
                // test and add sum.
                {
                    Inst::AluRRR {
                        alu_op: AluOPRRR::And,
                        rd: writable_spilltmp_reg2(),
                        rs1: tmp.to_reg(),
                        rs2: rs,
                    }
                    .emit(&[], sink, emit_info, state);
                    Inst::CondBr {
                        taken: CondBrTarget::Label(label_done),
                        not_taken: CondBrTarget::Fallthrough,
                        kind: IntegerCompare {
                            kind: IntCC::NotEqual,
                            rs1: zero_reg(),
                            rs2: spilltmp_reg2(),
                        },
                    }
                    .emit(&[], sink, emit_info, state);
                    Inst::AluRRImm12 {
                        alu_op: AluOPRRI::Addi,
                        rd: sum,
                        rs: sum.to_reg(),
                        imm12: Imm12::ONE,
                    }
                    .emit(&[], sink, emit_info, state);
                }
                // set step and tmp.
                {
                    Inst::AluRRImm12 {
                        alu_op: AluOPRRI::Addi,
                        rd: step,
                        rs: step.to_reg(),
                        imm12: Imm12::from_i16(-1),
                    }
                    .emit(&[], sink, emit_info, state);
                    Inst::AluRRImm12 {
                        alu_op: if leading {
                            AluOPRRI::Srli
                        } else {
                            AluOPRRI::Slli
                        },
                        rd: tmp,
                        rs: tmp.to_reg(),
                        imm12: Imm12::ONE,
                    }
                    .emit(&[], sink, emit_info, state);
                    Inst::gen_jump(label_loop).emit(&[], sink, emit_info, state);
                }
                sink.bind_label(label_done, &mut state.ctrl_plane);
            }
            &Inst::Brev8 {
                rs,
                ty,
                step,
                tmp,
                tmp2,
                rd,
            } => {
                Inst::gen_move(rd, zero_reg(), I64).emit(&[], sink, emit_info, state);
                Inst::load_imm12(step, Imm12::from_i16(ty.bits() as i16)).emit(
                    &[],
                    sink,
                    emit_info,
                    state,
                );
                //
                Inst::load_imm12(tmp, Imm12::ONE).emit(&[], sink, emit_info, state);
                Inst::AluRRImm12 {
                    alu_op: AluOPRRI::Slli,
                    rd: tmp,
                    rs: tmp.to_reg(),
                    imm12: Imm12::from_i16((ty.bits() - 1) as i16),
                }
                .emit(&[], sink, emit_info, state);
                Inst::load_imm12(tmp2, Imm12::ONE).emit(&[], sink, emit_info, state);
                Inst::AluRRImm12 {
                    alu_op: AluOPRRI::Slli,
                    rd: tmp2,
                    rs: tmp2.to_reg(),
                    imm12: Imm12::from_i16((ty.bits() - 8) as i16),
                }
                .emit(&[], sink, emit_info, state);

                let label_done = sink.get_label();
                let label_loop = sink.get_label();
                sink.bind_label(label_loop, &mut state.ctrl_plane);
                Inst::CondBr {
                    taken: CondBrTarget::Label(label_done),
                    not_taken: CondBrTarget::Fallthrough,
                    kind: IntegerCompare {
                        kind: IntCC::SignedLessThanOrEqual,
                        rs1: step.to_reg(),
                        rs2: zero_reg(),
                    },
                }
                .emit(&[], sink, emit_info, state);
                // test and set bit.
                {
                    Inst::AluRRR {
                        alu_op: AluOPRRR::And,
                        rd: writable_spilltmp_reg2(),
                        rs1: tmp.to_reg(),
                        rs2: rs,
                    }
                    .emit(&[], sink, emit_info, state);
                    let label_over = sink.get_label();
                    Inst::CondBr {
                        taken: CondBrTarget::Label(label_over),
                        not_taken: CondBrTarget::Fallthrough,
                        kind: IntegerCompare {
                            kind: IntCC::Equal,
                            rs1: zero_reg(),
                            rs2: spilltmp_reg2(),
                        },
                    }
                    .emit(&[], sink, emit_info, state);
                    Inst::AluRRR {
                        alu_op: AluOPRRR::Or,
                        rd: rd,
                        rs1: rd.to_reg(),
                        rs2: tmp2.to_reg(),
                    }
                    .emit(&[], sink, emit_info, state);
                    sink.bind_label(label_over, &mut state.ctrl_plane);
                }
                // set step and tmp.
                {
                    Inst::AluRRImm12 {
                        alu_op: AluOPRRI::Addi,
                        rd: step,
                        rs: step.to_reg(),
                        imm12: Imm12::from_i16(-1),
                    }
                    .emit(&[], sink, emit_info, state);
                    Inst::AluRRImm12 {
                        alu_op: AluOPRRI::Srli,
                        rd: tmp,
                        rs: tmp.to_reg(),
                        imm12: Imm12::ONE,
                    }
                    .emit(&[], sink, emit_info, state);
                    {
                        // reset tmp2
                        // if (step %=8 == 0) then tmp2 = tmp2 >> 15
                        // if (step %=8 != 0) then tmp2 = tmp2 << 1
                        let label_over = sink.get_label();
                        let label_sll_1 = sink.get_label();
                        Inst::load_imm12(writable_spilltmp_reg2(), Imm12::from_i16(8)).emit(
                            &[],
                            sink,
                            emit_info,
                            state,
                        );
                        Inst::AluRRR {
                            alu_op: AluOPRRR::Rem,
                            rd: writable_spilltmp_reg2(),
                            rs1: step.to_reg(),
                            rs2: spilltmp_reg2(),
                        }
                        .emit(&[], sink, emit_info, state);
                        Inst::CondBr {
                            taken: CondBrTarget::Label(label_sll_1),
                            not_taken: CondBrTarget::Fallthrough,
                            kind: IntegerCompare {
                                kind: IntCC::NotEqual,
                                rs1: spilltmp_reg2(),
                                rs2: zero_reg(),
                            },
                        }
                        .emit(&[], sink, emit_info, state);
                        Inst::AluRRImm12 {
                            alu_op: AluOPRRI::Srli,
                            rd: tmp2,
                            rs: tmp2.to_reg(),
                            imm12: Imm12::from_i16(15),
                        }
                        .emit(&[], sink, emit_info, state);
                        Inst::gen_jump(label_over).emit(&[], sink, emit_info, state);
                        sink.bind_label(label_sll_1, &mut state.ctrl_plane);
                        Inst::AluRRImm12 {
                            alu_op: AluOPRRI::Slli,
                            rd: tmp2,
                            rs: tmp2.to_reg(),
                            imm12: Imm12::ONE,
                        }
                        .emit(&[], sink, emit_info, state);
                        sink.bind_label(label_over, &mut state.ctrl_plane);
                    }
                    Inst::gen_jump(label_loop).emit(&[], sink, emit_info, state);
                }
                sink.bind_label(label_done, &mut state.ctrl_plane);
            }
            &Inst::StackProbeLoop {
                guard_size,
                probe_count,
                tmp: guard_size_tmp,
            } => {
                let step = writable_spilltmp_reg();
                Inst::load_constant_u64(step, (guard_size as u64) * (probe_count as u64))
                    .iter()
                    .for_each(|i| i.emit(&[], sink, emit_info, state));
                Inst::load_constant_u64(guard_size_tmp, guard_size as u64)
                    .iter()
                    .for_each(|i| i.emit(&[], sink, emit_info, state));

                let loop_start = sink.get_label();
                let label_done = sink.get_label();
                sink.bind_label(loop_start, &mut state.ctrl_plane);
                Inst::CondBr {
                    taken: CondBrTarget::Label(label_done),
                    not_taken: CondBrTarget::Fallthrough,
                    kind: IntegerCompare {
                        kind: IntCC::UnsignedLessThanOrEqual,
                        rs1: step.to_reg(),
                        rs2: guard_size_tmp.to_reg(),
                    },
                }
                .emit(&[], sink, emit_info, state);
                // compute address.
                Inst::AluRRR {
                    alu_op: AluOPRRR::Sub,
                    rd: writable_spilltmp_reg2(),
                    rs1: stack_reg(),
                    rs2: step.to_reg(),
                }
                .emit(&[], sink, emit_info, state);
                Inst::Store {
                    to: AMode::RegOffset(spilltmp_reg2(), 0, I8),
                    op: StoreOP::Sb,
                    flags: MemFlags::new(),
                    src: zero_reg(),
                }
                .emit(&[], sink, emit_info, state);
                // reset step.
                Inst::AluRRR {
                    alu_op: AluOPRRR::Sub,
                    rd: step,
                    rs1: step.to_reg(),
                    rs2: guard_size_tmp.to_reg(),
                }
                .emit(&[], sink, emit_info, state);
                Inst::gen_jump(loop_start).emit(&[], sink, emit_info, state);
                sink.bind_label(label_done, &mut state.ctrl_plane);
            }
            &Inst::VecAluRRRImm5 {
                op,
                vd,
                vd_src,
                imm,
                vs2,
                ref mask,
                ..
            } => {
                debug_assert_eq!(vd.to_reg(), vd_src);

                sink.put4(encode_valu_rrr_imm(op, vd, imm, vs2, *mask));
            }
            &Inst::VecAluRRRR {
                op,
                vd,
                vd_src,
                vs1,
                vs2,
                ref mask,
                ..
            } => {
                debug_assert_eq!(vd.to_reg(), vd_src);

                sink.put4(encode_valu_rrrr(op, vd, vs2, vs1, *mask));
            }
            &Inst::VecAluRRR {
                op,
                vd,
                vs1,
                vs2,
                ref mask,
                ..
            } => {
                sink.put4(encode_valu(op, vd, vs1, vs2, *mask));
            }
            &Inst::VecAluRRImm5 {
                op,
                vd,
                imm,
                vs2,
                ref mask,
                ..
            } => {
                sink.put4(encode_valu_rr_imm(op, vd, imm, vs2, *mask));
            }
            &Inst::VecAluRR {
                op,
                vd,
                vs,
                ref mask,
                ..
            } => {
                sink.put4(encode_valu_rr(op, vd, vs, *mask));
            }
            &Inst::VecAluRImm5 {
                op,
                vd,
                imm,
                ref mask,
                ..
            } => {
                sink.put4(encode_valu_r_imm(op, vd, imm, *mask));
            }
            &Inst::VecSetState { rd, ref vstate } => {
                sink.put4(encode_vcfg_imm(
                    0x57,
                    rd.to_reg(),
                    vstate.avl.unwrap_static(),
                    &vstate.vtype,
                ));

                // Update the current vector emit state.
                state.vstate = EmitVState::Known(vstate.clone());
            }

            &Inst::VecLoad {
                eew,
                to,
                ref from,
                ref mask,
                flags,
                ..
            } => {
                // Vector Loads don't support immediate offsets, so we need to load it into a register.
                let addr = match from {
                    VecAMode::UnitStride { base } => {
                        let base_reg = base.get_base_register();
                        let offset = base.get_offset_with_state(state);

                        // Reg+0 Offset can be directly encoded
                        if let (Some(base_reg), 0) = (base_reg, offset) {
                            base_reg
                        } else {
                            // Otherwise load the address it into a reg and load from it.
                            let tmp = writable_spilltmp_reg();
                            Inst::LoadAddr {
                                rd: tmp,
                                mem: base.clone(),
                            }
                            .emit(&[], sink, emit_info, state);
                            tmp.to_reg()
                        }
                    }
                };

                let srcloc = state.cur_srcloc();
                if !srcloc.is_default() && !flags.notrap() {
                    // Register the offset at which the actual load instruction starts.
                    sink.add_trap(TrapCode::HeapOutOfBounds);
                }

                sink.put4(encode_vmem_load(
                    0x07,
                    to.to_reg(),
                    eew,
                    addr,
                    from.lumop(),
                    *mask,
                    from.mop(),
                    from.nf(),
                ));
            }

            &Inst::VecStore {
                eew,
                ref to,
                from,
                ref mask,
                flags,
                ..
            } => {
                // Vector Stores don't support immediate offsets, so we need to load it into a register.
                let addr = match to {
                    VecAMode::UnitStride { base } => {
                        let base_reg = base.get_base_register();
                        let offset = base.get_offset_with_state(state);

                        // Reg+0 Offset can be directly encoded
                        if let (Some(base_reg), 0) = (base_reg, offset) {
                            base_reg
                        } else {
                            // Otherwise load the address it into a reg and load from it.
                            let tmp = writable_spilltmp_reg();
                            Inst::LoadAddr {
                                rd: tmp,
                                mem: base.clone(),
                            }
                            .emit(&[], sink, emit_info, state);
                            tmp.to_reg()
                        }
                    }
                };

                let srcloc = state.cur_srcloc();
                if !srcloc.is_default() && !flags.notrap() {
                    // Register the offset at which the actual load instruction starts.
                    sink.add_trap(TrapCode::HeapOutOfBounds);
                }

                sink.put4(encode_vmem_store(
                    0x27,
                    from,
                    eew,
                    addr,
                    to.sumop(),
                    *mask,
                    to.mop(),
                    to.nf(),
                ));
            }
        };
    }

    fn allocate(self, allocs: &mut AllocationConsumer) -> Self {
        fn alloc_value_regs(
            orgin: &ValueRegs<Reg>,
            alloc: &mut AllocationConsumer,
        ) -> ValueRegs<Reg> {
            match orgin.regs().len() {
                1 => ValueRegs::one(alloc.next(orgin.regs()[0])),
                2 => ValueRegs::two(alloc.next(orgin.regs()[0]), alloc.next(orgin.regs()[1])),
                _ => unreachable!(),
            }
        }

        fn alloc_writable_value_regs(
            origin: &ValueRegs<Writable<Reg>>,
            alloc: &mut AllocationConsumer,
        ) -> ValueRegs<Writable<Reg>> {
            alloc_value_regs(&origin.map(|r| r.to_reg()), alloc).map(Writable::from_reg)
        }

        match self {
            Inst::Nop0 => self,
            Inst::Nop4 => self,
            Inst::RawData { .. } => self,
            Inst::Lui { rd, imm } => Inst::Lui {
                rd: allocs.next_writable(rd),
                imm,
            },
            Inst::LoadInlineConst { rd, ty, imm } => Inst::LoadInlineConst {
                rd: allocs.next_writable(rd),
                ty,
                imm,
            },
            Inst::FpuRR {
                frm,
                alu_op,
                rd,
                rs,
            } => Inst::FpuRR {
                rs: allocs.next(rs),
                rd: allocs.next_writable(rd),
                frm,
                alu_op,
            },
            Inst::FpuRRRR {
                alu_op,
                rd,
                rs1,
                rs2,
                rs3,
                frm,
            } => Inst::FpuRRRR {
                rs1: allocs.next(rs1),
                rs2: allocs.next(rs2),
                rs3: allocs.next(rs3),
                rd: allocs.next_writable(rd),
                alu_op,
                frm,
            },
            Inst::FpuRRR {
                alu_op,
                frm,
                rd,
                rs1,
                rs2,
            } => Inst::FpuRRR {
                alu_op,
                frm,
                rs1: allocs.next(rs1),
                rs2: allocs.next(rs2),
                rd: allocs.next_writable(rd),
            },
            Inst::Unwind { .. } => self,
            Inst::DummyUse { reg } => Inst::DummyUse {
                reg: allocs.next(reg),
            },
            Inst::AluRRR {
                alu_op,
                rd,
                rs1,
                rs2,
            } => Inst::AluRRR {
                alu_op,
                rs1: allocs.next(rs1),
                rs2: allocs.next(rs2),
                rd: allocs.next_writable(rd),
            },
            Inst::AluRRImm12 {
                alu_op,
                rd,
                rs,
                imm12,
            } => Inst::AluRRImm12 {
                alu_op,
                rs: allocs.next(rs),
                rd: allocs.next_writable(rd),
                imm12,
            },
            Inst::CsrReg { op, rd, rs, csr } => Inst::CsrReg {
                op,
                rs: allocs.next(rs),
                rd: allocs.next_writable(rd),
                csr,
            },
            Inst::CsrImm { op, rd, csr, imm } => Inst::CsrImm {
                op,
                rd: allocs.next_writable(rd),
                csr,
                imm,
            },
            Inst::Load {
                rd,
                op,
                from,
                flags,
            } => Inst::Load {
                from: from.clone().with_allocs(allocs),
                rd: allocs.next_writable(rd),
                op,
                flags,
            },
            Inst::Store { op, src, flags, to } => Inst::Store {
                op,
                flags,
                to: to.clone().with_allocs(allocs),
                src: allocs.next(src),
            },

            Inst::Args { .. } => self,
            Inst::Rets { .. } => self,
            Inst::Ret { .. } => self,

            Inst::Extend {
                rd,
                rn,
                signed,
                from_bits,
                to_bits,
            } => Inst::Extend {
                rn: allocs.next(rn),
                rd: allocs.next_writable(rd),
                signed,
                from_bits,
                to_bits,
            },
            Inst::AdjustSp { .. } => self,

            Inst::Call { .. } => self,
            Inst::CallInd { mut info } => {
                info.rn = allocs.next(info.rn);
                Inst::CallInd { info }
            }

            Inst::ReturnCall { callee, info } => {
                for u in &info.uses {
                    let _ = allocs.next(u.vreg);
                }

                Inst::ReturnCall { callee, info }
            }

            Inst::ReturnCallInd { callee, info } => {
                let callee = allocs.next(callee);

                for u in &info.uses {
                    let _ = allocs.next(u.vreg);
                }

                Inst::ReturnCallInd { callee, info }
            }

            Inst::Jal { .. } => self,

            Inst::CondBr {
                taken,
                not_taken,
                mut kind,
            } => {
                kind.rs1 = allocs.next(kind.rs1);
                kind.rs2 = allocs.next(kind.rs2);
                Inst::CondBr {
                    taken,
                    not_taken,
                    kind,
                }
            }

            Inst::Mov { rd, rm, ty } => Inst::Mov {
                ty,
                rm: allocs.next(rm),
                rd: allocs.next_writable(rd),
            },

            Inst::MovFromPReg { rd, rm } => {
                debug_assert!([px_reg(2), px_reg(8)].contains(&rm));
                let rd = allocs.next_writable(rd);
                Inst::MovFromPReg { rd, rm }
            }

            Inst::BrTable {
                index,
                tmp1,
                tmp2,
                targets,
            } => Inst::BrTable {
                index: allocs.next(index),
                tmp1: allocs.next_writable(tmp1),
                tmp2: allocs.next_writable(tmp2),
                targets,
            },

            Inst::VirtualSPOffsetAdj { .. } => self,
            Inst::Atomic {
                op,
                rd,
                addr,
                src,
                amo,
            } => Inst::Atomic {
                op,
                amo,
                addr: allocs.next(addr),
                src: allocs.next(src),
                rd: allocs.next_writable(rd),
            },
            Inst::Fence { .. } => self,
            Inst::Auipc { rd, imm } => Inst::Auipc {
                rd: allocs.next_writable(rd),
                imm,
            },

            Inst::LoadAddr { rd, mem } => Inst::LoadAddr {
                mem: mem.with_allocs(allocs),
                rd: allocs.next_writable(rd),
            },

            Inst::Select {
                ref dst,
                condition,
                ref x,
                ref y,
            } => {
                let mut condition: IntegerCompare = condition.clone();
                condition.rs1 = allocs.next(condition.rs1);
                condition.rs2 = allocs.next(condition.rs2);
                let x = alloc_value_regs(x, allocs);
                let y = alloc_value_regs(y, allocs);
                let dst = alloc_writable_value_regs(dst, allocs);

                Inst::Select {
                    dst,
                    condition,
                    x,
                    y,
                }
            }
            Inst::Jalr { rd, base, offset } => {
                // For some reason this does not use base?
                debug_assert!(base.is_real());
                Inst::Jalr {
                    rd: allocs.next_writable(rd),
                    base,
                    offset,
                }
            }

            Inst::EBreak => self,

            Inst::Icmp {
                cc,
                rd,
                ref a,
                ref b,
                ty,
            } => Inst::Icmp {
                cc,
                a: alloc_value_regs(a, allocs),
                b: alloc_value_regs(b, allocs),
                rd: allocs.next_writable(rd),
                ty,
            },

            Inst::AtomicCas {
                offset,
                t0,
                dst,
                e,
                addr,
                v,
                ty,
            } => Inst::AtomicCas {
                ty,
                offset: allocs.next(offset),
                e: allocs.next(e),
                addr: allocs.next(addr),
                v: allocs.next(v),
                t0: allocs.next_writable(t0),
                dst: allocs.next_writable(dst),
            },

            Inst::AtomicRmwLoop {
                offset,
                op,
                dst,
                ty,
                p,
                x,
                t0,
            } => Inst::AtomicRmwLoop {
                op,
                ty,
                offset: allocs.next(offset),
                p: allocs.next(p),
                x: allocs.next(x),
                t0: allocs.next_writable(t0),
                dst: allocs.next_writable(dst),
            },

            Inst::FcvtToInt {
                is_sat,
                rd,
                rs,
                is_signed,
                in_type,
                out_type,
                tmp,
            } => Inst::FcvtToInt {
                is_sat,
                is_signed,
                in_type,
                out_type,
                rs: allocs.next(rs),
                tmp: allocs.next_writable(tmp),
                rd: allocs.next_writable(rd),
            },

            Inst::LoadExtName { rd, name, offset } => Inst::LoadExtName {
                rd: allocs.next_writable(rd),
                name,
                offset,
            },

            Inst::ElfTlsGetAddr { rd, name } => {
                let rd = allocs.next_writable(rd);
                debug_assert_eq!(a0(), rd.to_reg());
                Inst::ElfTlsGetAddr { rd, name }
            }

            Inst::TrapIf {
                rs1,
                rs2,
                cc,
                trap_code,
            } => Inst::TrapIf {
                rs1: allocs.next(rs1),
                rs2: allocs.next(rs2),
                cc,
                trap_code,
            },

            Inst::Udf { .. } => self,

            Inst::AtomicLoad { rd, ty, p } => Inst::AtomicLoad {
                ty,
                p: allocs.next(p),
                rd: allocs.next_writable(rd),
            },

            Inst::AtomicStore { src, ty, p } => Inst::AtomicStore {
                ty,
                src: allocs.next(src),
                p: allocs.next(p),
            },

            Inst::FloatRound {
                op,
                rd,
                int_tmp,
                f_tmp,
                rs,
                ty,
            } => Inst::FloatRound {
                op,
                ty,
                rs: allocs.next(rs),
                int_tmp: allocs.next_writable(int_tmp),
                f_tmp: allocs.next_writable(f_tmp),
                rd: allocs.next_writable(rd),
            },

            Inst::FloatSelect {
                op,
                rd,
                tmp,
                rs1,
                rs2,
                ty,
            } => Inst::FloatSelect {
                op,
                ty,
                rs1: allocs.next(rs1),
                rs2: allocs.next(rs2),
                tmp: allocs.next_writable(tmp),
                rd: allocs.next_writable(rd),
            },

            Inst::Popcnt {
                sum,
                tmp,
                step,
                rs,
                ty,
            } => Inst::Popcnt {
                rs: allocs.next(rs),
                tmp: allocs.next_writable(tmp),
                step: allocs.next_writable(step),
                sum: allocs.next_writable(sum),
                ty,
            },

            Inst::Rev8 { rs, rd, tmp, step } => Inst::Rev8 {
                rs: allocs.next(rs),
                tmp: allocs.next_writable(tmp),
                step: allocs.next_writable(step),
                rd: allocs.next_writable(rd),
            },

            Inst::Cltz {
                sum,
                tmp,
                step,
                rs,
                leading,
                ty,
            } => Inst::Cltz {
                rs: allocs.next(rs),
                tmp: allocs.next_writable(tmp),
                step: allocs.next_writable(step),
                sum: allocs.next_writable(sum),
                leading,
                ty,
            },

            Inst::Brev8 {
                rs,
                ty,
                step,
                tmp,
                tmp2,
                rd,
            } => Inst::Brev8 {
                rs: allocs.next(rs),
                step: allocs.next_writable(step),
                tmp: allocs.next_writable(tmp),
                tmp2: allocs.next_writable(tmp2),
                rd: allocs.next_writable(rd),
                ty,
            },

            Inst::StackProbeLoop { .. } => self,

            Inst::VecAluRRRImm5 {
                op,
                vd,
                vd_src,
                imm,
                vs2,
                mask,
                vstate,
            } => Inst::VecAluRRRImm5 {
                op,
                vs2: allocs.next(vs2),
                vd_src: allocs.next(vd_src),
                vd: allocs.next_writable(vd),
                mask: mask.with_allocs(allocs),
                imm,
                vstate,
            },

            Inst::VecAluRRRR {
                op,
                vd,
                vd_src,
                vs1,
                vs2,
                mask,
                vstate,
            } => Inst::VecAluRRRR {
                op,
                vs1: allocs.next(vs1),
                vs2: allocs.next(vs2),
                vd_src: allocs.next(vd_src),
                vd: allocs.next_writable(vd),
                mask: mask.with_allocs(allocs),
                vstate,
            },

            Inst::VecAluRRR {
                op,
                vd,
                vs1,
                vs2,
                mask,
                vstate,
            } => Inst::VecAluRRR {
                op,
                vs1: allocs.next(vs1),
                vs2: allocs.next(vs2),
                vd: allocs.next_writable(vd),
                mask: mask.with_allocs(allocs),
                vstate,
            },

            Inst::VecAluRRImm5 {
                op,
                vd,
                imm,
                vs2,
                mask,
                vstate,
            } => Inst::VecAluRRImm5 {
                op,
                imm,
                vs2: allocs.next(vs2),
                vd: allocs.next_writable(vd),
                mask: mask.with_allocs(allocs),
                vstate,
            },

            Inst::VecAluRR {
                op,
                vd,
                vs,
                mask,
                vstate,
            } => Inst::VecAluRR {
                op,
                vs: allocs.next(vs),
                vd: allocs.next_writable(vd),
                mask: mask.with_allocs(allocs),
                vstate,
            },

            Inst::VecAluRImm5 {
                op,
                vd,
                imm,
                mask,
                vstate,
            } => Inst::VecAluRImm5 {
                vd: allocs.next_writable(vd),
                mask: mask.with_allocs(allocs),
                op,
                imm,
                vstate,
            },

            Inst::VecSetState { rd, vstate } => Inst::VecSetState {
                rd: allocs.next_writable(rd),
                vstate,
            },

            Inst::VecLoad {
                eew,
                to,
                from,
                mask,
                flags,
                vstate,
            } => Inst::VecLoad {
                eew,
                from: from.clone().with_allocs(allocs),
                to: allocs.next_writable(to),
                mask: mask.with_allocs(allocs),
                flags,
                vstate,
            },

            Inst::VecStore {
                eew,
                to,
                from,
                mask,
                flags,
                vstate,
            } => Inst::VecStore {
                eew,
                to: to.clone().with_allocs(allocs),
                from: allocs.next(from),
                mask: mask.with_allocs(allocs),
                flags,
                vstate,
            },
        }
    }
}

fn emit_return_call_common_sequence(
    sink: &mut MachBuffer<Inst>,
    emit_info: &EmitInfo,
    state: &mut EmitState,
    new_stack_arg_size: u32,
    old_stack_arg_size: u32,
) {
    // We are emitting a dynamic number of instructions and might need an
    // island. We emit four instructions regardless of how many stack arguments
    // we have, up to two instructions for the actual call, and then two
    // instructions per word of stack argument space.
    let new_stack_words = new_stack_arg_size / 8;
    let insts = 4 + 2 + 2 * new_stack_words;
    let space_needed = insts * u32::try_from(Inst::UNCOMPRESSED_INSTRUCTION_SIZE).unwrap();
    if sink.island_needed(space_needed) {
        let jump_around_label = sink.get_label();
        Inst::gen_jump(jump_around_label).emit(&[], sink, emit_info, state);
        sink.emit_island(space_needed + 4, &mut state.ctrl_plane);
        sink.bind_label(jump_around_label, &mut state.ctrl_plane);
    }

    // Copy the new frame on top of our current frame.
    //
    // The current stack layout is the following:
    //
    //            | ...                 |
    //            +---------------------+
    //            | ...                 |
    //            | stack arguments     |
    //            | ...                 |
    //    current | return address      |
    //    frame   | old FP              | <-- FP
    //            | ...                 |
    //            | old stack slots     |
    //            | ...                 |
    //            +---------------------+
    //            | ...                 |
    //    new     | new stack arguments |
    //    frame   | ...                 | <-- SP
    //            +---------------------+
    //
    // We need to restore the old FP, restore the return address from the stack
    // to the link register, copy the new stack arguments over the old stack
    // arguments, adjust SP to point to the new stack arguments, and then jump
    // to the callee (which will push the old FP and RA again). Note that the
    // actual jump happens outside this helper function.

    assert_eq!(
        new_stack_arg_size % 8,
        0,
        "size of new stack arguments must be 8-byte aligned"
    );

    // The delta from our frame pointer to the (eventual) stack pointer value
    // when we jump to the tail callee. This is the difference in size of stack
    // arguments as well as accounting for the two words we pushed onto the
    // stack upon entry to this function (the return address and old frame
    // pointer).
    let fp_to_callee_sp = i64::from(old_stack_arg_size) - i64::from(new_stack_arg_size) + 16;

    let tmp1 = regs::writable_spilltmp_reg();
    let tmp2 = regs::writable_spilltmp_reg2();

    // Restore the return address to the link register, and load the old FP into
    // a temporary register.
    //
    // We can't put the old FP into the FP register until after we copy the
    // stack arguments into place, since that uses address modes that are
    // relative to our current FP.
    //
    // Note that the FP is saved in the function prologue for all non-leaf
    // functions, even when `preserve_frame_pointers=false`. Note also that
    // `return_call` instructions make it so that a function is considered
    // non-leaf. Therefore we always have an FP to restore here.

    Inst::gen_load(
        writable_link_reg(),
        AMode::FPOffset(8, I64),
        I64,
        MemFlags::trusted(),
    )
    .emit(&[], sink, emit_info, state);
    Inst::gen_load(tmp1, AMode::FPOffset(0, I64), I64, MemFlags::trusted()).emit(
        &[],
        sink,
        emit_info,
        state,
    );

    // Copy the new stack arguments over the old stack arguments.
    for i in (0..new_stack_words).rev() {
        // Load the `i`th new stack argument word from the temporary stack
        // space.
        Inst::gen_load(
            tmp2,
            AMode::SPOffset(i64::from(i * 8), types::I64),
            types::I64,
            ir::MemFlags::trusted(),
        )
        .emit(&[], sink, emit_info, state);

        // Store it to its final destination on the stack, overwriting our
        // current frame.
        Inst::gen_store(
            AMode::FPOffset(fp_to_callee_sp + i64::from(i * 8), types::I64),
            tmp2.to_reg(),
            types::I64,
            ir::MemFlags::trusted(),
        )
        .emit(&[], sink, emit_info, state);
    }

    // Initialize the SP for the tail callee, deallocating the temporary stack
    // argument space and our current frame at the same time.
    Inst::AluRRImm12 {
        alu_op: AluOPRRI::Addi,
        rd: regs::writable_stack_reg(),
        rs: regs::fp_reg(),
        imm12: Imm12::maybe_from_i64(fp_to_callee_sp).unwrap(),
    }
    .emit(&[], sink, emit_info, state);

    // Move the old FP value from the temporary into the FP register.
    Inst::Mov {
        ty: types::I64,
        rd: regs::writable_fp_reg(),
        rm: tmp1.to_reg(),
    }
    .emit(&[], sink, emit_info, state);

    state.virtual_sp_offset -= i64::from(new_stack_arg_size);
    trace!(
        "return_call[_ind] adjusts virtual sp offset by {} -> {}",
        new_stack_arg_size,
        state.virtual_sp_offset
    );
}
