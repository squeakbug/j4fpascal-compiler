use std::fmt::{self, Display};

use crate::riscv::instruction::{Iorw, Instruction};

impl Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instruction::UNIMP => f.write_str("unimp"),
            // RV32I base instruction set:
            Instruction::LUI { rd, imm } => write!(f, "lui x{rd}, {imm}"),
            Instruction::AUIPC { rd, imm } => write!(f, "auipc x{rd}, {imm}"),
            Instruction::JAL { rd, offset } => write!(f, "jal x{rd}, {offset}"),
            Instruction::JALR { rd, rs1, offset } => write!(f, "jalr x{rd}, {offset}(x{rs1})"),
            Instruction::BEQ { rs1, rs2, offset } => write!(f, "beq x{rs1}, x{rs2}, {offset}"),
            Instruction::BNE { rs1, rs2, offset } => write!(f, "bne x{rs1}, x{rs2}, {offset}"),
            Instruction::BLT { rs1, rs2, offset } => write!(f, "blt x{rs1}, x{rs2}, {offset}"),
            Instruction::BGE { rs1, rs2, offset } => write!(f, "bge x{rs1}, x{rs2}, {offset}"),
            Instruction::BLTU { rs1, rs2, offset } => write!(f, "bltu x{rs1}, x{rs2}, {offset}"),
            Instruction::BGEU { rs1, rs2, offset } => write!(f, "bgeu x{rs1}, x{rs2}, {offset}"),
            Instruction::LB { rd, rs1, offset } => write!(f, "lb x{rd}, {offset}(x{rs1})"),
            Instruction::LH { rd, rs1, offset } => write!(f, "lh x{rd}, {offset}(x{rs1})"),
            Instruction::LW { rd, rs1, offset } => write!(f, "lw x{rd}, {offset}(x{rs1})"),
            Instruction::LBU { rd, rs1, offset } => write!(f, "lbu x{rd}, {offset}(x{rs1})"),
            Instruction::LHU { rd, rs1, offset } => write!(f, "lhu x{rd}, {offset}(x{rs1})"),
            Instruction::SB { rs1, rs2, offset } => write!(f, "sb x{rs2}, {offset}(x{rs1})"),
            Instruction::SH { rs1, rs2, offset } => write!(f, "sh x{rs2}, {offset}(x{rs1})"),
            Instruction::SW { rs1, rs2, offset } => write!(f, "sw x{rs2}, {offset}(x{rs1})"),
            Instruction::ADDI { rd, rs1, imm } => write!(f, "addi x{rd}, x{rs1}, {imm}"),
            Instruction::SLTI { rd, rs1, imm } => write!(f, "slti x{rd}, x{rs1}, {imm}"),
            Instruction::SLTIU { rd, rs1, imm } => write!(f, "sltiu x{rd}, x{rs1}, {imm}"),
            Instruction::XORI { rd, rs1, imm } => write!(f, "xori x{rd}, x{rs1}, {imm}"),
            Instruction::ORI { rd, rs1, imm } => write!(f, "ori x{rd}, x{rs1}, {imm}"),
            Instruction::ANDI { rd, rs1, imm } => write!(f, "andi x{rd}, x{rs1}, {imm}"),
            Instruction::SLLI { rd, rs1, shamt } => write!(f, "slli x{rd}, x{rs1}, {shamt}"),
            Instruction::SRLI { rd, rs1, shamt } => write!(f, "srli x{rd}, x{rs1}, {shamt}"),
            Instruction::SRAI { rd, rs1, shamt } => write!(f, "srai x{rd}, x{rs1}, {shamt}"),
            Instruction::ADD { rd, rs1, rs2 } => write!(f, "add x{rd}, x{rs1}, x{rs2}"),
            Instruction::SUB { rd, rs1, rs2 } => write!(f, "sub x{rd}, x{rs1}, x{rs2}"),
            Instruction::SLL { rd, rs1, rs2 } => write!(f, "sll x{rd}, x{rs1}, x{rs2}"),
            Instruction::SLT { rd, rs1, rs2 } => write!(f, "slt x{rd}, x{rs1}, x{rs2}"),
            Instruction::SLTU { rd, rs1, rs2 } => write!(f, "sltu x{rd}, x{rs1}, x{rs2}"),
            Instruction::XOR { rd, rs1, rs2 } => write!(f, "xor x{rd}, x{rs1}, x{rs2}"),
            Instruction::SRL { rd, rs1, rs2 } => write!(f, "srl x{rd}, x{rs1}, x{rs2}"),
            Instruction::SRA { rd, rs1, rs2 } => write!(f, "sra x{rd}, x{rs1}, x{rs2}"),
            Instruction::OR { rd, rs1, rs2 } => write!(f, "or x{rd}, x{rs1}, x{rs2}"),
            Instruction::AND { rd, rs1, rs2 } => write!(f, "and x{rd}, x{rs1}, x{rs2}"),
            Instruction::FENCE { pred, succ } => {
                write!(f, "fence {pred}, {succ}")
            }
            Instruction::ECALL => f.write_str("ecall"),
            Instruction::EBREAK => f.write_str("ebreak"),
            // RV64I base instruction set:
            Instruction::LWU { rd, rs1, offset } => write!(f, "lwu x{rd}, {offset}(x{rs1})"),
            Instruction::LD { rd, rs1, offset } => write!(f, "ld x{rd}, {offset}(x{rs1})"),
            Instruction::SD { rs1, rs2, offset } => write!(f, "sd x{rs2}, {offset}(x{rs1})"),
            Instruction::ADDIW { rd, rs1, imm } => write!(f, "addiw x{rd}, x{rs1}, {imm}"),
            Instruction::SLLIW { rd, rs1, shamt } => write!(f, "slliw x{rd}, x{rs1}, {shamt}"),
            Instruction::SRLIW { rd, rs1, shamt } => write!(f, "srliw x{rd}, x{rs1}, {shamt}"),
            Instruction::SRAIW { rd, rs1, shamt } => write!(f, "sraiw x{rd}, x{rs1}, {shamt}"),
            Instruction::ADDW { rd, rs1, rs2 } => write!(f, "addw x{rd}, x{rs1}, x{rs2}"),
            Instruction::SUBW { rd, rs1, rs2 } => write!(f, "subw x{rd}, x{rs1}, x{rs2}"),
            Instruction::SLLW { rd, rs1, rs2 } => write!(f, "sllw x{rd}, x{rs1}, x{rs2}"),
            Instruction::SRLW { rd, rs1, rs2 } => write!(f, "srlw x{rd}, x{rs1}, x{rs2}"),
            Instruction::SRAW { rd, rs1, rs2 } => write!(f, "sraw x{rd}, x{rs1}, x{rs2}"),
            // ZIFENCE
            Instruction::FENCE_I => f.write_str("fence.i"),
            // RV32M extension:
            Instruction::MUL { rd, rs1, rs2 } => write!(f, "mul x{rd}, x{rs1}, x{rs2}"),
            Instruction::MULH { rd, rs1, rs2 } => write!(f, "mulh x{rd}, x{rs1}, x{rs2}"),
            Instruction::MULHSU { rd, rs1, rs2 } => write!(f, "mulhsu x{rd}, x{rs1}, x{rs2}"),
            Instruction::MULHU { rd, rs1, rs2 } => write!(f, "mulhu x{rd}, x{rs1}, x{rs2}"),
            Instruction::DIV { rd, rs1, rs2 } => write!(f, "div x{rd}, x{rs1}, x{rs2}"),
            Instruction::DIVU { rd, rs1, rs2 } => write!(f, "divu x{rd}, x{rs1}, x{rs2}"),
            Instruction::REM { rd, rs1, rs2 } => write!(f, "rem x{rd}, x{rs1}, x{rs2}"),
            Instruction::REMU { rd, rs1, rs2 } => write!(f, "remu x{rd}, x{rs1}, x{rs2}"),
            // RV64M extension:
            Instruction::MULW { rd, rs1, rs2 } => write!(f, "mulw x{rd}, x{rs1}, x{rs2}"),
            Instruction::DIVW { rd, rs1, rs2 } => write!(f, "divw x{rd}, x{rs1}, x{rs2}"),
            Instruction::DIVUW { rd, rs1, rs2 } => write!(f, "divuw x{rd}, x{rs1}, x{rs2}"),
            Instruction::REMW { rd, rs1, rs2 } => write!(f, "remw x{rd}, x{rs1}, x{rs2}"),
            Instruction::REMUW { rd, rs1, rs2 } => write!(f, "remuw x{rd}, x{rs1}, x{rs2}"),
            // RV32F extension:
            Instruction::FLW { frd, rs1, offset } => write!(f, "flw f{frd}, {offset}(x{rs1})"),
            Instruction::FSW { rs1, frs2, offset } => write!(f, "fsw f{frs2}, {offset}(x{rs1})"),
            Instruction::FMADD_S { frd, frs1, frs2, frs3, .. } => {
                write!(f, "fmadd.s f{frd}, f{frs1}, f{frs2}, f{frs3}")
            }
            Instruction::FMSUB_S { frd, frs1, frs2, frs3, .. } => {
                write!(f, "fmsub.s f{frd}, f{frs1}, f{frs2}, f{frs3}")
            }
            Instruction::FNMSUB_S { frd, frs1, frs2, frs3, .. } => {
                write!(f, "fnmsub.s f{frd}, f{frs1}, f{frs2}, f{frs3}")
            }
            Instruction::FNMADD_S { frd, frs1, frs2, frs3, .. } => {
                write!(f, "fnmadd.s f{frd}, f{frs1}, f{frs2}, f{frs3}")
            }
            Instruction::FADD_S { frd, frs1, frs2, .. } => {
                write!(f, "fadd.s f{frd}, f{frs1}, f{frs2}")
            }
            Instruction::FSUB_S { frd, frs1, frs2, .. } => {
                write!(f, "fsub.s f{frd}, f{frs1}, f{frs2}")
            }
            Instruction::FMUL_S { frd, frs1, frs2, .. } => {
                write!(f, "fmul.s f{frd}, f{frs1}, f{frs2}")
            }
            Instruction::FDIV_S { frd, frs1, frs2, .. } => {
                write!(f, "fdiv.s f{frd}, f{frs1}, f{frs2}")
            }
            Instruction::FSQRT_S { frd, frs1, .. } => write!(f, "fsqrt.s f{frd}, f{frs1}"),
            Instruction::FSGNJ_S { frd, frs1, frs2 } => {
                write!(f, "fsgnj.s f{frd}, f{frs1}, f{frs2}")
            }
            Instruction::FSGNJN_S { frd, frs1, frs2 } => {
                write!(f, "fsgnjn.s f{frd}, f{frs1}, f{frs2}")
            }
            Instruction::FSGNJX_S { frd, frs1, frs2 } => {
                write!(f, "fsgnjx.s f{frd}, f{frs1}, f{frs2}")
            }
            Instruction::FMIN_S { frd, frs1, frs2 } => write!(f, "fmin.s f{frd}, f{frs1}, f{frs2}"),
            Instruction::FMAX_S { frd, frs1, frs2 } => write!(f, "fmax.s f{frd}, f{frs1}, f{frs2}"),
            Instruction::FCVT_W_S { rd, frs1, .. } => write!(f, "fcvt.w.s x{rd}, f{frs1}"),
            Instruction::FCVT_WU_S { rd, frs1, .. } => write!(f, "fcvt.wu.s x{rd}, f{frs1}"),
            Instruction::FMV_X_W { rd, frs1 } => write!(f, "fmv.x.w x{rd}, f{frs1}"),
            Instruction::FEQ_S { rd, frs1, frs2 } => write!(f, "feq.s x{rd}, f{frs1}, f{frs2}"),
            Instruction::FLT_S { rd, frs1, frs2 } => write!(f, "flt.s x{rd}, f{frs1}, f{frs2}"),
            Instruction::FLE_S { rd, frs1, frs2 } => write!(f, "fle.s x{rd}, f{frs1}, f{frs2}"),
            Instruction::FCLASS_S { rd, frs1 } => write!(f, "fclass.s x{rd}, f{frs1}"),
            Instruction::FCVT_S_W { frd, rs1, .. } => write!(f, "fcvt.s.w f{frd}, x{rs1}"),
            Instruction::FCVT_S_WU { frd, rs1, .. } => write!(f, "fcvt.s.wu f{frd}, x{rs1}"),
            Instruction::FMV_W_X { frd, rs1 } => write!(f, "fmv.w.x f{frd}, x{rs1}"),
            // RV64F extension:
            Instruction::FCVT_L_S { rd, frs1, .. } => write!(f, "fcvt.l.s x{rd}, f{frs1}"),
            Instruction::FCVT_LU_S { rd, frs1, .. } => write!(f, "fcvt.lu.s x{rd}, f{frs1}"),
            Instruction::FCVT_S_L { frd, rs1, .. } => write!(f, "fcvt.s.l f{frd}, x{rs1}"),
            Instruction::FCVT_S_LU { frd, rs1, .. } => write!(f, "fcvt.s.lu f{frd}, x{rs1}"),
        }
    }
}

impl Display for Iorw {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if *self & Iorw::I == Iorw::I {
            f.write_str("i")?;
        }
        if *self & Iorw::O == Iorw::O {
            f.write_str("o")?;
        }
        if *self & Iorw::R == Iorw::R {
            f.write_str("r")?;
        }
        if *self & Iorw::W == Iorw::W {
            f.write_str("w")?;
        }

        Ok(())
    }
}
