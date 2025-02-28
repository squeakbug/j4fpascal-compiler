#[allow(non_camel_case_types)]
#[allow(clippy::upper_case_acronyms)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Instruction {
    // Unknown:
    UNIMP,
    // RV32I base instruction set:
    LUI { rd: u32, imm: u32 },
    AUIPC { rd: u32, imm: u32 },
    JAL { rd: u32, offset: i32 },
    JALR { rd: u32, rs1: u32, offset: i32 },
    BEQ { rs1: u32, rs2: u32, offset: i32 },
    BNE { rs1: u32, rs2: u32, offset: i32 },
    BLT { rs1: u32, rs2: u32, offset: i32 },
    BGE { rs1: u32, rs2: u32, offset: i32 },
    BLTU { rs1: u32, rs2: u32, offset: i32 },
    BGEU { rs1: u32, rs2: u32, offset: i32 },
    LB { rd: u32, rs1: u32, offset: i32 },
    LH { rd: u32, rs1: u32, offset: i32 },
    LW { rd: u32, rs1: u32, offset: i32 },
    LBU { rd: u32, rs1: u32, offset: i32 },
    LHU { rd: u32, rs1: u32, offset: i32 },
    SB { rs1: u32, rs2: u32, offset: i32 },
    SH { rs1: u32, rs2: u32, offset: i32 },
    SW { rs1: u32, rs2: u32, offset: i32 },
    ADDI { rd: u32, rs1: u32, imm: i32 },
    SLTI { rd: u32, rs1: u32, imm: i32 },
    SLTIU { rd: u32, rs1: u32, imm: i32 },
    XORI { rd: u32, rs1: u32, imm: i32 },
    ORI { rd: u32, rs1: u32, imm: i32 },
    ANDI { rd: u32, rs1: u32, imm: i32 },
    SLLI { rd: u32, rs1: u32, shamt: u32 },
    SRLI { rd: u32, rs1: u32, shamt: u32 },
    SRAI { rd: u32, rs1: u32, shamt: u32 },
    ADD { rd: u32, rs1: u32, rs2: u32 },
    SUB { rd: u32, rs1: u32, rs2: u32 },
    SLL { rd: u32, rs1: u32, rs2: u32 },
    SLT { rd: u32, rs1: u32, rs2: u32 },
    SLTU { rd: u32, rs1: u32, rs2: u32 },
    XOR { rd: u32, rs1: u32, rs2: u32 },
    SRL { rd: u32, rs1: u32, rs2: u32 },
    SRA { rd: u32, rs1: u32, rs2: u32 },
    OR { rd: u32, rs1: u32, rs2: u32 },
    AND { rd: u32, rs1: u32, rs2: u32 },
    FENCE { pred: Iorw, succ: Iorw },
    ECALL,
    EBREAK,
    // RV64I base instruction set:
    LWU { rd: u32, rs1: u32, offset: i32 },
    LD { rd: u32, rs1: u32, offset: i32 },
    SD { rs1: u32, rs2: u32, offset: i32 },
    ADDIW { rd: u32, rs1: u32, imm: i32 },
    SLLIW { rd: u32, rs1: u32, shamt: u32 },
    SRLIW { rd: u32, rs1: u32, shamt: u32 },
    SRAIW { rd: u32, rs1: u32, shamt: u32 },
    ADDW { rd: u32, rs1: u32, rs2: u32 },
    SUBW { rd: u32, rs1: u32, rs2: u32 },
    SLLW { rd: u32, rs1: u32, rs2: u32 },
    SRLW { rd: u32, rs1: u32, rs2: u32 },
    SRAW { rd: u32, rs1: u32, rs2: u32 },
    // RV32/RV64 Zifencei:
    FENCE_I,
    // RV32M extension:
    MUL { rd: u32, rs1: u32, rs2: u32 },
    MULH { rd: u32, rs1: u32, rs2: u32 },
    MULHSU { rd: u32, rs1: u32, rs2: u32 },
    MULHU { rd: u32, rs1: u32, rs2: u32 },
    DIV { rd: u32, rs1: u32, rs2: u32 },
    DIVU { rd: u32, rs1: u32, rs2: u32 },
    REM { rd: u32, rs1: u32, rs2: u32 },
    REMU { rd: u32, rs1: u32, rs2: u32 },
    // RV64M extension:
    MULW { rd: u32, rs1: u32, rs2: u32 },
    DIVW { rd: u32, rs1: u32, rs2: u32 },
    DIVUW { rd: u32, rs1: u32, rs2: u32 },
    REMW { rd: u32, rs1: u32, rs2: u32 },
    REMUW { rd: u32, rs1: u32, rs2: u32 },
    // RV32F extension:
    FLW { frd: u32, rs1: u32, offset: i32 },
    FSW { rs1: u32, frs2: u32, offset: i32 },
    FMADD_S { frd: u32, rm: u32, frs1: u32, frs2: u32, frs3: u32 },
    FMSUB_S { frd: u32, rm: u32, frs1: u32, frs2: u32, frs3: u32 },
    FNMSUB_S { frd: u32, rm: u32, frs1: u32, frs2: u32, frs3: u32 },
    FNMADD_S { frd: u32, rm: u32, frs1: u32, frs2: u32, frs3: u32 },
    FADD_S { frd: u32, rm: u32, frs1: u32, frs2: u32 },
    FSUB_S { frd: u32, rm: u32, frs1: u32, frs2: u32 },
    FMUL_S { frd: u32, rm: u32, frs1: u32, frs2: u32 },
    FDIV_S { frd: u32, rm: u32, frs1: u32, frs2: u32 },
    FSQRT_S { frd: u32, rm: u32, frs1: u32 },
    FSGNJ_S { frd: u32, frs1: u32, frs2: u32 },
    FSGNJN_S { frd: u32, frs1: u32, frs2: u32 },
    FSGNJX_S { frd: u32, frs1: u32, frs2: u32 },
    FMIN_S { frd: u32, frs1: u32, frs2: u32 },
    FMAX_S { frd: u32, frs1: u32, frs2: u32 },
    FCVT_W_S { rd: u32, rm: u32, frs1: u32 },
    FCVT_WU_S { rd: u32, rm: u32, frs1: u32 },
    FMV_X_W { rd: u32, frs1: u32 },
    FEQ_S { rd: u32, frs1: u32, frs2: u32 },
    FLT_S { rd: u32, frs1: u32, frs2: u32 },
    FLE_S { rd: u32, frs1: u32, frs2: u32 },
    FCLASS_S { rd: u32, frs1: u32 },
    FCVT_S_W { frd: u32, rm: u32, rs1: u32 },
    FCVT_S_WU { frd: u32, rm: u32, rs1: u32 },
    FMV_W_X { frd: u32, rs1: u32 },
    // RV64F extension:
    FCVT_L_S { rd: u32, rm: u32, frs1: u32 },
    FCVT_LU_S { rd: u32, rm: u32, frs1: u32 },
    FCVT_S_L { frd: u32, rm: u32, rs1: u32 },
    FCVT_S_LU { frd: u32, rm: u32, rs1: u32 },
}

impl Instruction {
    pub fn branch(&self) -> bool {
        matches!(
            self,
            Instruction::JAL { .. }
                | Instruction::JALR { .. }
                | Instruction::BEQ { .. }
                | Instruction::BNE { .. }
                | Instruction::BLT { .. }
                | Instruction::BGE { .. }
                | Instruction::BLTU { .. }
                | Instruction::BGEU { .. }
        )
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Iorw(pub(crate) u8);
