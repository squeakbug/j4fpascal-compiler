use crate::riscv::{
    instruction::{Instruction, Iorw},
    target::{Target, Xlen},
};

/// Decode one instruction from the start of some little-endian bytes. The
/// decoded instruction and its length in bytes are returned if successful.
///
/// Unsupported and unknown instructions are decoded as [`Instruction::UNIMP`].
/// Compressed instructions are decompressed.
///
/// `None` is returned if there are not enough bytes given to determine the
/// instruction.
pub fn decode_le_bytes(bytes: &[u8], target: &Target) -> Option<(Instruction, usize)> {
    let (code, _) = bytes.split_at_checked(4)?;

    let code: [u8; 4] = code.try_into().ok()?;
    let code = u32::from_le_bytes(code);

    let instruction = decode(code, target);
    Some((instruction, 4))
}

#[derive(Clone, Debug)]
pub struct Decoder<'a> {
    /// Target configuration to decode for.
    target: Target,
    bytes: &'a [u8],
}

impl<'a> Decoder<'a> {
    pub fn from_le_bytes(target: Target, bytes: &'a [u8]) -> Decoder<'a> {
        Decoder { target, bytes }
    }
}

impl Iterator for Decoder<'_> {
    type Item = Instruction;

    fn next(&mut self) -> Option<Self::Item> {
        let (insn, len) = decode_le_bytes(self.bytes, &self.target)?;
        self.bytes = &self.bytes[len..];

        Some(insn)
    }
}

/// Decode a 32-bit instruction.
///
/// Unsupported and unknown instructions are decoded to [`Instruction::UNIMP`].
pub fn decode(code: u32, target: &Target) -> Instruction {
    // Extract target features for shorter `if`s later on.
    let rv32 = target.xlen == Xlen::Rv32;
    let rv64 = target.xlen == Xlen::Rv64;
    let ext_zifencei = target.zifencei;
    let ext_m = target.m;
    let ext_f = target.f;

    let Encoding {
        opcode,
        funct3,
        funct7,
        rd,
        rs1,
        rs2,
        rs3,
        rm,
        uimm,
        rl,
        aq,
        csr,
        succ,
        pred,
        i_imm,
        s_imm,
        b_imm,
        u_imm,
        j_imm,
        shamt32,
        shamt64,
    } = Encoding::from(code);

    // Aliases:
    let frd = rd;
    let frs1 = rs1;
    let frs2 = rs2;
    let frs3 = rs3;
    let shamt = match target.xlen {
        Xlen::Rv32 => shamt32,
        Xlen::Rv64 => shamt64,
    };

    use Instruction::*;
    match (opcode, funct3, funct7) {
        // Base instruction set:
        (Op::LUI, _, _) => LUI { rd, imm: u_imm },
        (Op::AUIPC, _, _) => AUIPC { rd, imm: u_imm },
        (Op::JAL, _, _) => JAL { rd, offset: j_imm },
        (Op::JALR, 0b000, _) => JALR { rd, rs1, offset: i_imm },
        (Op::BRANCH, 0b000, _) => BEQ { rs1, rs2, offset: b_imm },
        (Op::BRANCH, 0b001, _) => BNE { rs1, rs2, offset: b_imm },
        (Op::BRANCH, 0b100, _) => BLT { rs1, rs2, offset: b_imm },
        (Op::BRANCH, 0b101, _) => BGE { rs1, rs2, offset: b_imm },
        (Op::BRANCH, 0b110, _) => BLTU { rs1, rs2, offset: b_imm },
        (Op::BRANCH, 0b111, _) => BGEU { rs1, rs2, offset: b_imm },
        (Op::LOAD, 0b000, _) => LB { rd, rs1, offset: i_imm },
        (Op::LOAD, 0b001, _) => LH { rd, rs1, offset: i_imm },
        (Op::LOAD, 0b010, _) => LW { rd, rs1, offset: i_imm },
        (Op::LOAD, 0b100, _) => LBU { rd, rs1, offset: i_imm },
        (Op::LOAD, 0b101, _) => LHU { rd, rs1, offset: i_imm },
        (Op::STORE, 0b000, _) => SB { rs1, rs2, offset: s_imm },
        (Op::STORE, 0b001, _) => SH { rs1, rs2, offset: s_imm },
        (Op::STORE, 0b010, _) => SW { rs1, rs2, offset: s_imm },
        (Op::OP_IMM, 0b000, _) => ADDI { rd, rs1, imm: i_imm },
        (Op::OP_IMM, 0b010, _) => SLTI { rd, rs1, imm: i_imm },
        (Op::OP_IMM, 0b011, _) => SLTIU { rd, rs1, imm: i_imm },
        (Op::OP_IMM, 0b100, _) => XORI { rd, rs1, imm: i_imm },
        (Op::OP_IMM, 0b110, _) => ORI { rd, rs1, imm: i_imm },
        (Op::OP_IMM, 0b111, _) => ANDI { rd, rs1, imm: i_imm },
        (Op::OP, 0b000, 0b0000000) => ADD { rd, rs1, rs2 },
        (Op::OP, 0b000, 0b0100000) => SUB { rd, rs1, rs2 },
        (Op::OP, 0b001, 0b0000000) => SLL { rd, rs1, rs2 },
        (Op::OP, 0b010, 0b0000000) => SLT { rd, rs1, rs2 },
        (Op::OP, 0b011, 0b0000000) => SLTU { rd, rs1, rs2 },
        (Op::OP, 0b100, 0b0000000) => XOR { rd, rs1, rs2 },
        (Op::OP, 0b101, 0b0000000) => SRL { rd, rs1, rs2 },
        (Op::OP, 0b101, 0b0100000) => SRA { rd, rs1, rs2 },
        (Op::OP, 0b110, 0b0000000) => OR { rd, rs1, rs2 },
        (Op::OP, 0b111, 0b0000000) => AND { rd, rs1, rs2 },
        (Op::MISC_MEM, 0b000, _) if code >> 28 & 0b1111 == 0b000 => FENCE { succ, pred },
        (Op::SYSTEM, 0b000, 0b0000000) if rd == 0b0 && rs1 == 0b0 && rs2 == 0b0 => ECALL,
        (Op::SYSTEM, 0b000, 0b0000000) if rd == 0b0 && rs1 == 0b0 && rs2 == 0b1 => EBREAK,
        // RV32 base instruction set:
        (Op::OP_IMM, 0b001, 0b0000000) if rv32 => SLLI { rd, rs1, shamt },
        (Op::OP_IMM, 0b101, 0b0000000) if rv32 => SRLI { rd, rs1, shamt },
        (Op::OP_IMM, 0b101, 0b0100000) if rv32 => SRAI { rd, rs1, shamt },
        // RV64 base instruction set:
        (Op::LOAD, 0b110, _) if rv64 => LWU { rd, rs1, offset: i_imm },
        (Op::LOAD, 0b011, _) if rv64 => LD { rd, rs1, offset: i_imm },
        (Op::STORE, 0b011, _) if rv64 => SD { rs1, rs2, offset: s_imm },
        (Op::OP_IMM, 0b001, f7) if rv64 && f7 >> 1 == 0b000000 => SLLI { rd, rs1, shamt },
        (Op::OP_IMM, 0b101, f7) if rv64 && f7 >> 1 == 0b000000 => SRLI { rd, rs1, shamt },
        (Op::OP_IMM, 0b101, f7) if rv64 && f7 >> 1 == 0b010000 => SRAI { rd, rs1, shamt },
        (Op::OP_IMM_32, 0b000, _) if rv64 => ADDIW { rd, rs1, imm: i_imm },
        (Op::OP_IMM_32, 0b001, 0b0000000) if rv64 => SLLIW { rd, rs1, shamt: shamt32 },
        (Op::OP_IMM_32, 0b101, 0b0000000) if rv64 => SRLIW { rd, rs1, shamt: shamt32 },
        (Op::OP_IMM_32, 0b101, 0b0100000) if rv64 => SRAIW { rd, rs1, shamt: shamt32 },
        (Op::OP_32, 0b000, 0b0000000) if rv64 => ADDW { rd, rs1, rs2 },
        (Op::OP_32, 0b000, 0b0100000) if rv64 => SUBW { rd, rs1, rs2 },
        (Op::OP_32, 0b001, 0b0000000) if rv64 => SLLW { rd, rs1, rs2 },
        (Op::OP_32, 0b101, 0b0000000) if rv64 => SRLW { rd, rs1, rs2 },
        (Op::OP_32, 0b101, 0b0100000) if rv64 => SRAW { rd, rs1, rs2 },
        // Zifencei extension:
        (Op::MISC_MEM, 0b001, _) if ext_zifencei => FENCE_I,
        // M extension:
        (Op::OP, 0b000, 0b0000001) if ext_m => MUL { rd, rs1, rs2 },
        (Op::OP, 0b001, 0b0000001) if ext_m => MULH { rd, rs1, rs2 },
        (Op::OP, 0b010, 0b0000001) if ext_m => MULHSU { rd, rs1, rs2 },
        (Op::OP, 0b011, 0b0000001) if ext_m => MULHU { rd, rs1, rs2 },
        (Op::OP, 0b100, 0b0000001) if ext_m => DIV { rd, rs1, rs2 },
        (Op::OP, 0b101, 0b0000001) if ext_m => DIVU { rd, rs1, rs2 },
        (Op::OP, 0b110, 0b0000001) if ext_m => REM { rd, rs1, rs2 },
        (Op::OP, 0b111, 0b0000001) if ext_m => REMU { rd, rs1, rs2 },
        // RV64M extension:
        (Op::OP_32, 0b000, 0b0000001) if rv64 && ext_m => MULW { rd, rs1, rs2 },
        (Op::OP_32, 0b100, 0b0000001) if rv64 && ext_m => DIVW { rd, rs1, rs2 },
        (Op::OP_32, 0b101, 0b0000001) if rv64 && ext_m => DIVUW { rd, rs1, rs2 },
        (Op::OP_32, 0b110, 0b0000001) if rv64 && ext_m => REMW { rd, rs1, rs2 },
        (Op::OP_32, 0b111, 0b0000001) if rv64 && ext_m => REMUW { rd, rs1, rs2 },
        // F extension:
        (Op::LOAD_FP, 0b010, _) if ext_f => FLW { frd, rs1, offset: i_imm },
        (Op::STORE_FP, 0b010, _) if ext_f => FSW { rs1, frs2, offset: s_imm },
        (Op::MADD, _, f7) if ext_f && f7 & 0b11 == 0b00 => FMADD_S { frd, rm, frs1, frs2, frs3 },
        (Op::MSUB, _, f7) if ext_f && f7 & 0b11 == 0b00 => FMSUB_S { frd, rm, frs1, frs2, frs3 },
        (Op::NMSUB, _, f7) if ext_f && f7 & 0b11 == 0b00 => FNMSUB_S { frd, rm, frs1, frs2, frs3 },
        (Op::NMADD, _, f7) if ext_f && f7 & 0b11 == 0b00 => FNMADD_S { frd, rm, frs1, frs2, frs3 },
        (Op::OP_FP, _, 0b0000000) if ext_f => FADD_S { frd, rm, frs1, frs2 },
        (Op::OP_FP, 0b001, 0b1110000) if ext_f && rs2 == 0b00000 => FCLASS_S { rd, frs1 },
        (Op::OP_FP, _, 0b1101000) if ext_f && rs2 == 0b00000 => FCVT_S_W { frd, rm, rs1 },
        (Op::OP_FP, _, 0b1101000) if ext_f && rs2 == 0b00001 => FCVT_S_WU { frd, rm, rs1 },
        (Op::OP_FP, _, 0b1100000) if ext_f && rs2 == 0b00000 => FCVT_W_S { rd, rm, frs1 },
        (Op::OP_FP, _, 0b1100000) if ext_f && rs2 == 0b00001 => FCVT_WU_S { rd, rm, frs1 },
        (Op::OP_FP, _, 0b0001100) if ext_f => FDIV_S { frd, rm, frs1, frs2 },
        (Op::OP_FP, _, 0b0000100) if ext_f => FSUB_S { frd, rm, frs1, frs2 },
        (Op::OP_FP, _, 0b0001000) if ext_f => FMUL_S { frd, rm, frs1, frs2 },
        (Op::OP_FP, _, 0b0101100) if ext_f && rs2 == 0b00000 => FSQRT_S { frd, rm, frs1 },
        (Op::OP_FP, 0b000, 0b0010000) if ext_f => FSGNJ_S { frd, frs1, frs2 },
        (Op::OP_FP, 0b001, 0b0010000) if ext_f => FSGNJN_S { frd, frs1, frs2 },
        (Op::OP_FP, 0b010, 0b0010000) if ext_f => FSGNJX_S { frd, frs1, frs2 },
        (Op::OP_FP, 0b000, 0b0010100) if ext_f => FMIN_S { frd, frs1, frs2 },
        (Op::OP_FP, 0b001, 0b0010100) if ext_f => FMAX_S { frd, frs1, frs2 },
        (Op::OP_FP, 0b000, 0b1110000) if ext_f && rs2 == 0b00000 => FMV_X_W { rd, frs1 },
        (Op::OP_FP, 0b010, 0b1010000) if ext_f => FEQ_S { rd, frs1, frs2 },
        (Op::OP_FP, 0b001, 0b1010000) if ext_f => FLT_S { rd, frs1, frs2 },
        (Op::OP_FP, 0b000, 0b1010000) if ext_f => FLE_S { rd, frs1, frs2 },
        (Op::OP_FP, 0b000, 0b1111000) if ext_f && rs2 == 0b00000 => FMV_W_X { frd, rs1 },
        // RV64F extension:
        (Op::OP_FP, _, 0b1100000) if rv64 && ext_f && rs2 == 0b00010 => FCVT_L_S { rd, rm, frs1 },
        (Op::OP_FP, _, 0b1100000) if rv64 && ext_f && rs2 == 0b00011 => FCVT_LU_S { rd, rm, frs1 },
        (Op::OP_FP, _, 0b1101000) if rv64 && ext_f && rs2 == 0b00010 => FCVT_S_L { frd, rm, rs1 },
        (Op::OP_FP, _, 0b1101000) if rv64 && ext_f && rs2 == 0b00011 => FCVT_S_LU { frd, rm, rs1 },
        // Unknown:
        _ => UNIMP,
    }
}

/// Instruction opcodes (the lower seven bits of an instruction).
pub struct Op;
impl Op {
    pub const LOAD: u32 = 0x03;
    pub const LOAD_FP: u32 = 0x07;
    // custom-0
    pub const MISC_MEM: u32 = 0x0f;
    pub const OP_IMM: u32 = 0x13;
    pub const AUIPC: u32 = 0x17;
    pub const OP_IMM_32: u32 = 0x1b;
    // 48b
    pub const STORE: u32 = 0x23;
    pub const STORE_FP: u32 = 0x27;
    // custom-1
    pub const AMO: u32 = 0x2f;
    pub const OP: u32 = 0x33;
    pub const LUI: u32 = 0x37;
    pub const OP_32: u32 = 0x3b;
    // 64b
    pub const MADD: u32 = 0x43;
    pub const MSUB: u32 = 0x47;
    pub const NMSUB: u32 = 0x4b;
    pub const NMADD: u32 = 0x4f;
    pub const OP_FP: u32 = 0x53;
    // reserved
    // custom-2/rv128
    // 48b
    pub const BRANCH: u32 = 0x63;
    pub const JALR: u32 = 0x67;
    // reserved
    pub const JAL: u32 = 0x6f;
    pub const SYSTEM: u32 = 0x73;
    // reserved
    // custom-3/rv128
    // >= 80b
}

pub struct Encoding {
    pub opcode: u32,
    pub funct3: u32,
    pub funct7: u32,
    pub rd: u32,
    pub rs1: u32,
    pub rs2: u32,
    pub rs3: u32,
    pub rm: u32,
    pub uimm: u32,
    pub rl: u32,
    pub aq: u32,
    pub csr: u32,
    pub shamt32: u32,
    pub shamt64: u32,
    pub succ: Iorw,
    pub pred: Iorw,
    pub i_imm: i32,
    pub s_imm: i32,
    pub b_imm: i32,
    pub u_imm: u32,
    pub j_imm: i32,
}

impl From<u32> for Encoding {
    /// Extract encoded fields from an instruction.
    fn from(code: u32) -> Self {
        Encoding {
            opcode: code & 0b1111111,
            rd: code >> 7 & 0b11111,
            funct3: code >> 12 & 0b111,
            rs1: code >> 15 & 0b11111,
            rs2: code >> 20 & 0b11111,
            rs3: code >> 27 & 0b11111,
            rm: code >> 12 & 0b111,
            uimm: code >> 15 & 0b11111,
            rl: code >> 25 & 0b1,
            aq: code >> 26 & 0b1,
            csr: code >> 20 & 0b1111_1111_1111,
            funct7: code >> 25,
            shamt32: code >> 20 & 0b11111,
            shamt64: code >> 20 & 0b111111,
            succ: Iorw::from(code >> 20 & 0b1111),
            pred: Iorw::from(code >> 24 & 0b1111),
            i_imm: code as i32 >> 20,
            s_imm: (code as i32 >> 25) << 5 | code as i32 >> 7 & 0b11111,
            b_imm: (code as i32 >> 31) << 12
                | (code as i32 >> 25 & 0b111111) << 5
                | (code as i32 >> 8 & 0b1111) << 1
                | (code as i32 >> 7 & 0b1) << 11,
            u_imm: (code >> 12),
            j_imm: (code as i32 >> 31) << 20
                | (code as i32 >> 21 & 0b11_1111_1111) << 1
                | (code as i32 >> 20 & 0b1) << 11
                | (code as i32 >> 12 & 0b1111_1111) << 12,
        }
    }
}

impl Iorw {
    pub const I: Iorw = Iorw(0b1000);
    pub const O: Iorw = Iorw(0b0100);
    pub const R: Iorw = Iorw(0b0010);
    pub const W: Iorw = Iorw(0b0001);
}

impl std::ops::BitOr for Iorw {
    type Output = Iorw;

    fn bitor(self, rhs: Self) -> Self::Output {
        Iorw(self.0 | rhs.0)
    }
}

impl std::ops::BitAnd for Iorw {
    type Output = Iorw;

    fn bitand(self, rhs: Self) -> Self::Output {
        Iorw(self.0 & rhs.0)
    }
}

impl From<u8> for Iorw {
    fn from(value: u8) -> Self {
        Iorw(value)
    }
}

impl From<u32> for Iorw {
    fn from(value: u32) -> Self {
        Iorw(value as u8)
    }
}
