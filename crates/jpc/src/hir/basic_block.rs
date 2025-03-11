use std::{cell::RefCell, fmt, rc::{Rc, Weak}};

#[derive(Debug, Clone)]
pub enum Type {
    Real,
    Integer,
    String,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Real => write!(f, "f64"),
            Type::Integer => write!(f, "i64"),
            Type::String => write!(f, "str"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Constant {
    Real(f64),
    Integer(i64),
    String(String),
}

impl fmt::Display for Constant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Real(v) => write!(f, "{}", v),
            Self::Integer(v) => write!(f, "{}", v),
            Self::String(v) => write!(f, "{}", v),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Var {
    pub name: String,
    pub type_: Type,
    pub use_count: usize,
}

#[derive(Debug, Clone)]
pub struct InstructionList {
    pub head: Option<Rc<Instruction>>,
    pub tail: Option<Rc<Instruction>>,
}

impl InstructionList {
    pub fn new() -> Self {
        InstructionList { head: None, tail: None }
    }
}

#[derive(Debug, Clone)]
pub struct Instruction {
    pub kind: InstructionKind,
    pub label: Option<String>,
}

#[derive(Debug, Clone)]
pub enum InstructionKind {
    // Control flow
    Def { func_name: String, },
    Jump { target: String },
    Branch { cond: Rc<Var>, target_true: String, target_false: String },
    Ret,
    RetVal { var: Rc<Var> },
    Call { func_name: String, args: Vec<usize> },

    // Memory access and adressing operations
    Store     { src: Rc<Var>, dst: usize },
    Load      { dst: Rc<Var>, src: usize },
    LoadConst { dst: Rc<Var>, num: usize },
    Assign    { dst: Rc<Var>, src: Rc<Var> },

    // Arithmetic operators
    Add { dst: Rc<Var>, src1: Rc<Var>, src2: Rc<Var> },
    Sub { dst: Rc<Var>, src1: Rc<Var>, src2: Rc<Var> },
    Mul { dst: Rc<Var>, src1: Rc<Var>, src2: Rc<Var> },
    Div { dst: Rc<Var>, src1: Rc<Var>, src2: Rc<Var> },
    Rem { dst: Rc<Var>, src1: Rc<Var>, src2: Rc<Var> },
    Negate { dst: Rc<Var>, src: Rc<Var> },

    // Logic operators
    And { dst: Rc<Var>, src1: Rc<Var>, src2: Rc<Var> },
    Or  { dst: Rc<Var>, src1: Rc<Var>, src2: Rc<Var> },
    Xor { dst: Rc<Var>, src1: Rc<Var>, src2: Rc<Var> },

    // Debug

    // SSA-related
    Icmp,
    Phi { var: Rc<Var> },
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use InstructionKind::*;

        match self.kind {
            // Control flow
            Def { ref func_name } => {
                write!(f, "def {}", func_name.clone())
            }
            Jump { ref target } => {
                write!(f, "j {}", target.clone())
            }
            Branch { ref cond, ref target_true, ref target_false } => {
                write!(
                    f,
                    "br {}, {}, {}",
                    cond.name, target_true.clone(), target_false.clone()
                )
            }
            Ret => {
                write!(f, "ret")
            }
            RetVal { ref var } => {
                write!(f, "ret {}", var.name)
            }
            Call { ref func_name, args: ref _args } => {
                write!(f, "call {}", func_name.clone()) 
            }
            // Memory access and adressing operations
            Store { ref src, dst } => {
                write!(f, "store {} %{}, %{}", src.type_, src.name, dst)
            }
            Load { ref dst, src } => {
                write!(
                    f,
                    "%{:<4} = load {}, %{}",
                    dst.name, dst.type_, src
                )
            }
            LoadConst { ref dst, ref num } => {
                write!(
                    f,
                    "%{:<4} = const {}, {}",
                    dst.name, dst.type_, num
                )
            }
            Assign { ref dst, ref src } => {
                write!(f, "{} = %{} %{}", dst.name, src.type_, src.name)
            }
            // Arithmetic operators
            Add { ref dst, ref src1, ref src2 } => {
                write!(
                    f,
                    "%{:<4} = add {} %{}, %{}",
                    dst.name, src1.type_, src1.name, src2.name
                )
            }
            Sub { ref dst, ref src1, ref src2 } => {
                write!(
                    f,
                    "%{:<4} = sub {} %{}, %{}",
                    dst.name, src1.type_, src1.name, src2.name
                )
            }
            Mul { ref dst, ref src1, ref src2 } => {
                write!(
                    f,
                    "%{:<4} = mul {} %{}, %{}",
                    dst.name, src1.type_, src1.name, src2.name
                )
            }
            Div { ref dst, ref src1, ref src2 } => {
                write!(
                    f,
                    "{:<4} = div {} %{}, %{}",
                    dst.name, src1.type_, src1.name, src2.name
                )
            }
            Rem { ref dst, ref src1, ref src2 } => {
                write!(
                    f,
                    "%{:<4} = rem {} %{}, %{}",
                    dst.name, src1.type_, src1.name, src2.name
                )
            }
            Negate { ref dst, ref src } => {
                write!(f, "%{:<4} = neg %{}", dst.name, src.name)
            }

            // Logic operators
            And { ref dst, ref src1, ref src2 } => {
                write!(
                    f,
                    "%{:<4} = and {} %{}, %{}",
                    dst.name, src1.type_, src1.name, src2.name
                )
            }
            Or { ref dst, ref src1, ref src2 } => {
                write!(
                    f,
                    "%{:<4} = or {} %{}, %{}",
                    dst.name, src1.type_, src1.name, src2.name
                )
            }
            Xor { ref dst, ref src1, ref src2 } => {
                write!(
                    f,
                    "%{:<4} = xor {} %{}, %{}",
                    dst.name, src1.type_, src1.name, src2.name
                )
            }
            // Other operators
            Icmp => write!(f, "icmp"),
            Phi { ref var } => write!(f, "phi {}", var.name.clone()),
        }
    }
}

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub label: Option<String>,
    pub instructions: Vec<Instruction>,
    pub predecessors: Vec<Rc<BasicBlock>>,
    pub next: Option<Rc<BasicBlock>>,
    pub if_true: Option<Rc<BasicBlock>>,
    pub if_false: Option<Rc<BasicBlock>>,

    pub belong_to: Weak<Function>,
}

impl fmt::Display for BasicBlock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Ok(())
    }
}

impl BasicBlock {
    pub fn new() -> Self {
        BasicBlock {
            label: None,
            instructions: Vec::new(),
            predecessors: Vec::new(),
            next: None,
            if_true: None,
            if_false: None,
            belong_to: Weak::new(),
        }
    }

    pub fn add_instruction(&mut self, inst: InstructionKind) {
        let inst = Instruction {
            kind: inst,
            label: None,
        };
        self.instructions.push(inst);
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub params: Vec<Var>,
    pub return_var: Option<Var>,

    pub root_block: Rc<RefCell<BasicBlock>>,
    pub exit_block: Option<Rc<BasicBlock>>,
    pub constants: Vec<Constant>,
    pub variables: Vec<Var>,

    pub belongs_to: Weak<Module>,
}

#[derive(Debug, Clone)]
pub struct Module {
    functions: Vec<Function>,
    variables: Vec<Var>,
    constants: Vec<Constant>,
}

impl Module {
    pub fn new() -> Self {
        Module {
            functions: vec![],
            constants: vec![],
            variables: vec![],
        }
    }

    pub fn get_var_by_num(&self, num: usize) -> Var {
        self.variables[num].clone()
    }

    pub fn get_const_by_num(&self, num: usize) -> Constant {
        self.constants[num].clone()
    }
}
