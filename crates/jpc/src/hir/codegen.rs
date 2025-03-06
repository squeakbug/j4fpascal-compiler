use std::{
    collections::HashMap,
    fmt, fs,
    io::{BufWriter, Write},
};

use core::{
    ast::{
        Block, DeclSection, DesignatorItem, Expr, ProcedureDeclaration, Program, Stmt,
        TypeDeclaration, UnlabeledStmt, VarDeclaration,
    },
    lexer::{self, Lexer},
    parser::{Parser, ParserError},
    sema::SemanticError,
};

#[derive(Debug)]
pub enum CodegenError {
    NotImplemented,
    UndefinedVariable,
    AssigmentToNotDefinedVariable,
    NoParentEnvironment,
    NotCallable,
    MismatchedTypes,
    MismathedArgumentsCount,
    AlreadyDefined,
}

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
    pub number: usize,
    pub use_count: usize,
    pub type_: Type,
}

#[derive(Debug, Clone)]
pub enum Instruction {
    // Memory access and adressing operations
    Store { src: Var, dst: Var },
    Load { dst: Var, src: Var },
    LoadConst { dst: Var, num: usize },

    // Arithmetic operators
    Add { dst: Var, src1: Var, src2: Var },
    Sub { dst: Var, src1: Var, src2: Var },
    Mul { dst: Var, src1: Var, src2: Var },
    Div { dst: Var, src1: Var, src2: Var },
    Rem { dst: Var, src1: Var, src2: Var },
    Negate { dst: Var, src: Var },

    // Login operators
    And { dst: Var, src1: Var, src2: Var },
    Or { dst: Var, src1: Var, src2: Var },
    Xor { dst: Var, src1: Var, src2: Var },

    // Debug

    // Other operators
    Icmp,
    Phi,
}

#[derive(Debug, Clone)]
pub struct Module {
    pub instructions: Vec<Instruction>,
    pub constants: Vec<Constant>,
}

pub struct CodeEmmiter {
    pub instructions: Vec<Instruction>,
    pub number: usize,
    pub constants: Vec<Constant>,
}

impl CodeEmmiter {
    pub fn new() -> Self {
        CodeEmmiter {
            instructions: vec![],
            number: 0,
            constants: vec![],
        }
    }

    fn add_const(&mut self, constant: Constant) -> usize {
        let num = self.constants.len();
        self.constants.push(constant);
        num
    }

    fn add_var(&mut self, type_: Type) -> Var {
        self.number += 1;
        Var {
            number: self.number,
            use_count: 0,
            type_,
        }
    }

    fn visit_expr(&mut self, expr: &Box<Expr>) -> Result<Var, CodegenError> {
        match expr.as_ref() {
            Expr::Binary {
                left,
                operator,
                right,
            } => {
                let src1 = self.visit_expr(left)?;
                let src2 = self.visit_expr(right)?;
                let dst = self.add_var(src1.type_.clone());
                let inst = match operator.kind {
                    lexer::TokenType::Plus => Ok(Instruction::Add {
                        dst: dst.clone(),
                        src1,
                        src2,
                    }),
                    lexer::TokenType::Minus => Ok(Instruction::Sub {
                        dst: dst.clone(),
                        src1,
                        src2,
                    }),
                    lexer::TokenType::Star => Ok(Instruction::Mul {
                        dst: dst.clone(),
                        src1,
                        src2,
                    }),
                    lexer::TokenType::Slash => Ok(Instruction::Div {
                        dst: dst.clone(),
                        src1,
                        src2,
                    }),
                    _ => Err(CodegenError::NotImplemented),
                }?;
                self.instructions.push(inst);
                Ok(dst)
            }
            Expr::Unary { operator, right } => {
                let src = self.visit_expr(right)?;
                match operator.kind {
                    lexer::TokenType::Minus => {
                        let dst = self.add_var(src.type_.clone());
                        let inst = Instruction::Negate {
                            dst: dst.clone(),
                            src,
                        };
                        self.instructions.push(inst);
                        Ok(dst)
                    }
                    _ => Err(CodegenError::NotImplemented),
                }
            }
            Expr::Literal { value } => match value.kind {
                lexer::TokenType::UnsignedInteger(ref num) => match num.parse::<i64>() {
                    Ok(num) => {
                        let dst = self.add_var(Type::Integer);
                        let num = self.add_const(Constant::Integer(num));
                        let inst = Instruction::LoadConst {
                            dst: dst.clone(),
                            num,
                        };
                        self.instructions.push(inst);
                        Ok(dst)
                    }
                    Err(_) => Err(CodegenError::NotImplemented),
                },
                lexer::TokenType::UnsignedReal(ref num) => match num.parse::<f64>() {
                    Ok(num) => {
                        let dst = self.add_var(Type::Integer);
                        let num = self.add_const(Constant::Real(num));
                        let inst = Instruction::LoadConst {
                            dst: dst.clone(),
                            num,
                        };
                        self.instructions.push(inst);
                        Ok(dst)
                    }
                    Err(_) => Err(CodegenError::NotImplemented),
                },
                lexer::TokenType::True => {
                    let dst = self.add_var(Type::Integer);
                    let num = self.add_const(Constant::Integer(1));
                    let inst = Instruction::LoadConst {
                        dst: dst.clone(),
                        num,
                    };
                    self.instructions.push(inst);
                    Ok(dst)
                }
                lexer::TokenType::False => {
                    let dst = self.add_var(Type::Integer);
                    let num = self.add_const(Constant::Integer(0));
                    let inst = Instruction::LoadConst {
                        dst: dst.clone(),
                        num,
                    };
                    self.instructions.push(inst);
                    Ok(dst)
                }
                lexer::TokenType::Nil => {
                    let dst = self.add_var(Type::Integer);
                    let num = self.add_const(Constant::Integer(0));
                    let inst = Instruction::LoadConst {
                        dst: dst.clone(),
                        num,
                    };
                    self.instructions.push(inst);
                    Ok(dst)
                }
                lexer::TokenType::StringLiteral(ref val) => {
                    let dst = self.add_var(Type::String);
                    let num = self.add_const(Constant::String(val.clone()));
                    let inst = Instruction::LoadConst {
                        dst: dst.clone(),
                        num,
                    };
                    self.instructions.push(inst);
                    Ok(dst)
                }
                _ => Err(CodegenError::NotImplemented),
            },
            Expr::Designator { designator } => Err(CodegenError::UndefinedVariable),
        }
    }

    fn visit_type_declaration(&mut self, _decl: &TypeDeclaration) -> Result<(), CodegenError> {
        Ok(())
    }

    fn visit_var_declaration(&mut self, decl: &VarDeclaration) -> Result<(), CodegenError> {
        Ok(())
    }

    fn visit_procedure_declaration(
        &mut self,
        decl: &ProcedureDeclaration,
    ) -> Result<(), CodegenError> {
        Ok(())
    }

    fn visit_label_declaration(&mut self, decl: &str) -> Result<(), CodegenError> {
        Ok(())
    }

    fn visit_const_declaration(&mut self, decl: &(String, Box<Expr>)) -> Result<(), CodegenError> {
        Ok(())
    }

    fn visit_decl_section(&mut self, section: &DeclSection) -> Result<(), CodegenError> {
        match section {
            DeclSection::Label(label_decl) => {
                for decl in label_decl.iter() {
                    self.visit_label_declaration(decl)?;
                }
                Ok(())
            }
            DeclSection::Const(const_decl) => {
                for decl in const_decl.iter() {
                    self.visit_const_declaration(decl)?;
                }
                Ok(())
            }
            DeclSection::Type(type_decl) => {
                for decl in type_decl.iter() {
                    self.visit_type_declaration(decl)?;
                }
                Ok(())
            }
            DeclSection::Variable(var_decl) => {
                for decl in var_decl.iter() {
                    self.visit_var_declaration(decl)?;
                }
                Ok(())
            }
            DeclSection::Procedure(proc_decl) => {
                self.visit_procedure_declaration(proc_decl)?;
                Ok(())
            }
        }
    }

    fn visit_statement(&mut self, stmt: &Box<Stmt>) -> Result<(), CodegenError> {
        match stmt.statement.as_ref() {
            UnlabeledStmt::Assigment { left, right } => {
                let _ident = &left.name;
                let _value = self.visit_expr(right)?;
                Ok(())
            }
            UnlabeledStmt::Compound { statements } => {
                for statement in statements {
                    self.visit_statement(statement)?;
                }
                Ok(())
            }
            _ => Ok(()),
        }
    }

    fn visit_block(&mut self, block: &Box<Block>) -> Result<(), CodegenError> {
        for section in block.decl_sections.iter() {
            self.visit_decl_section(section)?;
        }
        self.visit_statement(&block.body)
    }

    fn dump_inst(&self, f: &mut BufWriter<fs::File>, inst: &Instruction) -> std::io::Result<()> {
        use Instruction::*;

        match inst {
            // Memory access and adressing operations
            Store { src, dst } => {
                write!(f, "store {} %{}, %{}", src.type_, src.number, dst.number)
            }
            Load { dst, src } => {
                write!(
                    f,
                    "%{:<4} = load {}, %{}",
                    dst.number, dst.type_, src.number
                )
            }
            LoadConst { dst, num } => {
                write!(
                    f,
                    "%{:<4} = const {}, {}",
                    dst.number, dst.type_, self.constants[*num]
                )
            }
            // Arithmetic operators
            Add { dst, src1, src2 } => {
                write!(
                    f,
                    "%{:<4} = add {} %{}, %{}",
                    dst.number, src1.type_, src1.number, src2.number
                )
            }
            Sub { dst, src1, src2 } => {
                write!(
                    f,
                    "%{:<4} = sub {} %{}, %{}",
                    dst.number, src1.type_, src1.number, src2.number
                )
            }
            Mul { dst, src1, src2 } => {
                write!(
                    f,
                    "%{:<4} = mul {} %{}, %{}",
                    dst.number, src1.type_, src1.number, src2.number
                )
            }
            Div { dst, src1, src2 } => {
                write!(
                    f,
                    "{:<4} = div {} %{}, %{}",
                    dst.number, src1.type_, src1.number, src2.number
                )
            }
            Rem { dst, src1, src2 } => {
                write!(
                    f,
                    "%{:<4} = rem {} %{}, %{}",
                    dst.number, src1.type_, src1.number, src2.number
                )
            }
            Negate { dst, src } => {
                write!(f, "%{:<4} = neg %{}", dst.number, src.number)
            }

            // Logic operators
            And { dst, src1, src2 } => {
                write!(
                    f,
                    "%{:<4} = and {} %{}, %{}",
                    dst.number, src1.type_, src1.number, src2.number
                )
            }
            Or { dst, src1, src2 } => {
                write!(
                    f,
                    "%{:<4} = or {} %{}, %{}",
                    dst.number, src1.type_, src1.number, src2.number
                )
            }
            Xor { dst, src1, src2 } => {
                write!(
                    f,
                    "%{:<4} = xor {} %{}, %{}",
                    dst.number, src1.type_, src1.number, src2.number
                )
            }
            // Other operators
            Icmp => write!(f, "icmp"),
            Phi => write!(f, "phi"),
        }
    }

    pub fn dump(&self, f: &mut BufWriter<fs::File>) -> std::io::Result<()> {
        for inst in self.instructions.iter() {
            self.dump_inst(f, inst)?;
            writeln!(f)?;
        }
        Ok(())
    }

    pub fn visit_program(&mut self, program: &Box<Program>) -> Result<(), CodegenError> {
        self.visit_block(&program.block)
    }
}
