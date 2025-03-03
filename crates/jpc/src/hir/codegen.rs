use std::{collections::HashMap, fmt::{self, Debug}, fs};

use core::{
    ast::{
        Block, DeclSection, DesignatorItem, Expr, ProcedureDeclaration, 
        Program, Stmt, TypeDeclaration, UnlabeledStmt, VarDeclaration,
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
pub struct Var {
    pub number: usize,
    pub use_count: usize,
}

#[derive(Debug, Clone)]
pub enum Instruction {
    // Memory
    Store,
    Load,
    LoadConstant { dst: Var, const_num: usize },

    // Arithmetic operators
    Add { dst: Var, src1: Var, src2: Var },
    Sub { dst: Var, src1: Var, src2: Var },
    Mul { dst: Var, src1: Var, src2: Var },
    Div { dst: Var, src1: Var, src2: Var },
    Mod { dst: Var, src1: Var, src2: Var },
    Negate { dst: Var, src: Var },

    // Login operators
    LogAnd { dst: Var, src1: Var, src2: Var },
    LogOr { dst: Var, src1: Var, src2: Var },
    LogNot { dst: Var, src1: Var, src2: Var },
    Eq { dst: Var, src1: Var, src2: Var },
    Neq { dst: Var, src1: Var, src2: Var },
    Lt { dst: Var, src1: Var, src2: Var },
    Leq { dst: Var, src1: Var, src2: Var },
    Gt { dst: Var, src1: Var, src2: Var },
    Geq { dst: Var, src1: Var, src2: Var },
}

#[derive(Debug, Clone)]
pub enum Constant {
    Real(f64),
    Integer(i64),
    String(String),
    Boolean(bool),
    Null,
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

    fn add_var(&mut self, ) -> Var {
        self.number += 1;
        Var { number: self.number, use_count: 0 }
    }

    fn visit_expr(&mut self, expr: &Box<Expr>) -> Result<Var, CodegenError> {
        match expr.as_ref() {
            Expr::Binary { left, operator, right } => {
                let src1 = self.visit_expr(left)?;
                let src2 = self.visit_expr(right)?;
                let dst = self.add_var();
                let inst = match operator.kind {
                    lexer::TokenType::Plus => Ok(Instruction::Add { dst: dst.clone(), src1, src2 }),
                    lexer::TokenType::Minus => Ok(Instruction::Sub { dst: dst.clone(), src1, src2 }),
                    lexer::TokenType::Star => Ok(Instruction::Mul { dst: dst.clone(), src1, src2 }),
                    lexer::TokenType::Slash => Ok(Instruction::Div { dst: dst.clone(), src1, src2 }),
                    _ => Err(CodegenError::NotImplemented),
                }?;
                self.instructions.push(inst);
                Ok(dst)
            },
            Expr::Unary { operator, right } => {
                let src = self.visit_expr(right)?;
                match operator.kind {
                    lexer::TokenType::Minus => {
                        let dst = self.add_var();
                        let inst = Instruction::Negate { dst: dst.clone(), src };
                        self.instructions.push(inst);
                        Ok(dst)
                    },
                    _ => Err(CodegenError::NotImplemented),
                }
            },
            Expr::Literal { value } => {
                match value.kind {
                    lexer::TokenType::UnsignedInteger(ref num) => {
                        match num.parse::<i64>() {
                            Ok(num) => {
                                let dst = self.add_var();
                                let const_num = self.add_const(Constant::Integer(num));
                                let inst = Instruction::LoadConstant { 
                                    dst: dst.clone(), 
                                    const_num
                                };
                                self.instructions.push(inst);
                                Ok(dst)
                            },
                            Err(_) => Err(CodegenError::NotImplemented),
                        }
                    },
                    lexer::TokenType::UnsignedReal(ref num) => {
                        match num.parse::<f64>() {
                            Ok(num) => {
                                let dst = self.add_var();
                                let const_num = self.add_const(Constant::Real(num));
                                let inst = Instruction::LoadConstant { dst: dst.clone(), const_num };
                                self.instructions.push(inst);
                                Ok(dst)
                            },
                            Err(_) => Err(CodegenError::NotImplemented),
                        }
                    },
                    lexer::TokenType::True => {
                        let dst = self.add_var();
                        let const_num = self.add_const(Constant::Boolean(true));
                        let inst = Instruction::LoadConstant { dst: dst.clone(), const_num };
                        self.instructions.push(inst);
                        Ok(dst)
                    },
                    lexer::TokenType::False => {
                        let dst = self.add_var();
                        let const_num = self.add_const(Constant::Boolean(false));
                        let inst = Instruction::LoadConstant { dst: dst.clone(), const_num };
                        self.instructions.push(inst);
                        Ok(dst)
                    },
                    lexer::TokenType::Nil => {
                        let dst = self.add_var();
                        let const_num = self.add_const(Constant::Null);
                        let inst = Instruction::LoadConstant { dst: dst.clone(), const_num };
                        self.instructions.push(inst);
                        Ok(dst)
                    },
                    lexer::TokenType::StringLiteral(ref val) => {
                        let dst = self.add_var();
                        let const_num = self.add_const(Constant::String(val.clone()));
                        let inst = Instruction::LoadConstant { dst: dst.clone(), const_num };
                        self.instructions.push(inst);
                        Ok(dst)
                    },
                    _ => Err(CodegenError::NotImplemented),
                }
            },
            Expr::Designator { designator } => Err(CodegenError::UndefinedVariable)
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
        decl: &Box<ProcedureDeclaration>
    ) -> Result<(), CodegenError> {
        Ok(())
    }

    fn visit_decl_section(&mut self, section: &DeclSection) -> Result<(), CodegenError> {
        match section {
            DeclSection::Type(type_decl) => {
                self.visit_type_declaration(type_decl)?;
                Ok(())
            },
            DeclSection::Variable(var_decl) => {
                for decl in var_decl.iter() {
                    self.visit_var_declaration(decl)?;
                }
                Ok(())
            },
            DeclSection::Procedure(proc_decl) => {
                self.visit_procedure_declaration(proc_decl)?;
                Ok(())
            },
        }
    }

    fn visit_statement(&mut self, stmt: &Box<Stmt>) -> Result<(), CodegenError> {
        match stmt.statement.as_ref() {
            UnlabeledStmt::Assigment { left, right } => {
                let _ident = &left.name;
                let _value = self.visit_expr(right)?;
                Ok(())
            },
            _ => Ok(())
        }
    }

    fn visit_block(&mut self, block: &Box<Block>) -> Result<(), CodegenError> {
        for section in block.decl_sections.iter() {
            self.visit_decl_section(section)?;
        }
        self.visit_statement(&block.body)
    }

    pub fn visit_program(&mut self, program: &Box<Program>) -> Result<(), CodegenError> {
        self.visit_block(&program.block)
    }
}