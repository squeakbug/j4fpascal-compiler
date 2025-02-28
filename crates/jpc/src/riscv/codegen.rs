use std::{collections::HashMap, fmt};

use core::{
    ast::{
        Block, DeclSection, DesignatorItem, Expr, 
        ProcedureDeclaration, Stmt, TypeDeclaration, 
        UnlabeledStmt, VarDeclaration
    },
    lexer::{self, Lexer}, 
    parser::Parser
};

pub struct CodeEmmiter {
    
}

impl CodeEmmiter {
    pub fn new() -> Self {
        CodeEmmiter {
            
        }
    }
}

#[derive(Debug)]
pub enum CodegenError {
    NotImplemented
}

impl CodeEmmiter {
    pub fn visit_statement(&mut self, stmt: &Box<Stmt>) -> Result<Vec<String>, CodegenError> {
        match stmt.statement.as_ref() {
            _ => Err(CodegenError::NotImplemented),
        }
    }

    fn visit_expr(&mut self, expr: &Box<Expr>) -> Result<Vec<String>, CodegenError> {
        match expr.as_ref() {
            _ => Err(CodegenError::NotImplemented),
        }
    }

    fn visit_block(&mut self, block: &Box<Block>) -> Result<Vec<String>, CodegenError> {
        self.visit_statement(&block.body)
    }

    pub fn codegen(&mut self, exp: &str) -> Result<Vec<String>, CodegenError> {
        todo!()
    }
}
