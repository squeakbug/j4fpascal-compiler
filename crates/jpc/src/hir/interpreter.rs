use std::{fmt::Debug, vec};

use super::basic_block::{Constant, Instruction, InstructionKind, Module};

#[derive(Debug, Clone)]
pub enum InterpreterError {
    NotImplemented,
}

#[derive(Debug)]
pub struct Interpreter {
    imem: Vec<Instruction>,
    constants: Vec<Constant>,
    stack: Vec<u64>,
    ip: usize,
}

impl Interpreter {
    pub fn new(module: Module) -> Self {
        Interpreter {
            imem: vec![],
            constants: vec![],
            stack: vec![],
            ip: 0,
        }
    }

    pub fn exec(&mut self, inst: Instruction) {
        use InstructionKind::*;
        match inst.kind {
            // Memory access and adressing operations
            Store { src, dst } => {}
            Load { dst, src } => {}
            LoadConst { dst, num } => {}
            // Arithmetic operators
            Add { dst, src1, src2 } => {}
            Sub { dst, src1, src2 } => {}
            Mul { dst, src1, src2 } => {}
            Div { dst, src1, src2 } => {}
            Rem { dst, src1, src2 } => {}
            Negate { dst, src } => {}

            // Logic operators
            And { dst, src1, src2 } => {}
            Or { dst, src1, src2 } => {}
            Xor { dst, src1, src2 } => {}
            // Other operators
            Icmp => {}
            Phi => {}
        }
    }

    pub fn step(&mut self) {
        let inst = self.imem[self.ip].clone();
        self.exec(inst);
        self.ip += 1;
    }

    pub fn eval(&mut self) -> Result<(), InterpreterError> {
        loop {
            self.step();
        }
    }
}
