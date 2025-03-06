use core::ast::ProcedureDeclaration;
use std::rc::Rc;

use super::codegen::Instruction;

pub struct IRBuilder {}

pub struct BasicBlock {
    pub label: String,
    pub instructions: Vec<Instruction>,
    pub successors: Vec<Rc<BasicBlock>>,
    pub predecessors: Vec<Rc<BasicBlock>>,
}

impl BasicBlock {
    pub fn new() -> Self {
        BasicBlock {
            label: String::new(),
            instructions: Vec::new(),
            successors: Vec::new(),
            predecessors: Vec::new(),
        }
    }
}

pub struct ProcedureIR {
    pub name: String,
    pub entry_block: Rc<BasicBlock>,
    pub blocks: Vec<Rc<BasicBlock>>,
}

impl IRBuilder {
    pub fn new() -> Self {
        IRBuilder {}
    }

    pub fn build(func_decl: ProcedureDeclaration) -> ProcedureIR {
        let entry_block = Rc::new(BasicBlock::new());
        let blocks = vec![Rc::clone(&entry_block)];

        // TODO: generate IR for function body

        ProcedureIR {
            name: func_decl.head.name.clone(),
            entry_block: Rc::clone(&entry_block),
            blocks,
        }
    }
}
