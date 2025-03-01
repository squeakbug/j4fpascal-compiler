use std::rc::Rc;

/* IR opcode */
pub enum IRInstruction {
    /* intermediate use in front-end. No code generation */
    Generic,

    Store,
    Load,

    /* arithmetic operators */
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    LogAnd,
    LogOr,
    LogNot,
    Eq,
    Neq,
    Lt,
    Leq,
    Gt,
    Geq,
    Negate,

    Start
}

pub struct IRBuilder {

}

pub struct BasicBlock {
    pub label: String,
    pub instructions: Vec<IRInstruction>,
    pub successors: Vec<Rc<BasicBlock>>,
    pub predecessors: Vec<Rc<BasicBlock>>,
}

impl IRBuilder {
    pub fn new() -> Self {
        IRBuilder {
            
        }
    }
}
