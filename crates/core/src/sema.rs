use crate::lexer::SrcSpan;

pub struct Resolver {}

#[derive(Debug, Clone)]
pub struct SemanticError {
    pub location: SrcSpan,
    pub kind: SemanticErrorType,
}

#[derive(Debug, Clone)]
pub enum SemanticErrorType {
    TypeMismatch,
    VariableNotFound,
    ProcedureNotFound,
    AlreadyDeclared,
    InvalidArgumentsCount,
}

impl SemanticError {
    pub fn details(&self) -> (String, Vec<String>) {
        match &self.kind {
            SemanticErrorType::TypeMismatch => ("Type mismatch".into(), vec![]),
            SemanticErrorType::VariableNotFound => ("Variable not found".into(), vec![]),
            SemanticErrorType::ProcedureNotFound => ("Procedure not found".into(), vec![]),
            SemanticErrorType::AlreadyDeclared => ("Already declared".into(), vec![]),
            SemanticErrorType::InvalidArgumentsCount => ("Invalid arguments count".into(), vec![]),
        }
    }
}

impl Resolver {
    pub fn new() {}
}
