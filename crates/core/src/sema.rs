pub struct Resolver {

}

#[derive(Debug)]
pub enum SemanticError {
    TypeMismatch,
    VariableNotFound,
    ProcedureNotFound,
    AlreadyDeclared,
    InvalidArgumentsCount,
}

impl Resolver {
    pub fn new() {
        
    }
}
