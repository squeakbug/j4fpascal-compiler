use crate::lexer::Token;

#[derive(Debug, Clone)]
pub enum DesignatorItem {
    Generic,
    Field {
        name: Token,
    },
    ArrayAccess {
        indexes: Vec<Box<Expr>>,
    },
    Call {
        arguments: Vec<(Box<Expr>, Vec<Box<Expr>>)>,
    },
}

#[derive(Debug, Clone)]
pub struct Designator {
    pub name: String,
    pub items: Vec<DesignatorItem>,
}

// This syntax tree is less complex, than parsing tree (that i don't form)
#[derive(Debug, Clone)]
pub enum Expr {
    Binary {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },
    Unary {
        operator: Token,
        right: Box<Expr>,
    },
    Literal {
        value: Token,
    },
    Designator {
        designator: Designator,
    },
}

#[derive(Debug, Clone)]
pub enum CaseLabel {
    Simple(Box<Expr>),
    Range((Box<Expr>, Box<Expr>)),
}

#[derive(Debug, Clone)]
pub struct CaseItem {
    pub labels: Vec<CaseLabel>,
    pub statement: Box<Stmt>,
}

#[derive(Debug, Clone)]
pub struct Stmt {
    pub label: Option<Token>,
    pub statement: Box<UnlabeledStmt>,
}

impl From<UnlabeledStmt> for Stmt {
    fn from(value: UnlabeledStmt) -> Self {
        Stmt {
            label: None,
            statement: Box::new(value),
        }
    }
}

#[derive(Debug, Clone)]
pub enum UnlabeledStmt {
    Assigment {
        left: Designator,
        right: Box<Expr>,
    },
    ProcedureCall {
        designator: Designator,
    },
    Compound {
        statements: Vec<Box<Stmt>>,
    },
    If {
        condition: Box<Expr>,
        then_branch: Box<Stmt>,
        else_branch: Option<Box<Stmt>>,
    },
    Case {
        condition: Box<Expr>,
        case_items: Vec<Box<CaseItem>>,
        else_branch: Option<Box<Stmt>>,
    },
    Repeat {
        statements: Vec<Box<Stmt>>,
        condition: Box<Expr>,
    },
    While {
        condition: Box<Expr>,
        statement: Box<Stmt>,
    },
    For {
        var: Box<Designator>,
        init: Box<Expr>,
        to: Box<Expr>,
        statement: Box<Stmt>,
        is_down_to: bool,
    },
    Goto {
        label: Token,
    },
    Break,
    Continue,
}

#[derive(Debug, Clone)]
pub enum TypeDeclaration {
    ArrayType {

    },
    RecordType {

    },
    SetType {

    },
    FileType {

    },
    ScalarType {
        identifiers: Vec<String>,
    },
    SubrangeType {
        from: Box<Token>,
        to: Box<Token>,
    },
}

#[derive(Debug, Clone)]
pub struct VarDeclaration {
    pub name: String,
    pub var_type: String,
    // Maybe must be annotated during semantic pass
    pub init_value: Option<Box<Expr>>,
}

#[derive(Debug, Clone)]
pub struct ProcedureHeadDeclaration {
    pub name: String,
    pub params: Vec<(String, Option<String>, Option<Box<Expr>>)>,
    pub return_type: Option<TypeDeclaration>,
}

#[derive(Debug, Clone)]
pub struct ProcedureDeclaration {
    pub head: ProcedureHeadDeclaration,
    pub body: Box<Stmt>,
}

#[derive(Debug, Clone)]
pub enum DeclSection {
    Type (Box<TypeDeclaration>),
    Variable (Box<Vec<VarDeclaration>>),
    Procedure (Box<ProcedureDeclaration>),
}

#[derive(Debug, Clone)]
pub struct Block {
    pub decl_sections: Vec<DeclSection>,
    pub body: Box<Stmt>,
}

#[derive(Debug, Clone)]
pub struct Program {
    pub head: Option<(String, Vec<String>)>,
    pub block: Box<Block>,
}
