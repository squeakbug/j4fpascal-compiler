use crate::lexer::Token;

#[derive(Debug)]
pub enum DesignatorItem {
    Generic,
    Field {
        name: Token,
    },
    ArrayAccess {
        indexes: Vec<Box<Expr>>,
    },
    Call {
        parameters: Vec<(Box<Expr>, Vec<Box<Expr>>)>,
    },
}

#[derive(Debug)]
pub struct Designator {
    pub name: Option<Token>,
    pub items: Vec<DesignatorItem>,
}

// This syntax tree is less complex, than parsing tree (that i don't form)
#[derive(Debug)]
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

#[derive(Debug)]
pub enum CaseLabel {
    Simple(Box<Expr>),
    Range((Box<Expr>, Box<Expr>)),
}

#[derive(Debug)]
pub struct CaseItem {
    pub labels: Vec<CaseLabel>,
    pub statement: Box<Stmt>,
}

#[derive(Debug)]
pub enum Stmt {
    Labeled {
        label: Token,
        statement: Box<Stmt>,
    },
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
        var: Box<Expr>,
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
    Empty,
}

#[derive(Debug)]
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

#[derive(Debug)]
pub enum Declaration {
    Type(TypeDeclaration),
    Variable {
        name: Token,
        var_type: TypeDeclaration,
    },
    Procedure {
        name: Token,
        parameters: Vec<Token>,
        return_type: Option<TypeDeclaration>,
        body: Block,
    },
}

#[derive(Debug)]
pub struct Block {
    pub decl_sections: Vec<Declaration>,
    pub body: Box<Stmt>,
}
