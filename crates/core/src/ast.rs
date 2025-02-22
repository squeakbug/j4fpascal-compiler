use crate::lexer::Token;

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
    Grouping {
        expression: Box<Expr>,
    },
    Variable {
        var: Token,
    },
    Literal {
        value: Token,
    },
}

#[derive(Debug)]
pub enum Stmt {
    Labeled {
        label: Token,
        statement: Box<Stmt>,
    },
    Assigment {
        left: Token,
        right: Box<Expr>,
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
        condition: Box<Stmt>,
        cases: Vec<(Box<Expr>, Box<Stmt>)>,
    },
    Repetetive {
        condition: Box<Expr>,
        body: Box<Stmt>,
    },
    Procedure,
    Goto {
        label: Token,
    },
    Empty,
}
