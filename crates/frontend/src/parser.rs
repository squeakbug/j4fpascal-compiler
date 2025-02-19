use crate::lexer::Token;

pub struct NodeKind {
    pub name: String,
}

pub struct SymTab {
    pub symbols: Vec<String>,
}

pub struct AST {
    pub kind: NodeKind,
    pub children: Vec<Box<AST>>,
}

pub struct Parser {
    tokens: Vec<Token>,
}

impl Parser {
    pub fn new() -> Self {
        Parser {

        }
    }

    fn parse_expr(&mut self) -> Result<AST, ()> {

    }

    fn parse_term(&mut self) -> Result<AST, ()> {

    }

    fn parse_factor(&mut self) -> Result<AST, ()> {

    }

    pub fn parse(&mut self, tokens: Vec<Token>) -> Result<AST, ()> {
        todo!()
    }
}