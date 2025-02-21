use crate::lexer::Token;

pub struct NodeKind {
    pub name: String,
}

pub struct AST {
    pub kind: NodeKind,
    pub children: Vec<Box<AST>>,
}

pub struct Parser {
    istream: Vec<Token>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser {
            istream: tokens
        }
    }

    fn parse_expr(&mut self) -> Result<AST, ()> {
        todo!()
    }

    fn parse_term(&mut self) -> Result<AST, ()> {
        todo!()
    }

    fn parse_factor(&mut self) -> Result<AST, ()> {
        todo!()
    }

    pub fn parse(&mut self, _tokens: Vec<Token>) -> Result<AST, ()> {
        todo!()
    }
}
