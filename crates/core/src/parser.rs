use thiserror::Error;

use crate::{
    ast::Expr,
    lexer::{Token, TokenType},
};

pub struct Parser {
    istream: Option<Vec<Token>>,
    current: usize,
}

#[derive(Error, Debug)]
pub enum ParserError {
    #[error("Unexpected character '{0}' at position {1}")]
    UnexpectedCharacter(char, usize),

    #[error("Invalid number format at position {0}")]
    InvalidNumberFormat(usize),

    #[error("Unterminated string literal at position {0}")]
    UnterminatedStringLiteral(usize),

    #[error("Unknown token at position {0}")]
    UnknownToken(usize),

    #[error("Unexpected EOF at position {0}")]
    UnexpectedEOF(usize),
}

///
/// expression ::= equality ;
/// equality   ::= comparison ( ( "!=" | "==" ) comparison )* ;
/// comparison ::= term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
/// term       ::= factor ( ( "-" | "+" ) factor )* ;
/// factor     ::= unary ( ( "/" | "*" ) unary )* ;
/// unary      ::= ( "!" | "-" ) unary
///              | primary ;
/// primary    ::= NUMBER | STRING | "true" | "false" | "nil"
///              | "(" expression ")" ;
///

impl Parser {
    pub fn new() -> Self {
        Parser {
            istream: None,
            current: 0,
        }
    }

    fn peek(&mut self) -> Option<Token> {
        self.istream.as_mut().unwrap().get(self.current).cloned()
    }

    fn advance(&mut self) -> Option<Token> {
        let result = self.istream.as_mut().unwrap().get(self.current).cloned();
        self.current += 1;
        result
    }

    fn consume(&mut self, token_type: TokenType) -> Result<(), ParserError> {
        if let Some(token) = self.peek() {
            if token.token_type == token_type {
                self.advance();
                return Ok(());
            }
        }
        Err(ParserError::UnknownToken(self.current))
    }

    fn synchronize(&mut self) {
        self.advance();
        while let Some(token) = self.peek() {
            if token.token_type == TokenType::Semicolon {
                return;
            }
            match token.token_type {
                TokenType::Procedure
                | TokenType::Function
                | TokenType::Var
                | TokenType::For
                | TokenType::If => {
                    break;
                },
                _ => { let _ = self.advance(); }
            }
        }
    }

    pub fn parse(&mut self, tokens: Vec<Token>) -> Result<Expr, ParserError> {
        self.istream = Some(tokens);
        self.expression()
    }

    pub fn expression(&mut self) -> Result<Expr, ParserError> {
        self.equality()
    }

    // Как же это убого выглядит
    pub fn equality(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.comparison()?;
        while let Some(token) = self.peek() {
            match token.token_type {
                TokenType::Equal | TokenType::NotEqual => {
                    self.advance();
                    let right = self.comparison()?;
                    expr = Expr::Binary {
                        left: Box::new(expr),
                        operator: token.clone(),
                        right: Box::new(right),
                    };
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    pub fn comparison(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.term()?;
        while let Some(token) = self.peek() {
            match token.token_type {
                TokenType::Less
                | TokenType::Greater
                | TokenType::LessEqual
                | TokenType::GreaterEqual => {
                    self.advance();
                    let right = self.term()?;
                    expr = Expr::Binary {
                        left: Box::new(expr),
                        operator: token.clone(),
                        right: Box::new(right),
                    };
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    pub fn term(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.factor()?;
        while let Some(token) = self.peek() {
            match token.token_type {
                TokenType::Plus | TokenType::Minus => {
                    self.advance();
                    let right = self.factor()?;
                    expr = Expr::Binary {
                        left: Box::new(expr),
                        operator: token.clone(),
                        right: Box::new(right),
                    };
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    pub fn factor(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.unary()?;
        while let Some(token) = self.peek() {
            match token.token_type {
                TokenType::Star | TokenType::Slash => {
                    self.advance();
                    let right = self.unary()?;
                    expr = Expr::Binary {
                        left: Box::new(expr),
                        operator: token.clone(),
                        right: Box::new(right),
                    };
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    pub fn unary(&mut self) -> Result<Expr, ParserError> {
        match self.peek() {
            Some(
                token @ Token {
                    token_type: TokenType::Minus | TokenType::Bang,
                    ..
                },
            ) => {
                self.advance();
                let expr = self.unary()?;
                Ok(Expr::Unary {
                    operator: token.clone(),
                    right: Box::new(expr),
                })
            }
            None => Err(ParserError::UnexpectedEOF(0)),
            _ => self.primary(),
        }
    }

    /// TODO: необходимо разработать для себя "особые" методы анализа кода на Rust
    pub fn primary(&mut self) -> Result<Expr, ParserError> {
        match self.peek() {
            Some(
                token @ Token {
                    token_type:
                        TokenType::Number(_)
                        | TokenType::True
                        | TokenType::False
                        | TokenType::StringLiteral(_),
                    ..
                },
            ) => {
                self.advance();
                Ok(Expr::Literal {
                    value: token.clone(),
                })
            }
            Some(Token {
                token_type: TokenType::LeftParen,
                ..
            }) => {
                self.advance();
                let expr = self.expression()?;
                self.consume(TokenType::RightParen)?;
                Ok(expr)
            }
            _ => Err(ParserError::UnexpectedEOF(0)),
        }
    }
}
