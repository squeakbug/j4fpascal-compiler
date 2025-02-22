use thiserror::Error;

use crate::{
    ast::{Expr, Stmt},
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

    pub fn parse(&mut self, tokens: Vec<Token>) -> Result<Stmt, ParserError> {
        self.istream = Some(tokens);
        self.statement()
    }

    fn statement(&mut self) -> Result<Stmt, ParserError> {
        if let Some(token) = self.peek() {
            if token.token_type == TokenType::Label {
                self.advance();
                self.consume(TokenType::Colon)?;
                let statement = self.unlabeled_statement()?;
                Ok(Stmt::Labeled {
                    label: token,
                    statement: Box::new(statement),
                })
            } else {
                self.unlabeled_statement()
            }
        } else {
            return Err(ParserError::UnexpectedEOF(self.current));
        }
    }

    fn unlabeled_statement(&mut self) -> Result<Stmt, ParserError> {
        if let Ok(statement) = self.simple_statement() {
            Ok(statement)
        } else if let Ok(statement) = self.structured_statement() {
            Ok(statement)
        } else {
            Err(ParserError::UnterminatedStringLiteral(self.current))
        }
    }

    fn simple_statement(&mut self) -> Result<Stmt, ParserError> {
        if let Ok(statement) = self.assigmnent_statement() {
            Ok(statement)
        } else if let Ok(statement) = self.procedure_statement() {
            Ok(statement)
        } else if let Ok(statement) = self.goto_statement() {
            Ok(statement)
        } else if let Ok(statement) = self.empty_statement() {
            Ok(statement)
        } else {
            Err(ParserError::UnterminatedStringLiteral(self.current))
        }
    }

    // TODO: add structured identifier
    fn assigmnent_statement(&mut self) -> Result<Stmt, ParserError> {
        match self.peek() {
            Some(token@Token { token_type: TokenType::Identifier(_), .. }) => {
                self.advance();
                self.consume(TokenType::Assignment)?;
                Ok(Stmt::Assigment {
                    left: token,
                    right: Box::new(self.expression()?),
                })
            }
            _ => Err(ParserError::UnexpectedEOF(self.current)),
        }
    }

    fn procedure_statement(&mut self) -> Result<Stmt, ParserError> {
        todo!()
    }

    fn goto_statement(&mut self) -> Result<Stmt, ParserError> {
        todo!()
    }

    fn empty_statement(&mut self) -> Result<Stmt, ParserError> {
        todo!()
    }

    fn structured_statement(&mut self) -> Result<Stmt, ParserError> {
        if let Ok(statement) = self.compound_statement() {
            Ok(statement)
        } else if let Ok(statement) = self.conditional_statement() {
            Ok(statement)
        } else if let Ok(statement) = self.repetetive_statement() {
            Ok(statement)
        } else if let Ok(statement) = self.with_statement() {
            Ok(statement)
        } else {
            Err(ParserError::UnterminatedStringLiteral(self.current))
        }
    }

    fn compound_statement(&mut self) -> Result<Stmt, ParserError> {
        self.consume(TokenType::Begin)?;
        let mut statements = vec![Box::new(self.statement()?)];
        while let Some(token) = self.peek() {
            if token.token_type == TokenType::Semicolon {
                self.advance();
                let statement = Box::new(self.statement()?);
                statements.push(statement);
            }
        }
        self.consume(TokenType::End)?;
        Ok(Stmt::Compound {
            statements
        })
    }

    fn conditional_statement(&mut self) -> Result<Stmt, ParserError> {
        self.if_statement()
    }

    fn if_statement(&mut self) -> Result<Stmt, ParserError> {
        self.consume(TokenType::If)?;
        let condition = Box::new(self.expression()?);
        self.consume(TokenType::Then)?;
        let then_branch = Box::new(self.statement()?);
        if let Some(token) = self.peek() {
            if token.token_type == TokenType::Else {
                self.advance();
                let else_branch = Some(Box::new(self.statement()?));
                return Ok(Stmt::If {
                    condition,
                    then_branch,
                    else_branch,
                })
            }
        }
        Ok(Stmt::If {
            condition,
            then_branch,
            else_branch: None,
        })
    }

    fn case_statement(&mut self) -> Result<Stmt, ParserError> {
        todo!()
    }

    fn repetetive_statement(&mut self) -> Result<Stmt, ParserError> {
        todo!()
    }

    fn with_statement(&mut self) -> Result<Stmt, ParserError> {
        todo!()
    }

    // Как же это убого выглядит
    fn expression(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.simple_expression()?;
        if let Some(token) = self.peek() {
            match token.token_type {
                TokenType::Equal 
                | TokenType::NotEqual
                | TokenType::Less
                | TokenType::Greater
                | TokenType::LessEqual
                | TokenType::GreaterEqual
                | TokenType::In => {
                    self.advance();
                    let right = self.expression()?;
                    expr = Expr::Binary {
                        left: Box::new(expr),
                        operator: token.clone(),
                        right: Box::new(right),
                    };
                }
                _ => { },
            }
        }
        Ok(expr)
    }

    fn simple_expression(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.term()?;
        if let Some(token) = self.peek() {
            match token.token_type {
                TokenType::Plus | TokenType::Minus | TokenType::Or => {
                    self.advance();
                    let right = self.simple_expression()?;
                    expr = Expr::Binary {
                        left: Box::new(expr),
                        operator: token.clone(),
                        right: Box::new(right),
                    };
                }
                _ => { },
            }
        }
        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.signed_factor()?;
        while let Some(token) = self.peek() {
            match token.token_type {
                TokenType::Star | TokenType::Slash 
                | TokenType::Div | TokenType::Mod
                | TokenType::And => {
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

    fn signed_factor(&mut self) -> Result<Expr, ParserError> {
        if let Some(token) = self.peek() {
            match token.token_type {
                TokenType::Plus | TokenType::Minus => {
                    self.advance();
                    let right = self.factor()?;
                    Ok(Expr::Unary {
                        operator: token.clone(),
                        right: Box::new(right),
                    })
                },
                _ => self.factor()
            }
        } else {
            Err(ParserError::UnexpectedEOF(self.current))
        }
    }

    /// TODO: необходимо разработать для себя "особые" методы анализа кода на Rust
    fn factor(&mut self) -> Result<Expr, ParserError> {
        match self.peek() {
            // variable
            Some(token@Token {
                token_type: TokenType::Identifier(_),
               ..
            }) => {
                self.advance();
                Ok(Expr::Variable {
                    var: token.clone(),
                })
            },
            // LPAREN expression RPAREN
            Some(Token {
                token_type: TokenType::LeftParen,
                ..
            }) => {
                self.advance();
                let expr = self.expression()?;
                self.consume(TokenType::RightParen)?;
                Ok(expr)
            },
            // unsignedConstant
            Some(
                token @ Token {
                    token_type:
                        TokenType::UnsignedInteger(_)
                        | TokenType::UnsignedReal(_)
                        | TokenType::StringLiteral(_)
                        | TokenType::Nil,
                    ..
                },
            ) => {
                self.advance();
                Ok(Expr::Literal {
                    value: token.clone(),
                })
            },
            // NOT factor
            Some(token@Token {
                token_type: TokenType::Not,
                ..
            }) => {
                self.advance();
                let expr = self.factor()?;
                Ok(Expr::Unary {
                    operator: token.clone(),
                    right: Box::new(expr),
                })
            },
            // bool_
            Some(
                token @ Token {
                    token_type:
                        TokenType::True
                        | TokenType::False,
                    ..
                },
            ) => {
                self.advance();
                Ok(Expr::Literal {
                    value: token.clone(),
                })
            },
            _ => Err(ParserError::UnexpectedEOF(0)),
        }
    }
}
