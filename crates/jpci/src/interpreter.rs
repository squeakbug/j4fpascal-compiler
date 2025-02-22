use std::{fmt, collections::HashMap};

use core::{ast::Expr, lexer::{self, Lexer}, parser::Parser};

#[derive(Debug)]
pub enum InterpreterError {
    NotImplemented,
}

/// Result of tree-walking
pub enum Value {
    Number(f64),
    String(String),
    Boolean(bool),
    Null,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Number(num) => write!(f, "{}", num),
            Value::String(s) => write!(f, "\"{}\"", s),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::Null => write!(f, "null"),
        }
    }
}

pub struct Environment {
    enclosing: Option<Box<Environment>>,
    symbols: HashMap<String, Value>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            enclosing: None,
            symbols: HashMap::new(),
        }
    }

    pub fn with_enclosing(enclosing: Box<Environment>) -> Self {
        Environment {
            enclosing: Some(enclosing),
            symbols: HashMap::new(),
        }
    }
}

pub struct Interpreter {
    environment: Environment,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            environment: Environment::new(),
        }
    }

    fn visit_expr(&mut self, expr: &Box<Expr>) -> Result<Value, InterpreterError> {
        match expr.as_ref() {
            Expr::Binary { left, operator, right } => {
                let left = self.visit_expr(left)?;
                let right = self.visit_expr(right)?;
                match (left, right) {
                    (Value::Number(a), Value::Number(b)) => {
                        match operator.token_type {
                            lexer::TokenType::Plus => Ok(Value::Number(a + b)),
                            lexer::TokenType::Minus => Ok(Value::Number(a - b)),
                            lexer::TokenType::Star => Ok(Value::Number(a * b)),
                            lexer::TokenType::Slash => Ok(Value::Number(a / b)),
                            _ => Err(InterpreterError::NotImplemented),
                        }
                    },
                    _ => Err(InterpreterError::NotImplemented),
                }
            },
            Expr::Unary { operator, right } => {
                let right = self.visit_expr(right)?;
                match right {
                    Value::Number(a) => {
                        match operator.token_type {
                            lexer::TokenType::Minus => Ok(Value::Number(-a)),
                            _ => Err(InterpreterError::NotImplemented),
                        }
                    },
                    _ => Err(InterpreterError::NotImplemented),
                }
            },
            Expr::Literal { value } => {
                match value.token_type {
                    lexer::TokenType::Number(num) => Ok(Value::Number(num)),
                    lexer::TokenType::True => Ok(Value::Boolean(true)),
                    lexer::TokenType::False => Ok(Value::Boolean(false)),
                    _ => Err(InterpreterError::NotImplemented),
                }
            }
            _ => Err(InterpreterError::NotImplemented),
        }
    }

    pub fn eval(&mut self, exp: &str) -> Result<String, InterpreterError> {
        let mut lexer = Lexer::new();
        let mut parser = Parser::new();

        let tokens = lexer.lex(exp).map_err(|_err| {
            InterpreterError::NotImplemented
        })?;
        let ast = parser.parse(tokens).map_err(|_err| {
            InterpreterError::NotImplemented
        })?;
        self.visit_expr(&Box::new(ast)).map(|v| v.to_string())
    }
}