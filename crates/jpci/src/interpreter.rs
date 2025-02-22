use std::{borrow::Borrow, collections::HashMap, fmt};

use core::{ast::{Expr, Stmt}, lexer::{self, Lexer, Token, TokenType}, parser::Parser};

#[derive(Debug)]
pub enum InterpreterError {
    NotImplemented,
}

/// Result of tree-walking
#[derive(Debug, Clone)]
pub enum Value {
    UnsignedInteger(i64),
    UnsignedReal(f64),
    String(String),
    Boolean(bool),
    Null,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::UnsignedInteger(num) => write!(f, "{}", num),
            Value::UnsignedReal(num) => write!(f, "{}", num),
            Value::String(s) => write!(f, "\"{}\"", s),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::Null => write!(f, "null"),
        }
    }
}

#[derive(Debug, Clone)]
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

    pub fn assign(&mut self, var: String, val: Value) {
        self.symbols.insert(var, val);
    }

    pub fn get(&self, var: String) -> Option<Value> {
        if let Some(val) = self.symbols.get(&var) {
            Some(val.clone())
        } else if self.enclosing.is_some() {
            self.enclosing.as_ref().unwrap().get(var)
        } else {
            None
        }
    }
}

#[derive(Debug)]
pub struct Interpreter {
    environment: Option<Environment>,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            environment: Some(Environment::new()),
        }
    }

    fn visit_statement(&mut self, stmt: &Box<Stmt>) -> Result<(), InterpreterError> {
        match stmt.as_ref() {
            Stmt::Assigment { left, right } => {
                match left {
                    Token { token_type: TokenType::Identifier(ref varname), .. } => {
                        let value = self.visit_expr(right)?;
                        self.environment.as_mut().map(|e| e.assign(varname.clone(), value));
                        Ok(())
                    },
                    _ => Err(InterpreterError::NotImplemented),
                }
            },
            Stmt::If { condition, then_branch, else_branch } => {
                let condition_value = self.visit_expr(condition)?;
                match condition_value {
                    Value::Boolean(true) => {
                        self.visit_statement(then_branch)
                    },
                    Value::Boolean(false) => {
                        if let Some(else_branch) = else_branch {
                            self.visit_statement(else_branch)
                        } else {
                            Ok(())
                        }
                    },
                    _ => Err(InterpreterError::NotImplemented),
                }
            },
            Stmt::Compound { statements } => {
                let old = std::mem::replace(&mut self.environment, None);
                let new = Environment::with_enclosing(Box::new(old.clone().unwrap()));
                self.environment = Some(new);
                for statement in statements {
                    self.visit_statement(statement)?;
                }
                self.environment = old;
                Ok(())
            },
            _ => Err(InterpreterError::NotImplemented),
        }
    }

    // TODO: add typecast
    fn visit_expr(&mut self, expr: &Box<Expr>) -> Result<Value, InterpreterError> {
        match expr.as_ref() {
            Expr::Binary { left, operator, right } => {
                let left = self.visit_expr(left)?;
                let right = self.visit_expr(right)?;
                match (left, right) {
                    (Value::UnsignedInteger(a), Value::UnsignedInteger(b)) => {
                        match operator.token_type {
                            lexer::TokenType::Plus => Ok(Value::UnsignedInteger(a + b)),
                            lexer::TokenType::Minus => Ok(Value::UnsignedInteger(a - b)),
                            lexer::TokenType::Star => Ok(Value::UnsignedInteger(a * b)),
                            lexer::TokenType::Slash => Ok(Value::UnsignedInteger(a / b)),
                            _ => Err(InterpreterError::NotImplemented),
                        }
                    },
                    (Value::UnsignedReal(a), Value::UnsignedReal(b)) => {
                        match operator.token_type {
                            lexer::TokenType::Plus => Ok(Value::UnsignedReal(a + b)),
                            lexer::TokenType::Minus => Ok(Value::UnsignedReal(a - b)),
                            lexer::TokenType::Star => Ok(Value::UnsignedReal(a * b)),
                            lexer::TokenType::Slash => Ok(Value::UnsignedReal(a / b)),
                            _ => Err(InterpreterError::NotImplemented),
                        }
                    },
                    _ => Err(InterpreterError::NotImplemented),
                }
            },
            Expr::Unary { operator, right } => {
                let right = self.visit_expr(right)?;
                match right {
                    Value::UnsignedInteger(a) => {
                        match operator.token_type {
                            lexer::TokenType::Minus => Ok(Value::UnsignedInteger(-a)),
                            _ => Err(InterpreterError::NotImplemented),
                        }
                    },
                    Value::UnsignedReal(a) => {
                        match operator.token_type {
                            lexer::TokenType::Minus => Ok(Value::UnsignedReal(-a)),
                            _ => Err(InterpreterError::NotImplemented),
                        }
                    },
                    _ => Err(InterpreterError::NotImplemented),
                }
            },
            Expr::Literal { value } => {
                match value.token_type {
                    lexer::TokenType::UnsignedInteger(ref num) => {
                        match num.parse::<i64>() {
                            Ok(num) => Ok(Value::UnsignedInteger(num)),
                            Err(_) => Err(InterpreterError::NotImplemented),
                        }
                    },
                    lexer::TokenType::UnsignedReal(ref num) => {
                        match num.parse::<f64>() {
                            Ok(num) => Ok(Value::UnsignedReal(num)),
                            Err(_) => Err(InterpreterError::NotImplemented),
                        }
                    },
                    lexer::TokenType::True => Ok(Value::Boolean(true)),
                    lexer::TokenType::False => Ok(Value::Boolean(false)),
                    _ => Err(InterpreterError::NotImplemented),
                }
            },
            Expr::Variable { var } => {
                match var {
                    Token { token_type: TokenType::Identifier(name), .. } => {
                        let val = self.environment.as_ref().unwrap().get(name.clone()).unwrap();
                        return Ok(val);
                    },
                    _ => return Err(InterpreterError::NotImplemented),
                }
            }
            _ => Err(InterpreterError::NotImplemented),
        }
    }

    pub fn eval(&mut self, exp: &str) -> Result<(), InterpreterError> {
        let mut lexer = Lexer::new();
        let mut parser = Parser::new();

        let tokens = lexer.lex(exp).map_err(|_err| {
            InterpreterError::NotImplemented
        })?;
        dbg!(&tokens);
        let ast = parser.parse(tokens).map_err(|_err| {
            InterpreterError::NotImplemented
        })?;
        dbg!(&ast);
        self.visit_statement(&Box::new(ast))
    }
}