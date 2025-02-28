use std::{collections::HashMap, fmt};

use core::{
    ast::{Block, DeclSection, DesignatorItem, Expr, ProcedureDeclaration, Stmt, TypeDeclaration, UnlabeledStmt, VarDeclaration}, 
    lexer::{self, Lexer}, 
    parser::Parser
};

#[derive(Debug)]
pub enum InterpreterError {
    NotImplemented,
    UndefinedVariable,
    AssigmentToNotDefinedVariable,
    NoParentEnvironment,
    NotCallable,
    MismatchedTypes,
}

#[derive(Debug, Clone)]
pub struct ProcedureValue {
    name: String,
    params: Vec<String>,
    body: Block,
}

/// Result of tree-walking
#[derive(Debug, Clone)]
pub enum Value {
    UnsignedInteger(i64),
    UnsignedReal(f64),
    String(String),
    Boolean(bool),
    Procedure(ProcedureValue),
    Null,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::UnsignedInteger(num) => write!(f, "{:?}", num),
            Value::UnsignedReal(num) => write!(f, "{:?}", num),
            Value::String(s) => write!(f, "\"{:?}\"", s),
            Value::Boolean(b) => write!(f, "{:?}", b),
            Value::Procedure(p) => write!(f, "{:?}", p),
            Value::Null => write!(f, "null"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Environment {
    pub enclosing: Option<Box<Environment>>,
    pub symbols: HashMap<String, Value>,
}

macro_rules! get_typed {
    ($self:expr, $var:expr, $variant:ident, $type:ty) => {
        match $self.symbols.get($var) {
            Some(&Value::$variant(val)) => Ok(val),
            Some(_) => Err(InterpreterError::MismatchedTypes),
            _ => Err(InterpreterError::UndefinedVariable),
        }
    };
    ($self:expr, $var:expr, $variant:ident, $type:ty, $ref:tt) => {
        match $self.symbols.get($var) {
            Some(Value::$variant($ref val)) => Ok(val),
            Some(_) => Err(InterpreterError::MismatchedTypes),
            _ => Err(InterpreterError::UndefinedVariable),
        }
    };
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

    pub fn define(&mut self, var: &str, val: Value) {
        self.symbols.insert(var.to_owned(), val);
    }

    pub fn assign(&mut self, var: &str, val: Value) -> Result<(), InterpreterError> {
        if let Some(sym) = self.symbols.get_mut(var) {
            *sym = val;
            Ok(())
        } else if let Some(ref mut enclosing) = self.enclosing {
            enclosing.assign(var, val)
        } else {
            Err(InterpreterError::AssigmentToNotDefinedVariable)
        }
    }

    pub fn get_value(&self, var: &str) -> Option<&Value> {
        self.symbols.get(var)
    }

    pub fn take_enclosing(&mut self) -> Option<Box<Environment>> {
        std::mem::replace(&mut self.enclosing, None)
    }
}

#[derive(Debug)]
pub struct Interpreter {
    environment: Box<Environment>,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            environment: Box::new(Environment::new()),
        }
    }

    fn scope_enter(&mut self) -> Result<(), InterpreterError> {
        let old = std::mem::replace(&mut self.environment, Box::new(Environment::new()));
        self.environment = Box::new(Environment::with_enclosing(old));
        Ok(())
    }

    fn scope_exit(&mut self) -> Result<(), InterpreterError> {
        self.environment = self.environment.as_mut().take_enclosing().ok_or(
            InterpreterError::NoParentEnvironment
        )?;
        Ok(())
    }

    fn visit_type_declaration(&mut self, _decl: &TypeDeclaration) -> Result<(), InterpreterError> {
        todo!()
    }

    fn visit_var_declaration(&mut self, _decl: &VarDeclaration) -> Result<(), InterpreterError> {
        todo!()
    }

    fn visit_procedure_declaration(
        &mut self, 
        _decl: &ProcedureDeclaration
    ) -> Result<(), InterpreterError> {
        todo!()
    }

    fn visit_decl_section(&mut self, section: &DeclSection) -> Result<(), InterpreterError> {
        match section {
            DeclSection::Type(type_declarations) => {
                for decl in type_declarations {
                    self.visit_type_declaration(decl)?;
                }
                Ok(())
            },
            DeclSection::Variable(var_declarations) => {
                for decl in var_declarations {
                    self.visit_var_declaration(decl)?;
                }
                Ok(())
            },
            DeclSection::Procedure(procedure_declarations) => {
                for decl in procedure_declarations {
                    self.visit_procedure_declaration(decl)?;
                }
                Ok(())
            },
        }
    }

    fn visit_statement(&mut self, stmt: &Box<Stmt>) -> Result<(), InterpreterError> {
        match stmt.statement.as_ref() {
            UnlabeledStmt::Assigment { left, right } => {
                let ident = &left.name;
                let value = self.visit_expr(right)?;
                self.environment.assign(ident, value)?;
                Ok(())
            },
            UnlabeledStmt::ProcedureCall { designator } => {
                let ident = &designator.name;
                let val = self.environment.get_value(ident).cloned().ok_or(
                    InterpreterError::UndefinedVariable
                )?;
                match val {
                    Value::Procedure(proc) => {
                        match designator.items.first() {
                            Some(&DesignatorItem::Call { ref arguments }) => {
                                self.scope_enter()?;
                                for (param, arg) in proc.params.iter().zip(arguments) {
                                    let value = self.visit_expr(&arg.0)?;
                                    self.environment.define(param, value);
                                }
                                let proc_stmt = &proc.body.body;
                                self.visit_statement(proc_stmt)?;
                                self.scope_exit()?;
                                Ok(())
                            },
                            None => Err(InterpreterError::NotImplemented),
                            _ => Err(InterpreterError::NotCallable),
                        }
                    },
                    _ => Err(InterpreterError::NotCallable),
                }
            },
            UnlabeledStmt::Compound { statements } => {
                self.scope_enter()?;
                for statement in statements {
                    self.visit_statement(statement)?;
                }
                self.scope_exit()?;
                Ok(())
            },
            UnlabeledStmt::If { condition, then_branch, else_branch } => {
                let condition_value = self.visit_expr(condition)?;
                self.scope_enter()?;
                let result = match condition_value {
                    Value::Boolean(true) => {
                        self.visit_statement(then_branch)
                    },
                    Value::Boolean(false) => {
                        if let Some(else_branch) = else_branch {
                            self.visit_statement(else_branch)?;
                        }
                        Ok(())
                    },
                    _ => Err(InterpreterError::NotImplemented),
                };
                self.scope_exit()?;
                result
            },
            UnlabeledStmt::Case { condition, case_items, else_branch } => {
                Ok(())
            },
            UnlabeledStmt::Repeat { statements, condition } => {
                self.scope_enter()?;
                loop {
                    for stmt in statements {
                        self.visit_statement(stmt)?;
                    }
                    match self.visit_expr(condition)? {
                        Value::Boolean(false) => break,
                        Value::Boolean(true) => { },
                        _ => return Err(InterpreterError::MismatchedTypes),
                    }
                }
                self.scope_exit()?;
                Ok(())
            },
            UnlabeledStmt::While { condition, statement } => {
                self.scope_enter()?;
                loop {
                    match self.visit_expr(condition)? {
                        Value::Boolean(false) => break,
                        Value::Boolean(true) => self.visit_statement(statement)?,
                        _ => return Err(InterpreterError::MismatchedTypes),
                    }
                }
                self.scope_exit()?;
                Ok(())
            },
            UnlabeledStmt::For { var, init, to, statement, is_down_to } => {
                self.scope_enter()?;
                let init_val = self.visit_expr(init)?;
                let var_name = &var.name;
                self.environment.define(var_name, init_val);
                loop {
                    let i = get_typed!(self.environment, var_name, UnsignedInteger, i64)?;
                    let to_val = self.visit_expr(to)?;
                    match to_val {
                        Value::UnsignedInteger(to_val) => {
                            if *is_down_to {
                                if i <= to_val { break; }
                                self.environment.assign(var_name, Value::UnsignedInteger(i - 1))?;
                            } else {
                                if i >= to_val { break; }
                                self.environment.assign(var_name, Value::UnsignedInteger(i + 1))?;
                            }
                            self.visit_statement(statement)?;
                        },
                        _ => return Err(InterpreterError::MismatchedTypes),
                    }
                }
                self.scope_exit()?;
                Ok(())
            },
            UnlabeledStmt::Goto { label: _label } => {
                Err(InterpreterError::NotImplemented)
            },
            UnlabeledStmt::Break => {
                Err(InterpreterError::NotImplemented)
            },
            UnlabeledStmt::Continue => {
                Err(InterpreterError::NotImplemented)
            },
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
                    lexer::TokenType::Nil => Ok(Value::Null),
                    lexer::TokenType::StringLiteral(ref val) => Ok(Value::String(val.clone())),
                    _ => Err(InterpreterError::NotImplemented),
                }
            },
            Expr::Designator { designator } => {
                let name = &designator.name;
                return self.environment.get_value(name).cloned().ok_or(
                    InterpreterError::UndefinedVariable
                );
            }
        }
    }

    fn visit_block(&mut self, block: &Box<Block>) -> Result<(), InterpreterError> {
        for section in block.decl_sections.iter() {
            self.visit_decl_section(section)?;
        }
        self.visit_statement(&block.body)
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
        })?.unwrap();
        dbg!(&ast);
        self.visit_block(&Box::new(ast))
    }
}