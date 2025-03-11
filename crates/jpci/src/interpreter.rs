use std::{
    collections::HashMap,
    fmt::{self, Debug},
    fs,
};

use core::{
    ast::{
        Block, DeclSection, Designator, DesignatorItem, Expr, ProcedureDeclaration, Program, Stmt,
        TypeDeclaration, UnlabeledStmt, VarDeclaration,
    },
    error::{self, Error},
    lexer::{self, Lexer},
    parser::Parser,
};

use camino::Utf8PathBuf;

use crate::callable::{
    Arity, Callable, NativeProcedureValue, ProcedureValue, ReadProcedureValue,
    ReadlnProcedureValue, WriteProcedureValue, WritelnProcedureValue,
};

#[derive(Debug, Clone)]
pub enum InterpreterError {
    NotImplemented,
    UndefinedVariable,
    AssigmentToNotDefinedVariable,
    NoParentEnvironment,
    NotCallable,
    MismatchedTypes,
    MismathedArgumentsCount,
    AlreadyDefined,
}

#[derive(Debug, Clone)]
pub struct Environment {
    pub enclosing: Option<Box<Environment>>,
    pub symbols: HashMap<String, Symbol>,
}

macro_rules! get_typed {
    ($self:expr, $var:expr, $variant:ident, $type:ty) => {
        match $self.get_value($var) {
            Some(&Value::$variant(val)) => Ok(val),
            Some(_) => Err(InterpreterError::MismatchedTypes),
            _ => Err(InterpreterError::UndefinedVariable),
        }
    };
    ($self:expr, $var:expr, $variant:ident, $type:ty, $ref:tt) => {
        match $self.get_value($var) {
            Some(Value::$variant($ref val)) => Ok(val),
            Some(_) => Err(InterpreterError::MismatchedTypes),
            _ => Err(InterpreterError::UndefinedVariable),
        }
    };
}

impl Environment {
    fn get_builtin_symbols() -> HashMap<String, Value> {
        let write_sym = Value::Procedure(ProcedureValue::Write(WriteProcedureValue));
        let writeln_sym = Value::Procedure(ProcedureValue::Writeln(WritelnProcedureValue));
        let read_sym = Value::Procedure(ProcedureValue::Read(ReadProcedureValue));
        let readln_sym = Value::Procedure(ProcedureValue::Readln(ReadlnProcedureValue));
        let mut symbols = HashMap::new();
        symbols.insert("write".to_owned(), write_sym);
        symbols.insert("writeln".to_owned(), writeln_sym);
        symbols.insert("read".to_owned(), read_sym);
        symbols.insert("readln".to_owned(), readln_sym);
        symbols
    }

    pub fn new() -> Self {
        Environment {
            enclosing: None,
            symbols: Self::get_builtin_symbols(),
        }
    }

    pub fn with_enclosing(enclosing: Box<Environment>) -> Self {
        Environment {
            enclosing: Some(enclosing),
            symbols: HashMap::new(),
        }
    }

    pub fn define(&mut self, var: &str, val: Value) -> Result<(), InterpreterError> {
        if let Some(_) = self.get_value(var) {
            return Err(InterpreterError::AlreadyDefined);
        } else {
            self.symbols.insert(var.to_owned(), val);
            Ok(())
        }
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
        if let Some(sym) = self.symbols.get(var) {
            Some(sym)
        } else if let Some(ref enclosing) = self.enclosing {
            enclosing.get_value(var)
        } else {
            None
        }
    }

    pub fn take_enclosing(&mut self) -> Option<Box<Environment>> {
        std::mem::replace(&mut self.enclosing, None)
    }
}

#[derive(Debug)]
pub struct Interpreter {
    pub environment: Box<Environment>,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            environment: Box::new(Environment::new()),
        }
    }

    pub fn scope_enter(&mut self) -> Result<(), InterpreterError> {
        let old = std::mem::replace(&mut self.environment, Box::new(Environment::new()));
        self.environment = Box::new(Environment::with_enclosing(old));
        Ok(())
    }

    pub fn scope_exit(&mut self) -> Result<(), InterpreterError> {
        self.environment = self
            .environment
            .as_mut()
            .take_enclosing()
            .ok_or(InterpreterError::NoParentEnvironment)?;
        Ok(())
    }

    fn visit_type_declaration(&mut self, decl: &TypeDeclaration) -> Result<(), InterpreterError> {
        todo!()
    }

    fn visit_var_declaration(&mut self, decl: &VarDeclaration) -> Result<(), InterpreterError> {
        let init_value = if let Some(val) = &decl.init_value {
            self.visit_expr(val)?
        } else {
            match &decl.var_type {
                _ => Value::UnsignedInteger(0),
            }
        };
        self.environment.define(&decl.name, init_value)
    }

    fn visit_procedure_declaration(
        &mut self,
        decl: &ProcedureDeclaration,
    ) -> Result<(), InterpreterError> {
        let proc = ProcedureValue::Native(NativeProcedureValue { decl: decl.clone() });
        self.environment
            .define(&decl.head.name, Value::Procedure(proc))
    }

    fn visit_label_declaration(&mut self, _decl: &str) -> Result<(), InterpreterError> {
        Ok(())
    }

    fn visit_const_declaration(
        &mut self,
        _decl: &(String, Box<Expr>),
    ) -> Result<(), InterpreterError> {
        Ok(())
    }

    fn visit_decl_section(&mut self, section: &DeclSection) -> Result<(), InterpreterError> {
        match section {
            DeclSection::Label(label_decl) => {
                for decl in label_decl.iter() {
                    self.visit_label_declaration(decl)?;
                }
                Ok(())
            }
            DeclSection::Const(const_decl) => {
                for decl in const_decl.iter() {
                    self.visit_const_declaration(decl)?;
                }
                Ok(())
            }
            DeclSection::Type(type_decl) => {
                for decl in type_decl.iter() {
                    self.visit_type_declaration(decl)?;
                }
                Ok(())
            }
            DeclSection::Variable(var_decl) => {
                for decl in var_decl.iter() {
                    self.visit_var_declaration(decl)?;
                }
                Ok(())
            }
            DeclSection::Procedure(proc_decl) => {
                self.visit_procedure_declaration(proc_decl)?;
                Ok(())
            }
        }
    }

    pub fn visit_statement(&mut self, stmt: &Box<Stmt>) -> Result<(), InterpreterError> {
        match stmt.statement.as_ref() {
            UnlabeledStmt::Assigment { left, right } => {
                let ident = &left.name;
                let value = self.visit_expr(right)?;
                self.environment.assign(ident, value)?;
                Ok(())
            }
            UnlabeledStmt::ProcedureCall { designator } => {
                let ident = &designator.name;
                // We need clone cause statements in procedure definition can delete function symbol itself
                // And also i don't know how i can represent this invariant in type system
                let mut proc = Box::new(
                    get_typed!(self.environment, ident, Procedure, ProcedureValue, ref)?.clone(),
                );
                match designator.items.first() {
                    Some(&DesignatorItem::Call { ref arguments }) => {
                        if let Arity::Static(arity) = proc.arity() {
                            if arity != arguments.len() {
                                return Err(InterpreterError::MismathedArgumentsCount);
                            }
                        }
                        let values: Result<Vec<Value>, _> = arguments
                            .iter()
                            .map(|arg| match *arg.0 {
                                // Call-by-reference
                                // TODO: check if formal parameter has 'var' modifier
                                Expr::Designator { ref designator } => {
                                    Ok(Value::Ref(designator.clone()))
                                }
                                // Call-by-value
                                // TODO: wtf boxing???
                                ref expr => self.visit_expr(&Box::new(expr.clone())),
                            })
                            .collect();
                        proc.call(self, values?)?;
                        Ok(())
                    }
                    None => {
                        if let Arity::Static(arity) = proc.arity() {
                            if arity != 0 {
                                return Err(InterpreterError::MismathedArgumentsCount);
                            }
                        }
                        proc.call(self, vec![])?;
                        Ok(())
                    }
                    _ => Err(InterpreterError::NotCallable),
                }
            }
            UnlabeledStmt::Compound { statements } => {
                self.scope_enter()?;
                for statement in statements {
                    self.visit_statement(statement)?;
                }
                self.scope_exit()?;
                Ok(())
            }
            UnlabeledStmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let condition_value = self.visit_expr(condition)?;
                self.scope_enter()?;
                let result = match condition_value {
                    Value::Boolean(true) => self.visit_statement(then_branch),
                    Value::Boolean(false) => {
                        if let Some(else_branch) = else_branch {
                            self.visit_statement(else_branch)?;
                        }
                        Ok(())
                    }
                    _ => Err(InterpreterError::MismatchedTypes),
                };
                self.scope_exit()?;
                result
            }
            UnlabeledStmt::Case {
                condition: _,
                case_items: _,
                else_branch: _,
            } => {
                Ok(())
            },
            UnlabeledStmt::Repeat {
                statements,
                condition,
            } => {
                self.scope_enter()?;
                loop {
                    for stmt in statements {
                        self.visit_statement(stmt)?;
                    }
                    match self.visit_expr(condition)? {
                        Value::Boolean(false) => break,
                        Value::Boolean(true) => {}
                        _ => return Err(InterpreterError::MismatchedTypes),
                    }
                }
                self.scope_exit()?;
                Ok(())
            }
            UnlabeledStmt::While {
                condition,
                statement,
            } => {
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
            }
            UnlabeledStmt::For {
                var,
                init,
                to,
                statement,
                is_down_to,
            } => {
                self.scope_enter()?;
                let init_val = self.visit_expr(init)?;
                let var_name = &var.name;
                self.environment.define(var_name, init_val)?;
                loop {
                    let i = get_typed!(self.environment, var_name, UnsignedInteger, i64)?;
                    let to_val = self.visit_expr(to)?;
                    match to_val {
                        Value::UnsignedInteger(to_val) => {
                            if *is_down_to {
                                if i <= to_val {
                                    break;
                                }
                                self.environment
                                    .assign(var_name, Value::UnsignedInteger(i - 1))?;
                            } else {
                                if i >= to_val {
                                    break;
                                }
                                self.environment
                                    .assign(var_name, Value::UnsignedInteger(i + 1))?;
                            }
                            self.visit_statement(statement)?;
                        }
                        _ => return Err(InterpreterError::MismatchedTypes),
                    }
                }
                self.scope_exit()?;
                Ok(())
            }
            UnlabeledStmt::Goto { label: _label } => Err(InterpreterError::NotImplemented),
            UnlabeledStmt::Break => Err(InterpreterError::NotImplemented),
            UnlabeledStmt::Continue => Err(InterpreterError::NotImplemented),
            UnlabeledStmt::Empty => Ok(()),
        }
    }

    // TODO: add typecast
    fn visit_expr(&mut self, expr: &Box<Expr>) -> Result<Value, InterpreterError> {
        match expr.as_ref() {
            Expr::Binary {
                left,
                operator,
                right,
            } => {
                let left = self.visit_expr(left)?;
                let right = self.visit_expr(right)?;
                if operator.kind == lexer::TokenType::Or {
                    return match (left, right) {
                        (Value::Boolean(a), Value::Boolean(b)) => Ok(Value::Boolean(a | b)),
                        _ => Err(InterpreterError::NotImplemented),
                    };
                }
                if operator.kind == lexer::TokenType::And {
                    return match (left, right) {
                        (Value::Boolean(a), Value::Boolean(b)) => Ok(Value::Boolean(a & b)),
                        _ => Err(InterpreterError::NotImplemented),
                    };
                }
                match (left, right) {
                    (Value::UnsignedInteger(a), Value::UnsignedInteger(b)) => match operator.kind {
                        lexer::TokenType::Plus => Ok(Value::UnsignedInteger(a + b)),
                        lexer::TokenType::Minus => Ok(Value::UnsignedInteger(a - b)),
                        lexer::TokenType::Star => Ok(Value::UnsignedInteger(a * b)),
                        lexer::TokenType::Slash => Ok(Value::UnsignedInteger(a / b)),
                        lexer::TokenType::Greater => Ok(Value::Boolean(a > b)),
                        lexer::TokenType::GreaterEqual => Ok(Value::Boolean(a >= b)),
                        lexer::TokenType::Less => Ok(Value::Boolean(a < b)),
                        lexer::TokenType::LessEqual => Ok(Value::Boolean(a <= b)),
                        lexer::TokenType::Equal => Ok(Value::Boolean(a == b)),
                        lexer::TokenType::NotEqual => Ok(Value::Boolean(a != b)),
                        _ => Err(InterpreterError::NotImplemented),
                    },
                    (Value::UnsignedReal(a), Value::UnsignedReal(b)) => match operator.kind {
                        lexer::TokenType::Plus => Ok(Value::UnsignedReal(a + b)),
                        lexer::TokenType::Minus => Ok(Value::UnsignedReal(a - b)),
                        lexer::TokenType::Star => Ok(Value::UnsignedReal(a * b)),
                        lexer::TokenType::Slash => Ok(Value::UnsignedReal(a / b)),
                        _ => Err(InterpreterError::NotImplemented),
                    },
                    _ => Err(InterpreterError::NotImplemented),
                }
            }
            Expr::Unary { operator, right } => {
                let right = self.visit_expr(right)?;
                match right {
                    Value::UnsignedInteger(a) => match operator.kind {
                        lexer::TokenType::Minus => Ok(Value::UnsignedInteger(-a)),
                        _ => Err(InterpreterError::NotImplemented),
                    },
                    Value::UnsignedReal(a) => match operator.kind {
                        lexer::TokenType::Minus => Ok(Value::UnsignedReal(-a)),
                        _ => Err(InterpreterError::NotImplemented),
                    },
                    _ => Err(InterpreterError::NotImplemented),
                }
            }
            Expr::Literal { value } => match value.kind {
                lexer::TokenType::UnsignedInteger(ref num) => match num.parse::<i64>() {
                    Ok(num) => Ok(Value::UnsignedInteger(num)),
                    Err(_) => Err(InterpreterError::NotImplemented),
                },
                lexer::TokenType::UnsignedReal(ref num) => match num.parse::<f64>() {
                    Ok(num) => Ok(Value::UnsignedReal(num)),
                    Err(_) => Err(InterpreterError::NotImplemented),
                },
                lexer::TokenType::True => Ok(Value::Boolean(true)),
                lexer::TokenType::False => Ok(Value::Boolean(false)),
                lexer::TokenType::Nil => Ok(Value::Null),
                lexer::TokenType::StringLiteral(ref val) => Ok(Value::String(val.clone())),
                _ => Err(InterpreterError::NotImplemented),
            },
            Expr::Designator { designator } => {
                let name = &designator.name;
                return self
                    .environment
                    .get_value(name)
                    .cloned()
                    .ok_or(InterpreterError::UndefinedVariable);
            }
        }
    }

    fn visit_block(&mut self, block: &Box<Block>) -> Result<(), InterpreterError> {
        for section in block.decl_sections.iter() {
            self.visit_decl_section(section)?;
        }
        self.visit_statement(&block.body)
    }

    pub fn visit_program(&mut self, program: &Box<Program>) -> Result<(), InterpreterError> {
        self.visit_block(&program.block)
    }
}
