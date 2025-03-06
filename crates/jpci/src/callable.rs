use core::ast::ProcedureDeclaration;

use crate::interpreter::{Interpreter, InterpreterError, Value};

pub trait Callable {
    fn call(
        &mut self,
        executor: &mut Interpreter, 
        args: Vec<Value>
    ) -> Result<(), InterpreterError>;
    fn arity(&self) -> usize;
}

#[derive(Debug, Clone)]
pub struct NativeProcedureValue {
    pub decl: ProcedureDeclaration,
}

#[derive(Debug, Clone)]
pub struct WriteProcedureValue;

impl Callable for WriteProcedureValue {
    fn call(
        &mut self, 
        _executor: &mut Interpreter, 
        args: Vec<Value>
    ) -> Result<(), InterpreterError> {
        let mut result = String::new();
        for arg in args {
            match arg {
                Value::String(s) => result.push_str(&format!("{}", s)),
                v => result.push_str(&format!("{}", v)),
            }
        }
        print!("{}", result);
        Ok(())
    }

    fn arity(&self) -> usize { 1 }
}

#[derive(Debug, Clone)]
pub struct WritelnProcedureValue;

impl Callable for WritelnProcedureValue {
    fn call(
        &mut self, 
        _executor: &mut Interpreter, 
        args: Vec<Value>
    ) -> Result<(), InterpreterError> {
        let mut result = String::new();
        for arg in args {
            match arg {
                Value::String(s) => result.push_str(&format!("{}", s)),
                v => result.push_str(&format!("{}", v)),
            }
        }
        println!("{}", result);
        Ok(())
    }

    fn arity(&self) -> usize { 1 }
}


#[derive(Debug, Clone)]
pub struct ReadProcedureValue;

impl Callable for ReadProcedureValue {
    fn call(
        &mut self,
        _executor: &mut Interpreter, 
        _args: Vec<Value>
    ) -> Result<(), InterpreterError> {
        Ok(())
    }

    fn arity(&self) -> usize { 0 }
}

#[derive(Debug, Clone)]
pub struct ReadlnProcedureValue;

impl Callable for ReadlnProcedureValue {
    fn call(
        &mut self,
        _executor: &mut Interpreter, 
        _args: Vec<Value>
    ) -> Result<(), InterpreterError> {
        Ok(())
    }

    fn arity(&self) -> usize { 0 }
}

impl Callable for NativeProcedureValue {
    fn call(
        &mut self, 
        executor: &mut Interpreter, 
        args: Vec<Value>
    ) -> Result<(), InterpreterError> {
        executor.scope_enter()?;
        let def = &self.decl;
        let head = &def.head;
        for (param, value) in head.params.iter().zip(args.into_iter()) {
            executor.environment.define(&param.0, value)?;
        }
        executor.visit_statement(&def.body)?;
        executor.scope_exit()?;
        Ok(())
    }

    fn arity(&self) -> usize { self.decl.head.params.len() }
}

#[derive(Debug, Clone)]
pub enum ProcedureValue {
    Native(NativeProcedureValue),
    Write(WriteProcedureValue),
    Writeln(WritelnProcedureValue),
    Read(ReadProcedureValue),
    Readln(ReadlnProcedureValue),
}

impl Callable for ProcedureValue {
    fn arity(&self) -> usize {
        match self {
            ProcedureValue::Native(ref value) => value.arity(),
            ProcedureValue::Write(ref value) => value.arity(),
            ProcedureValue::Writeln(ref value) => value.arity(),
            ProcedureValue::Read(ref value) => value.arity(),
            ProcedureValue::Readln(ref value) => value.arity(),
        }
    }

    fn call(
        &mut self,
        executor: &mut Interpreter, 
        args: Vec<Value>
    ) -> Result<(), InterpreterError> { 
        match self {
            ProcedureValue::Native(ref mut value) => value.call(executor, args),
            ProcedureValue::Write(ref mut value) => value.call(executor, args),
            ProcedureValue::Writeln(ref mut value) => value.call(executor, args),
            ProcedureValue::Read(ref mut value) => value.call(executor, args),
            ProcedureValue::Readln(ref mut value) => value.call(executor, args),
        }
    }
}
