use std::env;
use std::fs::File;
use std::io::{self, Read};

pub enum Opcode {
    Break,
    Push,
    Pop,
    Add,
    Sub,
    Mul,
    Div,
}

pub struct PMachine {
    ip: usize,
    imem: Vec<u8>,
    istack: Vec<u32>,
    dstack: Vec<u32>,
}

impl PMachine {
    fn ipop(&mut self) -> Option<u32> {
        self.istack.pop()
    }

    fn ipush(&mut self, inst: u32) {
        self.istack.push(inst)
    }

    fn dpop(&mut self) -> Option<u32> {
        self.dstack.pop()
    }

    fn dpush(&mut self, data: u32) {
        self.dstack.push(data);
    }
}

#[derive(Debug)]
pub enum RuntimeError {
    UnexpectedOpcode,
    EmptyStack,
    FailedToParseValue,
}

impl PMachine {
    pub fn new() -> Self {
        PMachine {
            ip: 0,
            imem: vec![],
            istack: vec![],
            dstack: vec![],
        }
    }

    pub fn load_imem(&mut self, imem: Vec<u8>) {
        self.imem = imem;
    }

    pub fn run(&mut self) -> Result<(), RuntimeError> {
        loop {
            match self.imem[self.ip] {
                val if val == Opcode::Break as u8 => {
                    break;
                }
                val if val == Opcode::Push as u8 => {
                    let bytes = self.imem[self.ip + 1..self.ip + 5]
                        .try_into()
                        .map_err(|_e| RuntimeError::FailedToParseValue)?;
                    let v = u32::from_be_bytes(bytes);
                    self.dpush(v);
                    self.ip += 5;
                }
                val if val == Opcode::Pop as u8 => {
                    let a = self.dpop().ok_or(RuntimeError::EmptyStack)?;
                    let b = self.dpop().ok_or(RuntimeError::EmptyStack)?;
                    self.dpush(a + b);
                    self.ip += 1;
                }
                val if val == Opcode::Add as u8 => {
                    let a = self.dpop().ok_or(RuntimeError::EmptyStack)?;
                    let b = self.dpop().ok_or(RuntimeError::EmptyStack)?;
                    self.dpush(a + b);
                    self.ip += 1;
                }
                val if val == Opcode::Sub as u8 => {
                    let a = self.dpop().ok_or(RuntimeError::EmptyStack)?;
                    let b = self.dpop().ok_or(RuntimeError::EmptyStack)?;
                    self.dpush(a - b);
                    self.ip += 1;
                }
                val if val == Opcode::Mul as u8 => {
                    let a = self.dpop().ok_or(RuntimeError::EmptyStack)?;
                    let b = self.dpop().ok_or(RuntimeError::EmptyStack)?;
                    self.dpush(a * b);
                    self.ip += 1;
                }
                val if val == Opcode::Div as u8 => {
                    let a = self.dpop().ok_or(RuntimeError::EmptyStack)?;
                    let b = self.dpop().ok_or(RuntimeError::EmptyStack)?;
                    self.dpush(a / b);
                    self.ip += 1;
                }
                _ => {}
            }
        }
        Ok(())
    }
}

pub fn read_file(filepath: &str) -> io::Result<Vec<u8>> {
    let mut file = File::open(filepath)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;
    Ok(buffer)
}

#[derive(Debug)]
pub enum DeassembleError {
    UnexpectedOpcode,
    FailedToParseValue,
}

pub fn deassemble(code: Vec<u8>) -> Result<(), DeassembleError> {
    let mut addr = 0x0;
    while addr < code.len() {
        match code[addr] {
            val if val == Opcode::Break as u8 => {
                addr += 1;
                println!("{addr:08x}   break");
            }
            val if val == Opcode::Push as u8 => {
                let bytes = code[addr + 1..addr + 5]
                    .try_into()
                    .map_err(|_e| DeassembleError::FailedToParseValue)?;
                let v = u32::from_be_bytes(bytes);
                addr += 5;
                println!("{addr:08x}   push {v:#x}");
            }
            val if val == Opcode::Pop as u8 => {
                addr += 1;
                println!("{addr:08x}   pop");
            }
            val if val == Opcode::Add as u8 => {
                addr += 1;
                println!("{addr:08x}   add");
            }
            val if val == Opcode::Sub as u8 => {
                addr += 1;
                println!("{addr:08x}   sub");
            }
            val if val == Opcode::Mul as u8 => {
                addr += 1;
                println!("{addr:08x}   mul");
            }
            val if val == Opcode::Div as u8 => {
                addr += 1;
                println!("{addr:08x}   div");
            }
            _ => {}
        }
    }
    Ok(())
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <file_path>", args[0]);
        std::process::exit(1);
    }

    let mut pm = PMachine::new();
    let imem = read_file(&args[1])?;
    pm.load_imem(imem);
    let _ = pm.run();

    Ok(())
}
