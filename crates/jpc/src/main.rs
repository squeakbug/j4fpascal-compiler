mod riscv;

use std::{
    env,
    io::{self, Read, Write},
    fs::File,
};

use riscv::codegen::CodeEmmiter;

pub fn read_file(filepath: &str) -> io::Result<String> {
    let mut file = File::open(filepath)?;
    let mut buffer = String::new();
    file.read_to_string(&mut buffer)?;
    Ok(buffer)
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <file_path>", args[0]);
        std::process::exit(1);
    }

    let mut gen = CodeEmmiter::new();
    let source = read_file(&args[1])?;
    let result = gen.codegen(&source).expect("Failed to codegen");

    Ok(())
}