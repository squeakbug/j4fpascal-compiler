mod riscv;
mod hir;

use core::{lexer::Lexer, parser::Parser};
use std::{
    env, fs::{self, File}, io::{self, Read}
};
 
use hir::codegen::CodeEmmiter;

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

    let mb_tokens = Lexer::new(source.chars()).collect::<Vec<_>>();
    let tokens = mb_tokens.into_iter().collect::<Result<Vec<_>, _>>()
        .expect("Failed to scan");
    fs::write("tokens.txt", format!("{:#?}", &tokens)).unwrap();

    let mut parser = Parser::new(tokens.into_iter());
    let ast = parser.parse().expect("Failed to parse");
    fs::write("ast.txt", format!("{:#?}", &ast)).unwrap();

    gen.visit_program(&Box::new(ast)).expect("Failed to codegen");

    Ok(())
}