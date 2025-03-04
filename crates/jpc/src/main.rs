mod riscv;
mod hir;

use std::{
    env, fs::{self, File}, io::{self, Read}
};

use clap::{Arg, Command, CommandFactory, Parser};
use hir::codegen::CodeEmmiter;

use core::{lexer, parser};

/// Simple program to mimic the rustc compiler
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Input file to compile
    #[arg(long)]
    input: Option<String>,

    /// Comma separated list of types of output for the compiler to emit
    #[arg(long)]
    emit: Option<String>,

    /// Equivalent to -C debuginfo=2
    #[arg(short = 'g', long)]
    debug_info: bool,

    /// Equivalent to -C opt-level=2
    #[arg(short = 'O', long)]
    optimize: bool,

    /// Write output to <filename>
    #[arg(short = 'o', long)]
    output: Option<String>,
}

pub fn read_file(filepath: &str) -> io::Result<String> {
    let mut file = File::open(filepath)?;
    let mut buffer = String::new();
    file.read_to_string(&mut buffer)?;
    Ok(buffer)
}

fn main() -> io::Result<()> {
    let args = Args::parse();

    if let Some(input) = args.input {
        let mut gen = CodeEmmiter::new();
        let source = read_file(&input)?;

        let mb_tokens = lexer::Lexer::new(source.chars()).collect::<Vec<_>>();
        let tokens = mb_tokens.into_iter().collect::<Result<Vec<_>, _>>()
            .expect("Failed to scan");
        fs::write("tokens.txt", format!("{:#?}", &tokens)).unwrap();

        let mut parser = parser::Parser::new(tokens.into_iter());
        let ast = parser.parse().expect("Failed to parse");
        fs::write("ast.txt", format!("{:#?}", &ast)).unwrap();

        gen.visit_program(&Box::new(ast)).expect("Failed to codegen");
    } else {
        let _ = Args::command().print_help();
    }

    Ok(())
}