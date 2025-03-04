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
    /// [tok|ast|hir]
    /// list of types of output for the compiler to emit
    emit: Vec<String>,

    /// Equivalent to -C debuginfo=2
    #[arg(short = 'g', long)]
    debug_info: bool,

    /// Equivalent to -C opt-level=2
    #[arg(short = 'O', long)]
    optimize: bool,

    /// Write output to <filename>
    #[arg(short = 'o', long)]
    output: Option<String>,

    /// Input file to compile
    input: Option<String>,
}

pub fn read_file(filepath: &str) -> io::Result<String> {
    let mut file = File::open(filepath)?;
    let mut buffer = String::new();
    file.read_to_string(&mut buffer)?;
    Ok(buffer)
}

fn main() -> io::Result<()> {
    let args = Args::parse();

    let mut is_need_tok_out = false;
    let mut is_need_ast_out = false;
    let mut is_need_hir_out = false;
    for emit_type in args.emit {
        match emit_type.as_str() {
            "tok" => is_need_tok_out = true,
            "ast" => is_need_ast_out = true,
            "hir" => is_need_hir_out = true,
            _ => println!("Unknown")
        }
    }

    if let Some(input) = args.input {
        let mut gen = CodeEmmiter::new();
        let source = read_file(&input)?;

        let mb_tokens = lexer::Lexer::new(source.chars()).collect::<Vec<_>>();
        let tokens = mb_tokens.into_iter().collect::<Result<Vec<_>, _>>()
            .expect("Failed to scan");
        if is_need_tok_out {
            if let Err(err) = fs::write(format!("{input}.tok"), format!("{:#?}", &tokens)) {
                println!("Failed to output tokens: {err:?}");
            }
        }

        let mut parser = parser::Parser::new(tokens.into_iter());
        let ast = parser.parse().expect("Failed to parse");
        if is_need_tok_out {
            if let Err(err) = fs::write(format!("{input}.ast"), format!("{:#?}", &ast)) {
                println!("Failed to output AST: {err:?}");
            }
        }

        gen.visit_program(&Box::new(ast)).expect("Failed to codegen");
        if is_need_hir_out {
            if let Err(err) = fs::write(format!("{input}.hir"), format!("{:#?}", &ast)) {
                println!("Failed to output AST: {err:?}");
            }
        }
    } else {
        let _ = Args::command().print_help();
    }

    Ok(())
}