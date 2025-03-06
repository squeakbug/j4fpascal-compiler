mod callable;
mod interpreter;

use std::{
    env,
    fs::{self, File},
    io::{self, BufWriter, Read, Write},
};

use camino::Utf8PathBuf;
use clap::{Arg, Command, CommandFactory, Parser};
use core::{error::Error, lexer, parser};

use crate::interpreter::Interpreter;

/// Simple program to mimic the rustc compiler
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// [tok|ast|hir]
    /// list of types of output for the compiler to emit
    #[arg(long)]
    emit: Vec<String>,

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
    let mut interpreter = Interpreter::new();

    let mut is_need_tok_out = false;
    let mut is_need_ast_out = false;
    let mut is_need_hir_out = false;
    for emit_type in args.emit {
        match emit_type.as_str() {
            "tok" => is_need_tok_out = true,
            "ast" => is_need_ast_out = true,
            "hir" => is_need_hir_out = true,
            _ => println!("Unknown"),
        }
    }

    let mut eval = |ifilename: Option<String>, exp: &str| {
        let ifilename = ifilename.unwrap_or(String::from("stdin"));
        let mb_tokens = lexer::Lexer::new(exp.chars()).collect::<Vec<_>>();
        let tokens = mb_tokens
            .into_iter()
            .collect::<Result<Vec<_>, _>>()
            .map_err(|error| Error::Lexer {
                path: Utf8PathBuf::from(exp),
                src: exp.to_string(),
                error,
            })
            .unwrap();
        if is_need_tok_out {
            if let Err(err) = fs::write(format!("{ifilename}.tok"), format!("{:#?}", &tokens)) {
                println!("Failed to output tokens: {err:?}");
            }
        }

        let mut parser = parser::Parser::new(tokens.into_iter());
        let ast = parser
            .parse()
            .map_err(|err| Error::Parser {
                path: Utf8PathBuf::from(exp),
                src: exp.to_string(),
                error: err,
            })
            .unwrap();
        if is_need_ast_out {
            if let Err(err) = fs::write(format!("{ifilename}.ast"), format!("{:#?}", &ast)) {
                println!("Failed to output AST: {err:?}");
            }
        }

        interpreter.visit_program(&Box::new(ast)).unwrap()
    };

    if let Some(input) = args.input {
        let source = read_file(&input)?;
        eval(Some(String::from(input)), &source);
    } else {
        loop {
            println!("j4fpascal 0.0.1. Type \"help\", \"copyright\", \"credits\" or \"license\" for more information. Type \"exit\" to quit.");
            print!("jpci> ");
            io::stdout().flush().unwrap();
            let mut input = String::new();
            io::stdin()
                .read_line(&mut input)
                .expect("Failed to read line");

            if input.trim() == "exit" {
                println!("Leaving JPCi.");
                break;
            }
            eval(None, &input);
        }
    }

    Ok(())
}
