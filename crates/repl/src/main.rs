use std::io::{self, Write};

use frontend::parser::AST;
pub use frontend::{lexer, parser};

struct Interpreter {

}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {}
    }

    fn interpret_ast(&mut self, ast: &AST) {
        
    }

    pub fn eval(&mut self, exp: &str) -> Result<String, String> {
        let tokens = lexer::lex(exp).unwrap();
        let ast = parser::parse(&x).unwrap();
        self.interpret_ast(ast);
        todo!()
    }
}

fn main() {
    println!("j4fpascal 0.0.1. Type \"help\", \"copyright\", \"credits\" or \"license\" for more information. Type \"exit\" to quit.");

    let mut interpreter = Interpreter::new();

    loop {
        print!(">>> ");
        io::stdout().flush().unwrap();
        let mut input = String::new();
        io::stdin().read_line(&mut input).expect("Failed to read line");

        if input.trim() == "exit" {
            break;
        }

        match interpreter.eval(&input) {
            Ok(result) => println!("Result: {}", result),
            Err(e) => println!("Error: {}", e),
        }
    }
}
