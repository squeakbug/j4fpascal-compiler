mod interpreter;

use std::io::{self, Write};

use interpreter::Interpreter;

fn main() {
    println!("j4fpascal 0.0.1. Type \"help\", \"copyright\", \"credits\" or \"license\" for more information. Type \"exit\" to quit.");

    let mut interpreter = Interpreter::new();

    loop {
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

        match interpreter.eval(&input) {
            Ok(result) => println!("Result: {}", result),
            Err(e) => println!("Error: {:?}", e),
        }
    }
}
