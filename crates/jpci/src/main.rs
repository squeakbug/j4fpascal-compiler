mod interpreter;

use std::{fs::File, io::{self, Read, Write}};

use interpreter::Interpreter;

pub fn read_file(filepath: &str) -> io::Result<String> {
    let mut file = File::open(filepath)?;
    let mut buffer = String::new();
    file.read_to_string(&mut buffer)?;
    Ok(buffer)
}

fn main() -> io::Result<()> {
    println!("j4fpascal 0.0.1. Type \"help\", \"copyright\", \"credits\" or \"license\" for more information. Type \"exit\" to quit.");

    let mut interpreter = Interpreter::new();
    let args: Vec<String> = std::env::args().collect();
    if args.len() > 1 {
        let source = read_file(&args[1])?;
        interpreter.eval(&source).unwrap();
    } else {
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
                Ok(_) => println!(""),
                Err(e) => println!("Error: {:?}", e),
            }

            dbg!(&interpreter);
        }
    }

    Ok(())
}
