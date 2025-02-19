#![allow(non_camel_case_types)]
#![allow(unused)]

use std::{
    fs,
    io::{self, Write},
    process::{Command, Stdio},
    thread::spawn,
};

use colored::Colorize;

use regex::{
    dfs::{DotPrintable, FiniteAutomata, SymbolType},
    {minimization, determinization, thompson},
};

const REGEX_OUTPUT: i32 = 1;
const REGEX_OUTPUT_GRAPH: i32 = 2;
const CHECK_STRING: i32 = 3;
const CHANGE_REGEX: i32 = 4;
const EXIT: i32 = 5;

fn main() {
    let mut regex: Option<Regex> = None;
    let mut automata: Option<FiniteAutomata> = None;

    loop {
        println!("1. Вывести регулярное выражение");
        println!("2. Вывести конечный автомат");
        println!("3. Проверить допускает ли автомат строку");
        println!("4. Изменить регулярное выражение");
        println!("5. Выход");

        let mut choice = String::new();
        print!("Выберите пункт меню: ");
        io::stdout().flush().unwrap();
        io::stdin().read_line(&mut choice).expect("Ошибка чтения строки");

        match choice.trim().parse::<i32>() {
            Ok(REGEX_OUTPUT) => {
                if let Some(r) = regex.as_ref() {
                    println!("Текущее регулярное выражение: {}", r.to_string().bold());
                } else {
                    println!("{}", "Регулярное выражение еще не установлено".red());
                }
            }
            Ok(REGEX_OUTPUT_GRAPH) => {
                if let Some(r) = regex.as_ref() {
                    println!("Текущее регулярное выражение: {}", r.to_string().bold());
                    println!("Граф автомата: ");
                    let string_graph = automata.as_ref().unwrap().to_dot_notation();

                    let output_filename = "output.svg";
                    let mut child = Command::new("dot")
                        .stdin(Stdio::piped())
                        .stdout(Stdio::piped())
                        .arg("-Tsvg")
                        .spawn()
                        .expect("Ошибка при запуске команды dot");

                    let mut stdin = child.stdin.take().expect("Ошибка открытия канала");
                    std::thread::spawn(move || {
                        stdin.write_all(string_graph.as_bytes()).expect("Ошибка записи в поток ввода");
                    });

                    let output = child.wait_with_output().expect("Ошибка чтения из потока вывода");
                    fs::write(output_filename, &output.stdout).unwrap();
                    Command::new("pix")
                        .arg(&output_filename)
                        .spawn()
                        .expect("Ошибка при запуске команды pix");
                } else {
                    println!("{}", "Регулярное выражение еще не установлено".red());
                }
            }
            Ok(CHECK_STRING) => {
                if let Some(r) = regex.as_ref() {
                    let mut string_input = String::new();
                    print!("Введите строку: ");
                    io::stdout().flush().unwrap();
                    io::stdin().read_line(&mut string_input).expect("Ошибка чтения строки");
                    if string_input.ends_with('\n') {
                        string_input.pop();
                    }
                    if string_input.ends_with('\r') {
                        string_input.pop();
                    }
                    if automata.as_ref().unwrap().is_match(&string_input) {
                        println!("{}", "Строка соответствует регулярному выражению".green());
                    } else {
                        println!("{}", "Строка не соответствует регулярному выражению".red());
                    }
                } else {
                    println!("{}", "Регулярное выражение еще не установлено".red());
                }
            }
            Ok(CHANGE_REGEX) => {
                if let Some(r) = regex.as_ref() {
                    println!("Текущее регулярное выражение: {}", r.to_string().bold());
                }
                let mut regex_input = String::new();
                print!("Введите новое регулярное выражение: ");
                io::stdout().flush().unwrap();
                io::stdin().read_line(&mut regex_input).expect("Ошибка чтения");
                let new_regex = Regex::new(regex_input.trim()).expect(&format!("{}", "Некорректное регулярное выражение".red()));
                let fa = thompson::from_regexp(&new_regex);
                let no_eps_fa = determinization::remove_eps(fa);
                let det_fa = determinization::determ(no_eps_fa);
                let min_fa = minimization::minimize(det_fa);
                automata = Some(min_fa);
                regex = Some(new_regex);
            }
            Ok(EXIT) => {
                break;
            }
            _ => {
                println!("{}", "Введите значение какой-нибудь функции из пространства Лебега в точке 0");
            }
        }
    }
}
