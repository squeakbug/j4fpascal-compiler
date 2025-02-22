#![allow(non_camel_case_types)]
#![allow(unused)]

use std::{
    fs,
    io::{self, Write},
    process::{Command, Stdio},
    thread::spawn,
};

use colored::Colorize;

const REGEX_OUTPUT: i32 = 1;
const REGEX_OUTPUT_GRAPH: i32 = 2;
const CHECK_STRING: i32 = 3;
const CHANGE_REGEX: i32 = 4;
const EXIT: i32 = 5;

fn main() {
    let mut regex: Option<regex::Regex> = None;

    loop {
        println!("1. Вывести регулярное выражение");
        println!("2. Проверить допускает ли автомат строку");
        println!("3. Изменить регулярное выражение");
        println!("4. Выход");

        let mut choice = String::new();
        print!("Выберите пункт меню: ");
        io::stdout().flush().unwrap();
        io::stdin()
            .read_line(&mut choice)
            .expect("Ошибка чтения строки");

        match choice.trim().parse::<i32>() {
            Ok(REGEX_OUTPUT) => {
                if let Some(ref regex) = regex {
                    println!("Текущее регулярное выражение: {}", regex.as_str().bold());
                } else {
                    println!("{}", "Регулярное выражение еще не установлено".red());
                }
            }
            Ok(CHECK_STRING) => {
                if let Some(ref regex) = regex {
                    let mut string_input = String::new();
                    print!("Введите строку: ");
                    io::stdout().flush().unwrap();
                    io::stdin()
                        .read_line(&mut string_input)
                        .expect("Ошибка чтения строки");
                    if string_input.ends_with('\n') {
                        string_input.pop();
                    }
                    if string_input.ends_with('\r') {
                        string_input.pop();
                    }
                    if regex.is_match(&string_input) {
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
                    println!("Текущее регулярное выражение: {}", r.as_str().bold());
                }
                let mut regex_input = String::new();
                print!("Введите новое регулярное выражение: ");
                io::stdout().flush().unwrap();
                io::stdin()
                    .read_line(&mut regex_input)
                    .expect("Ошибка чтения");
                let trimmed_regex = regex_input.trim();
                let new_regex = regex::Regex::new(trimmed_regex)
                    .expect(&format!("{}", "Некорректное регулярное выражение".red()));
                regex = Some(new_regex);
            }
            Ok(EXIT) => {
                break;
            }
            _ => {
                println!(
                    "{}",
                    "Введите значение какой-нибудь функции из пространства Лебега в точке 0"
                );
            }
        }
    }
}
