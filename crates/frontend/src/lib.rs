#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    Plus,
    Minus,
    Mul,
    Div,
    Var(String),
    Const(i32),
}

pub fn lex(stream: &str) -> Result<Vec<Token>, ()> {
    let mut result = vec![];
    let mut current_var = String::new();
    let mut chars = stream.chars();
    
    while let Some(c) = chars.next() {
        match c {
            '+' => result.push(Token::Plus),
            '-' => result.push(Token::Minus),
            '*' => result.push(Token::Mul),
            '/' => result.push(Token::Div),
            '0'..='9' => {
                let mut number = c.to_string();
                while let Some(next_char) = chars.next() {
                    if next_char.is_digit(10) {
                        number.push(next_char);
                    } else {
                        break;
                    }
                }

                if let Ok(value) = number.parse::<i32>() {
                    result.push(Token::Const(value));
                }
            },
            'a'..='z' | 'A'..='Z' => {
                current_var.push(c);
            },
            ' ' | '\n' | '\t' => {
                continue;
            },
            _ => {
                return Err(());
            }
        }
    }

    if !current_var.is_empty() {
        result.push(Token::Var(current_var));
    }

    Ok(result)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lex_arith() {
        assert_eq!(
            lex("1 + 2").unwrap(), 
            vec![Token::Const(1), Token::Plus, Token::Const(2)]
        );

        assert_eq!(
            lex("a + b").unwrap(), 
            vec![
                Token::Var(String::from("a")), 
                Token::Plus, 
                Token::Var(String::from("b"))
            ]
        );
    }
}
