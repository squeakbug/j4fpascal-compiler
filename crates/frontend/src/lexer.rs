use std::{
    collections::HashMap, iter::Peekable, str::Chars
};

use thiserror::Error;
use lazy_static::lazy_static;

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    True, False,
    Number(f64),
    StringLiteral(String),
    Semicolon,
    Comma,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    EOF,

    // Operators
    Plus, Minus, Bang,
    Star, Slash,
    Less, Greater,
    LessEqual, GreaterEqual,
    Percent,
    Equal, NotEqual,
    And, Or, Not,
    Assignment,

    // Keywords
    If, While, For, Until, Begin, End, Of, Return,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub pos: usize,
}

pub struct Lexer<'a> {
    istream: Peekable<Chars<'a>>,
    ostream: Vec<Token>,
    lexeme_start: Option<Peekable<Chars<'a>>>,
    pos: usize,
}

#[derive(Debug, Error)]
pub enum LexerError {
    #[error("Unexpected character '{0}' at position {1}")]
    UnexpectedCharacter(char, usize),

    #[error("Invalid number format at position {0}")]
    InvalidNumberFormat(usize),

    #[error("Unterminated string literal at position {0}")]
    UnterminatedStringLiteral(usize),

    #[error("Unknown token at position {0}")]
    UnknownToken(usize),

    #[error("Unexpected EOF at position {0}")]
    UnexpectedEOF(usize),
}

// TODO: change to const expression from modern Rust
lazy_static! {
    static ref KEYWORDS: HashMap<&'static str, TokenType> = {
        let mut m = HashMap::new();
        m.insert("if", TokenType::If);
        m.insert("while", TokenType::While);
        m.insert("for", TokenType::For);
        m.insert("until", TokenType::Until);
        m.insert("begin", TokenType::Begin);
        m.insert("end", TokenType::End);
        m.insert("of", TokenType::Of);
        m.insert("return", TokenType::Return);
        m
    };
}

// TODO: implement Iter trait for Lexer
impl<'a> Lexer<'a> {
    pub fn new(stream: &'a str) -> Self {
        Lexer {
            istream: stream.chars().peekable(),
            ostream: Vec::new(),
            lexeme_start: None,
            pos: 0,
        }
    }

    fn advance(&mut self) -> Option<char> {
        self.pos += 1;
        self.istream.next()
    }

    fn peek(&mut self) -> Option<char> {
        self.istream.peek().map(|c| c.to_owned())
    }

    /// Inserts token to output stream, adding current position
    fn add_token(&mut self, token_type: TokenType) {
        self.ostream.push(Token { token_type, pos: self.pos });
    }

    fn space(&mut self) {
        while let Some(c) = self.peek() {
            match c {
                ' ' | '\t' | '\n' => { let _ =  self.advance(); },
                _ => break,
            }
        }
    }

    fn separator(&mut self) {
        if let Some(';') = self.peek() {
            self.add_token(TokenType::Semicolon);
            self.advance();
        }
    }

    // TODO: add hex and octal separators
    fn number(&mut self) {
        let mut length = 0;
        self.lexeme_start = Some(self.istream.clone());
        while let Some(c) = self.peek() {
            if c.is_digit(10) {
                length += 1;
                self.advance();
            } else {
                break;
            }
        }
        if let Some('.') = self.peek() {
            length += 1;
            while let Some(c) = self.peek() {
                if c.is_digit(10) {
                    length += 1;
                    self.advance();
                } else {
                    break;
                }
            }
        }

        let iter = self.lexeme_start.take().expect("UNREACHABLE");
        let literal = iter.take(length).collect::<String>();
        match literal.parse::<f64>() {
            Ok(number) => self.add_token(TokenType::Number(number)),
            Err(_) => { }
        };
    }

    fn string_literal(&mut self) {
        if let Some('\"') = self.peek() {
            let mut length = 0;
            self.lexeme_start = Some(self.istream.clone());
            self.advance();
            while let Some(c) = self.advance() {
                if c == '\"' {
                    let iter = self.lexeme_start.take().expect("UNREACHABLE");
                    let literal = iter.take(length).collect::<String>();
                    self.add_token(TokenType::StringLiteral(literal));
                    break;
                }
                length += 1;
            }
        }
    }

    fn keyword(&mut self) {
        let mut length = 0;
        self.lexeme_start = Some(self.istream.clone());
        while let Some(c) = self.peek() {
            if c.is_alphanumeric() {
                self.advance();
                length += 1;
            } else {
                break;
            }
        }

        let iter = self.lexeme_start.take().expect("UNREACHABLE");
        let literal = iter.take(length).collect::<String>();
        if let Some(token_type) = KEYWORDS.get(literal.as_str()) {
            self.add_token(token_type.clone());
        }
    }

    fn operator(&mut self) -> Option<bool> {
        let ch = self.istream.peek()?;
        match ch {
            '+' => self.add_token(TokenType::Plus),
            '-' => self.add_token(TokenType::Minus),
            '*' => self.add_token(TokenType::Star),
            '/' => self.add_token(TokenType::Minus),
            '<' => {
                match self.istream.next() {
                    Some('=') => self.add_token(TokenType::LessEqual),
                    _ => self.add_token(TokenType::Less),
                }
            },
            '>' => {
                match self.istream.next() {
                    Some('=') => self.add_token(TokenType::GreaterEqual),
                    _ => self.add_token(TokenType::Greater),
                }
            },
            '=' => {
                match self.istream.next() {
                    Some('=') => self.add_token(TokenType::Equal),
                    _ => self.add_token(TokenType::Assignment),
                }
            },
            '!' => {
                match self.istream.next() {
                    Some('=') => self.add_token(TokenType::GreaterEqual),
                    _ => self.add_token(TokenType::Greater),
                }
            },
            _ => return Some(false),
        }
        self.advance();
        return Some(true);
    }

    fn lex_token(&mut self) {
        self.space();
        self.separator();
        self.operator();
        self.string_literal();
        self.number();
        self.keyword();
    }

    // TODO: must return iterator instead of Vec
    pub fn lex(&mut self) -> Result<Vec<Token>, LexerError> {
        while self.peek().is_some() {
            self.lex_token();
        }
        self.add_token(TokenType::EOF);
        Ok(self.ostream.clone())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lex_arith() {
        let mut lexer = Lexer::new("1+ 2");
        assert_eq!(lexer.lex().unwrap().len(), 4);
    }
}
