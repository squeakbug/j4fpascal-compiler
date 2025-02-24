use std::{collections::HashMap, iter::Peekable, str::Chars};

use lazy_static::lazy_static;
use thiserror::Error;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenType {
    // Unsigned constant
    UnsignedInteger(String),
    UnsignedReal(String),

    // Bool
    True,
    False,

    StringLiteral(String),
    Colon,
    Semicolon,
    LeftParen,
    RightParen,
    LeftBrack,
    RightBrack,
    Eof,
    Nil,

    // Relational operator
    Equal,
    NotEqual,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,

    // Additive operator
    Plus,
    Minus,
    Or,

    // Multiplicative operator
    Star,
    Slash,
    Div,
    Mod,
    And,

    // Other operators
    Assignment,
    Not,
    Comma,
    Bleat,
    DoubleBleat,
    Dot,
    DotDot,
    Pointer2, // '^'

    // Builtin types
    Boolean,
    Char,
    Integer,
    Real,
    Set,
    String,

    // Identifier
    Identifier(String),

    // Keywords
    Array,
    Begin,
    Break,
    Case,
    Const,
    Continue,
    Do,
    Else,
    End,
    For,
    Function,
    Goto,
    If,
    In,
    Is,
    Label,
    Of,
    Packed,
    Procedure,
    Program,
    Record,
    Repeat,
    Then,
    To,
    Type,
    Until,
    Var,
    While,
    With,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub pos: usize,
}

pub struct Lexer<'stream> {
    istream: Option<Peekable<Chars<'stream>>>,
    ostream: Vec<Token>,
    lexeme_start: Option<Peekable<Chars<'stream>>>,
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
        m.insert("and", TokenType::And);
        m.insert("array", TokenType::Array);
        m.insert("begin", TokenType::Begin);
        m.insert("boolean", TokenType::Boolean);
        m.insert("case", TokenType::Case);
        m.insert("char", TokenType::Char);
        m.insert("const", TokenType::Const);
        m.insert("div", TokenType::Div);
        m.insert("do", TokenType::Do);
        m.insert("else", TokenType::Else);
        m.insert("end", TokenType::End);
        m.insert("false", TokenType::False);
        m.insert("for", TokenType::For);
        m.insert("function", TokenType::Function);
        m.insert("goto", TokenType::Goto);
        m.insert("if", TokenType::If);
        m.insert("in", TokenType::In);
        m.insert("is", TokenType::Is);
        m.insert("integer", TokenType::Integer);
        m.insert("label", TokenType::Label);
        m.insert("mod", TokenType::Mod);
        m.insert("nil", TokenType::Nil);
        m.insert("not", TokenType::Not);
        m.insert("of", TokenType::Of);
        m.insert("or", TokenType::Or);
        m.insert("packed", TokenType::Packed);
        m.insert("procedure", TokenType::Procedure);
        m.insert("program", TokenType::Program);
        m.insert("real", TokenType::Real);
        m.insert("record", TokenType::Record);
        m.insert("repeat", TokenType::Repeat);
        m.insert("set", TokenType::Set);
        m.insert("string", TokenType::String);
        m.insert("then", TokenType::Then);
        m.insert("to", TokenType::To);
        m.insert("to", TokenType::True);
        m.insert("type", TokenType::Type);
        m.insert("until", TokenType::Until);
        m.insert("var", TokenType::Var);
        m.insert("while", TokenType::While);
        m.insert("with", TokenType::With);
        m
    };
}

// TODO: implement Iter trait for Lexer
impl<'stream> Lexer<'stream> {
    pub fn new() -> Self {
        Lexer {
            istream: None,
            ostream: Vec::new(),
            lexeme_start: None,
            pos: 0,
        }
    }

    fn advance(&mut self) -> Option<char> {
        self.pos += 1;
        self.istream.as_mut().unwrap().next()
    }

    fn peek(&mut self) -> Option<char> {
        self.istream.as_mut().unwrap().peek().map(|c| c.to_owned())
    }

    /// Inserts token to output stream, adding current position
    fn add_token(&mut self, token_type: TokenType) {
        self.ostream.push(Token {
            token_type,
            pos: self.pos,
        });
    }

    fn space(&mut self) {
        while let Some(c) = self.peek() {
            match c {
                ' ' | '\t' | '\n' => {
                    let _ = self.advance();
                }
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

    // Упростили парсер за счет усложнения лексера
    // TODO: add hex and octal separators
    fn number(&mut self) {
        let mut length = 0;
        let mut is_float = false;
        self.lexeme_start = Some(self.istream.clone().unwrap());
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
            is_float = true;
            while let Some(c) = self.peek() {
                if c.is_digit(10) {
                    length += 1;
                    self.advance();
                } else {
                    break;
                }
            }
        }

        if length > 0 {
            let iter = self.lexeme_start.take().expect("UNREACHABLE");
            let literal = iter.take(length).collect::<String>();
            if is_float {
                self.add_token(TokenType::UnsignedReal(literal))
            } else {
                self.add_token(TokenType::UnsignedInteger(literal))
            }
        }
    }

    fn string_literal(&mut self) {
        if let Some('\"') = self.peek() {
            let mut length = 0;
            self.lexeme_start = Some(self.istream.clone().unwrap());
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

    fn keyword_or_identifier(&mut self) {
        let mut length = 0;
        self.lexeme_start = Some(self.istream.clone().unwrap());
        while let Some(c) = self.peek() {
            if c.is_alphanumeric() {
                self.advance();
                length += 1;
            } else {
                break;
            }
        }

        if length > 0 {
            let iter = self.lexeme_start.take().expect("UNREACHABLE");
            let literal = iter.take(length).collect::<String>();
            if let Some(token_type) = KEYWORDS.get(literal.as_str()) {
                self.add_token(token_type.clone());
            } else {
                self.add_token(TokenType::Identifier(literal));
            }   
        }
    }

    fn operator(&mut self) -> Option<bool> {
        let ch = self.peek()?;
        match ch {
            '+' => self.add_token(TokenType::Plus),
            '-' => self.add_token(TokenType::Minus),
            '*' => self.add_token(TokenType::Star),
            '/' => self.add_token(TokenType::Slash),
            '(' => self.add_token(TokenType::LeftParen),
            ')' => self.add_token(TokenType::RightParen),
            '[' => self.add_token(TokenType::LeftBrack),
            ']' => self.add_token(TokenType::RightBrack),
            '=' => self.add_token(TokenType::Equal),
            ',' => self.add_token(TokenType::Comma),
            '<' => {
                self.advance();
                match self.peek() {
                    Some('=') => self.add_token(TokenType::LessEqual),
                    Some('>') => self.add_token(TokenType::NotEqual),
                    _ => self.add_token(TokenType::Less),
                }
            },
            '>' => {
                self.advance();
                match self.peek() {
                    Some('=') => self.add_token(TokenType::GreaterEqual),
                    _ => self.add_token(TokenType::Greater),
                }
            },
            ':' => {
                self.advance();
                match self.peek() {
                    Some('=') => self.add_token(TokenType::Assignment),
                    _ => self.add_token(TokenType::Colon),
                }
            },
            '.' => {
                self.advance();
                match self.peek() {
                    Some('.') => self.add_token(TokenType::DotDot),
                    _ => self.add_token(TokenType::Dot),
                }
            },
            '@' => {
                self.advance();
                match self.peek() {
                    Some('@') => self.add_token(TokenType::DoubleBleat),
                    _ => self.add_token(TokenType::Bleat),
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
        self.keyword_or_identifier();
    }

    // TODO: must return iterator instead of Vec
    pub fn lex(&mut self, istream: &'stream str) -> Result<Vec<Token>, LexerError> {
        self.istream = Some(istream.chars().peekable());
        while self.peek().is_some() {
            self.lex_token();
        }
        self.add_token(TokenType::Eof);
        self.istream = None;
        Ok(self.ostream.clone())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lex_arith() {
        let mut lexer = Lexer::new();
        assert_eq!(
            lexer.lex("y = x / (1 + 2) * 4").unwrap()
                .into_iter()
                .map(|token| token.token_type).collect::<Vec<_>>(), 
            vec![
                TokenType::Identifier(String::from("y")),
                TokenType::Assignment,
                TokenType::Identifier(String::from("x")),
                TokenType::Slash,
                TokenType::LeftParen,
                TokenType::UnsignedInteger(String::from("1")), 
                TokenType::Plus, 
                TokenType::UnsignedInteger(String::from("2")),
                TokenType::RightParen,
                TokenType::Star,
                TokenType::UnsignedInteger(String::from("4")),
                TokenType::Eof
            ]
        );
    }
}

// Ошибки вида "if a !asf b" выявляется на этапе
// синтаксического анализа. Лексер отдает только токены:
// ["if", "a", "!", "asf" , "b"]
