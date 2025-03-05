use std::path::Display;

pub type Result<Ok, Err = LexerError> = std::result::Result<Ok, Err>;

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

    // Identifier
    Identifier(String),

    // Keywords
    Array,
    Begin,
    Break,
    Case,
    Class,
    Const,
    Continue,
    Do,
    Else,
    End,
    File,
    For,
    Function,
    Goto,
    If,
    In,
    Interface,
    Is,
    Label,
    Object,
    Of,
    Packed,
    Procedure,
    Program,
    Record,
    Repeat,
    Set,
    Then,
    To,
    Type,
    Until,
    Var,
    While,
    With,

    // Comments
    Comment,
    CommentDoc { content: String },
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenType,
    pub start_pos: usize,
    pub end_pos: usize,
}

impl AsRef<Token> for Token {
    fn as_ref(&self) -> &Token {
        self
    }
}

impl From<&Token> for SrcSpan {
    fn from(value: &Token) -> Self {
        SrcSpan {
            start: value.start_pos,
            end: value.end_pos,
        }
    }
}

pub struct Lexer<T: Iterator<Item = char>> {
    istream: T,
    ostream: Vec<Token>,
    chr0: Option<char>,
    chr1: Option<char>,
    loc0: usize,
}

#[derive(Debug, Clone)]
pub struct SrcSpan {
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, Clone)]
pub struct LexerError {
    pub location: SrcSpan,
    pub kind: LexerErrorType,
}

impl LexerError {
    pub fn details(&self) -> (String, Vec<String>) {
        match &self.kind {
            LexerErrorType::UnrecognizedToken { tok } => (format!("Unrecognized token: '{tok}'"), vec![]),
            LexerErrorType::MissingExponent => ("Missing exponent".into(), vec![]),
            LexerErrorType::UnterminatedStringLiteral => ("Unterminated string literal".into(), vec![]),
            LexerErrorType::RadixIntNoValue => ("Radix int no value".into(), vec![]),
            LexerErrorType::DigitOutOfRadix => ("Digit out of radix".into(), vec![]),
            LexerErrorType::TooBigValue => ("Too big value".into(), vec![]),
        }
    }
}

#[derive(Debug, Clone)]
pub enum LexerErrorType {
    UnrecognizedToken { tok: char },  
    MissingExponent,                  // For example 100e - there is no number after 'e'
    UnterminatedStringLiteral,
    RadixIntNoValue,
    DigitOutOfRadix,
    TooBigValue,
}

// TODO: change to const expression from modern Rust
// I can use FSM or prefix tree (aka trie), but i would save not too much memory
// But maybe rust compiler will do this...
pub fn str_to_keyword(word: &str) -> Option<TokenType> {
    match word {
        "and" => Some(TokenType::And),
        "array" => Some(TokenType::Array),
        "begin" => Some(TokenType::Begin),
        "case" => Some(TokenType::Case),
        "class" => Some(TokenType::Class),
        "const" => Some(TokenType::Const),
        "div" => Some(TokenType::Div),
        "do" => Some(TokenType::Do),
        "else" => Some(TokenType::Else),
        "end" => Some(TokenType::End),
        "false" => Some(TokenType::False),
        "file" => Some(TokenType::File),
        "for" => Some(TokenType::For),
        "function" => Some(TokenType::Function),
        "goto" => Some(TokenType::Goto),
        "if" => Some(TokenType::If),
        "in" => Some(TokenType::In),
        "is" => Some(TokenType::Is),
        "interface" => Some(TokenType::Interface),
        "label" => Some(TokenType::Label),
        "mod" => Some(TokenType::Mod),
        "nil" => Some(TokenType::Nil),
        "not" => Some(TokenType::Not),
        "object" => Some(TokenType::Object),
        "of" => Some(TokenType::Of),
        "or" => Some(TokenType::Or),
        "packed" => Some(TokenType::Packed),
        "procedure" => Some(TokenType::Procedure),
        "program" => Some(TokenType::Program),
        "record" => Some(TokenType::Record),
        "repeat" => Some(TokenType::Repeat),
        "set" => Some(TokenType::Set),
        "then" => Some(TokenType::Then),
        "to" => Some(TokenType::To),
        "true" => Some(TokenType::True),
        "type" => Some(TokenType::Type),
        "until" => Some(TokenType::Until),
        "var" => Some(TokenType::Var),
        "while" => Some(TokenType::While),
        "with" => Some(TokenType::With),
        _ => None,
    }
}

// TODO: implement Iter trait for Lexer
impl<T> Lexer<T>
where 
    T: Iterator<Item = char>
{
    pub fn new(istream: T) -> Self {
        let mut lxr = Lexer {
            istream,
            ostream: Vec::new(),
            chr0: None,
            chr1: None,
            loc0: 0,
        };
        let _ = lxr.next_char();
        let _ = lxr.next_char();
        lxr
    }

    // Helper function to retrieve the current position.
    fn get_pos(&self) -> usize {
        self.loc0
    }

    fn peek(&mut self) -> Option<char> {
        self.chr0
    }

    fn peek_next(&mut self) -> Option<char> {
        self.chr1
    }

    fn next_char(&mut self) -> Option<char> {
        let c = self.chr0;
        let nxt = self.istream.next();
        self.chr0 = self.chr1;
        self.chr1 = nxt;
        self.loc0 += 1;
        c
    }

    fn add_tok1(&mut self, kind: TokenType) {
        self.add_tokn(kind, 1);
    }

    fn add_tokn(&mut self, kind: TokenType, n: usize) {
        self.ostream.push(Token {
            kind,
            start_pos: self.loc0 - n,
            end_pos: self.loc0,
        });
    }

    fn add_tok(&mut self, token: Token) {
        self.ostream.push(token);
    }

    fn is_space(&mut self, chr: char) -> bool {
        match chr {
            ' ' | '\t' | '\n' => true,
            _ => false,
        }
    }

    fn is_separator(&mut self, chr: char) -> bool {
        match chr {
            ';' => true,
            _ => false,
        }
    }

    // Consume a single character with the given radix.
    fn take_number(&mut self, radix: u32) -> Option<char> {
        let take_char = Lexer::<T>::is_digit_of_radix(self.chr0, radix);

        if take_char {
            Some(self.next_char().expect("take_number next char"))
        } else {
            None
        }
    }

    // Test if a digit is of a certain radix.
    fn is_digit_of_radix(c: Option<char>, radix: u32) -> bool {
        match radix {
            2 | 8 | 10 | 16 => c.filter(|c| c.is_digit(radix)).is_some(),
            other => panic!("Radix not implemented: {other}"),
        }
    }

    // Consume a sequence of numbers with the given radix,
    // the digits can be decorated with underscores
    // like this: '1_2_3_4' == '1234'
    fn radix_run(&mut self, radix: u32) -> String {
        let mut value_text = String::new();

        loop {
            if let Some(c) = self.take_number(radix) {
                value_text.push(c);
            } else if self.chr0 == Some('_') && Lexer::<T>::is_digit_of_radix(self.chr1, radix) {
                value_text.push('_');
                let _ = self.next_char();
            } else {
                break;
            }
        }
        value_text
    }

    fn lex_number_radix(&mut self, start_pos: usize, radix: u32, prefix: &str) -> Result<Token> {
        let num = self.radix_run(radix);
        if num.is_empty() {
            let location = self.get_pos() - 1;
            Err(LexerError {
                kind: LexerErrorType::RadixIntNoValue,
                location: SrcSpan {
                    start: location,
                    end: location,
                },
            })
        } else if radix < 16 && Lexer::<T>::is_digit_of_radix(self.chr0, 16) {
            let location = self.get_pos();
            Err(LexerError {
                kind: LexerErrorType::DigitOutOfRadix,
                location: SrcSpan {
                    start: location,
                    end: location,
                },
            })
        } else {
            let value = format!("{prefix}{num}");
            let end_pos = self.get_pos();
            Ok(Token {
                start_pos,
                kind: TokenType::UnsignedInteger(value),
                end_pos,
            })
        }
    }

    // Lex a normal number, that is, no octal, hex or binary number.
    // This function cannot be reached without the head of the stream being either 0-9 or '-', 0-9
    fn lex_decimal_number(&mut self) -> Result<Token> {
        self.lex_decimal_or_int_number(true)
    }

    fn lex_int_number(&mut self) -> Result<Token> {
        self.lex_decimal_or_int_number(false)
    }

    fn lex_decimal_or_int_number(&mut self, can_lex_decimal: bool) -> Result<Token> {
        let start_pos = self.get_pos();
        let mut value = String::new();
        // consume negative sign
        if self.chr0 == Some('-') {
            value.push(self.next_char().expect("lex_normal_number negative"));
        }
        // consume first run of digits
        value.push_str(&self.radix_run(10));

        // If float:
        if can_lex_decimal && self.chr0 == Some('.') {
            value.push(self.next_char().expect("lex_normal_number float"));
            value.push_str(&self.radix_run(10));

            // If scientific:
            if self.chr0 == Some('e') {
                value.push(self.next_char().expect("lex_normal_number scientific"));
                if self.chr0 == Some('-') {
                    value.push(
                        self.next_char()
                            .expect("lex_normal_number scientific negative"),
                    );
                }
                let exponent_run = self.radix_run(10);
                if exponent_run.is_empty() {
                    return Err(LexerError {
                        kind: LexerErrorType::MissingExponent,
                        location: SrcSpan { start: start_pos, end: self.get_pos() },
                    });
                }
                value.push_str(&exponent_run);
            }
            let end_pos = self.get_pos();
            Ok(Token {
                start_pos,
                kind: TokenType::UnsignedReal(value),
                end_pos,
            })
        } else {
            let end_pos = self.get_pos();
            Ok(Token {
                start_pos,
                kind: TokenType::UnsignedInteger(value),
                end_pos,
            })
        }
    }

    // Упростили парсер за счет усложнения лексера
    // TODO: add hex and octal separators
    fn lex_number(&mut self) -> Result<Token> {
        let start_pos = self.get_pos();
        let num = if self.chr0 == Some('0') {
            if self.chr1 == Some('x') || self.chr1 == Some('X') {
                // Hex!
                let _ = self.next_char();
                let _ = self.next_char();
                self.lex_number_radix(start_pos, 16, "0x")?
            } else if self.chr1 == Some('o') || self.chr1 == Some('O') {
                // Octal!
                let _ = self.next_char();
                let _ = self.next_char();
                self.lex_number_radix(start_pos, 8, "0o")?
            } else if self.chr1 == Some('b') || self.chr1 == Some('B') {
                // Binary!
                let _ = self.next_char();
                let _ = self.next_char();
                self.lex_number_radix(start_pos, 2, "0b")?
            } else {
                self.lex_decimal_number()?
            }
        } else {
            self.lex_decimal_number()?
        };

        Ok(num)
    }

    fn lex_string_literal(&mut self) -> Result<Token> {
        let mut literal = String::new();
        let start_pos = self.get_pos();
        loop {
            match self.peek() {
                Some('\'') => {
                    break;
                },
                Some(chr) => {
                    // may cause reallocations, but it's not really important
                    literal.push(chr);
                    self.next_char();
                },
                None => return Err(LexerError {
                    location: SrcSpan { start: start_pos, end: self.get_pos() },
                    kind: LexerErrorType::UnterminatedStringLiteral,
                })
            }
        }
        let end_pos = self.loc0;
        Ok(Token {
            start_pos,
            kind: TokenType::StringLiteral(literal),
            end_pos,
        })
    }

    fn is_ident_start(&self, c: char) -> bool {
        matches!(c, '_' | 'a'..='z' | 'A'..='Z')
    }

    fn is_number_start(&self, c: char, c1: Option<char>) -> bool {
        match c {
            '0'..='9' => true,
            '-' => matches!(c1, Some('0'..='9')),
            _ => false,
        }
    }

    fn is_name_continuation(&self) -> bool {
        self.chr0
            .map(|c| matches!(c, '_' | '0'..='9' | 'a'..='z' | 'A'..='Z'))
            .unwrap_or(false)
    }

    fn lex_ident(&mut self) -> Result<Token> {
        let mut name = String::new();
        let start_pos = self.loc0;
        while self.is_name_continuation() {
            if let Some(chr) = self.next_char() {
                // may cause reallocations, but it's not really important
                name.push(chr);
            }
        }
        let end_pos = self.loc0;

        match str_to_keyword(&name) {
            Some(kind) => Ok(Token { start_pos, kind, end_pos }),
            _ => {
                let kind = TokenType::Identifier(name.into()); 
                Ok(Token { start_pos, kind, end_pos })
            },
        }
    }

    // There are 3 kinds of comments
    // '//', normal
    // '(* ... *)', multiline
    // '(** ... **)', document
    // this function is entered after 2 slashes
    fn lex_multiline_comment(&mut self) -> Token {
        enum Kind {
            Comment,
            Doc,
        }
        let kind = match (self.chr0, self.chr1) {
            (Some('*'), Some('*')) => {
                let _ = self.next_char();
                let _ = self.next_char();
                Kind::Doc
            }
            _ => {
                let _ = self.next_char();
                Kind::Comment
            }
        };
        let mut content = String::new();

        let start_pos = self.get_pos();
        while (Some('*'), Some(')')) != (self.chr0, self.chr1) {
            match self.chr0 {
                Some(c) => content.push(c),
                None => break,
            }
            let _ = self.next_char();
        }
        self.next_char();
        self.next_char();
        let end_pos = self.get_pos();

        let kind = match kind {
            Kind::Comment => TokenType::Comment,
            Kind::Doc => TokenType::CommentDoc { content },
        };
        Token { start_pos, kind, end_pos }
    }

    fn lex_oneline_comment(&mut self) -> Token {
        let start_pos = self.get_pos();
        while Some('\n') != self.chr0 {
            match self.chr0 {
                Some(_) => { let _ = self.next_char(); },
                None => break,
            }
        }
        let end_pos = self.get_pos();
        Token { start_pos, kind: TokenType::Comment, end_pos }
    }

    fn consume_character(&mut self, ch: char) -> Result<()> {
        match ch {
            '+' => self.add_tok1(TokenType::Plus),
            '-' => self.add_tok1(TokenType::Minus),
            '*' => self.add_tok1(TokenType::Star),
            ')' => self.add_tok1(TokenType::RightParen),
            '[' => self.add_tok1(TokenType::LeftBrack),
            ']' => self.add_tok1(TokenType::RightBrack),
            '=' => self.add_tok1(TokenType::Equal),
            ',' => self.add_tok1(TokenType::Comma),
            '(' => {
                match self.peek_next() {
                    Some('*') => {
                        self.next_char();
                        let comment = self.lex_multiline_comment();
                        self.ostream.push(comment);
                    },
                    _ => self.add_tok1(TokenType::LeftParen),
                }
            }
            '/' => {
                match self.peek_next() {
                    Some('/') => {
                        self.next_char();
                        let comment = self.lex_oneline_comment();
                        self.ostream.push(comment);
                    },
                    _ => self.add_tok1(TokenType::Slash),
                }
            },
            '<' => {
                match self.peek_next() {
                    Some('=') => {
                        self.next_char();
                        self.add_tokn(TokenType::LessEqual, 2)
                    },
                    Some('>') => {
                        self.next_char();
                        self.add_tokn(TokenType::NotEqual, 2)
                    },
                    _ => self.add_tok1(TokenType::Less),
                }
            },
            '>' => {
                match self.peek_next() {
                    Some('=') => {
                        self.next_char();
                        self.add_tokn(TokenType::GreaterEqual, 2)
                    },
                    _ => self.add_tok1(TokenType::Greater),
                }
            },
            ':' => {
                match self.peek_next() {
                    Some('=') => {
                        self.next_char();
                        self.add_tokn(TokenType::Assignment, 2)
                    },
                    _ => self.add_tok1(TokenType::Colon),
                }
            },
            '.' => {
                match self.peek_next() {
                    Some('.') => {
                        self.next_char();
                        self.add_tokn(TokenType::DotDot, 2)
                    },
                    _ => self.add_tok1(TokenType::Dot),
                }
            },
            '@' => {
                self.next_char();
                match self.peek() {
                    Some('@') => self.add_tokn(TokenType::DoubleBleat, 2),
                    _ => self.add_tok1(TokenType::Bleat),
                }
            },
            '\'' => {
                self.next_char();
                let string = self.lex_string_literal()?;
                self.ostream.push(string);
            },
            c => {
                let location = self.get_pos();
                return Err(LexerError {
                    kind: LexerErrorType::UnrecognizedToken { tok: c },
                    location: SrcSpan {
                        start: location,
                        end: location,
                    },
                });
            }
        }
        return Ok(());
    }

    fn consume_normal(&mut self) -> Result<()> {
        if let Some(chr) = self.chr0 {
            if self.is_space(chr) {
                self.next_char();
            } else if self.is_separator(chr) {
                self.add_tok1(TokenType::Semicolon);
                self.next_char();
            } else if self.is_ident_start(chr) {
                let name = self.lex_ident()?;
                self.add_tok(name);
            } else if self.is_number_start(chr, self.chr1) {
                let num = self.lex_number()?;
                self.add_tok(num);
            } else {
                self.consume_character(chr)?;
                self.next_char();
            }
        } else {
            self.add_tok1(TokenType::Eof);
        }
        Ok(())
    }

    pub fn inner_next(&mut self) -> Result<Token> {
        while self.ostream.is_empty() {
            self.consume_normal()?;
        }

        Ok(self.ostream.remove(0))
    }
}

impl<T> Iterator for Lexer<T>
where
    T: Iterator<Item = char>,
{
    type Item = Result<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.inner_next();

        match token {
            Ok(Token { kind: TokenType::Eof, .. }) => None,
            r => Some(r),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lex_arith() {
        let lexer = Lexer::new("y = x / (1 + 2) * 4".chars());
        let result = lexer.into_iter().collect::<Vec<_>>();
        let result = result.into_iter()
            .collect::<Result<Vec<_>, _>>()
            .unwrap().into_iter()
            .map(|tok| tok.kind)
            .collect::<Vec<_>>();
        assert_eq!(
            result,
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
