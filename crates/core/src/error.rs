use std::io::Write;

use camino::Utf8PathBuf;
use termcolor::Buffer;
use thiserror::Error;

use crate::{diagnostic::{Diagnostic, Label, Level, Location}, lexer::{self, SrcSpan}, parser::{ParserError, ParserErrorType}, sema::SemanticError};

pub type Result<Ok, Err = Error> = std::result::Result<Ok, Err>;

#[derive(Debug, Clone, Error)]
pub enum Error {
    #[error("failed to scan source code")]
    Lexer{
        path: Utf8PathBuf,
        src: String,
        error: lexer::LexerError,
    },

    #[error("failed to parse source code")]
    Parser {
        path: Utf8PathBuf,
        src: String,
        error: ParserError,
    },

    #[error("failed to resolve source code")]
    Semantic {
        path: Utf8PathBuf,
        src: String,
        error: SemanticError,
    },
}

impl Error {
    pub fn pretty_string(&self) -> String {
        let mut nocolor = Buffer::no_color();
        self.pretty(&mut nocolor);
        String::from_utf8(nocolor.into_inner()).expect("Error printing produced invalid utf8")
    }

    pub fn pretty(&self, buffer: &mut Buffer) {
        for diagnostic in self.to_diagnostics() {
            diagnostic.write(buffer);
            writeln!(buffer).expect("write new line after diagnostic");
        }
    }

    pub fn to_diagnostics(&self) -> Vec<Diagnostic> {
        match &self {
            Error::Lexer { path, src, error } => {
                let (label, extra) = error.details();
                let text = extra.join("\n");

                vec![Diagnostic {
                    title: "Scanning error".into(),
                    text,
                    hint: None,
                    level: Level::Error,
                    location: Some(Location {
                        label: Label {
                            text: Some(label.to_string()),
                            span: error.location.clone(),
                        },
                        path: path.clone(),
                        src: src.clone(),
                        extra_labels: vec![],
                    }),
                }]
            },
            Error::Parser { path, src, error } => {
                let (label, extra) = error.details();
                let text = extra.join("\n");

                let adjusted_location = if error.kind == ParserErrorType::UnexpectedEof {
                    SrcSpan {
                        start: src.len() - 1,
                        end: src.len() - 1,
                    }
                } else {
                    error.location.clone()
                };

                vec![Diagnostic {
                    title: "Parsing error".into(),
                    text,
                    hint: None,
                    level: Level::Error,
                    location: Some(Location {
                        label: Label {
                            text: Some(label.to_string()),
                            span: adjusted_location,
                        },
                        path: path.clone(),
                        src: src.clone(),
                        extra_labels: vec![],
                    }),
                }]
            },
            Error::Semantic { path, src, error } => {
                let (label, extra) = error.details();
                let text = extra.join("\n");

                vec![Diagnostic {
                    title: "Semantic error".into(),
                    text,
                    hint: None,
                    level: Level::Error,
                    location: Some(Location {
                        label: Label {
                            text: Some(label.to_string()),
                            span: error.location.clone(),
                        },
                        path: path.clone(),
                        src: src.clone(),
                        extra_labels: vec![],
                    }),
                }]
            },
        }
    }
}
