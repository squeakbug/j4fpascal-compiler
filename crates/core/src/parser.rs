use crate::{
    ast::{
        Block, CaseItem, CaseLabel, DeclSection, Designator, DesignatorItem, Expr, Param,
        ParamModifier, ProcedureDeclaration, ProcedureHeadDeclaration, Program, Stmt, TypeDecl,
        TypeDeclaration, UnlabeledStmt, VarDeclaration,
    },
    lexer::{SrcSpan, Token, TokenType},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParserErrorType {
    ExpectedProgram,
    ExpectedIdentifier,
    ExpectedExpression,
    ExpectedTerm,
    ExpectedSimpleExpression,
    ExpectedFactor,
    ExpectedLeftParen,
    ExpectedToken { tok: TokenType },
    UnexpectedToken { tok: TokenType },
    ExpectedFormalParameter,
    ExpectedActualParameter,
    ExpectedUnlabeledStmt,
    ExpectedBlock,
    ExpectedTypeDeclaration,
    ExpectedCaseLabel,
    ExpectedDesignator,
    UnexpectedEof,
}

#[derive(Debug, Clone)]
pub struct ParserError {
    pub location: SrcSpan,
    pub kind: ParserErrorType,
}

impl ParserError {
    pub fn details(&self) -> (String, Vec<String>) {
        match &self.kind {
            ParserErrorType::ExpectedProgram => ("Expected program".into(), vec![]),
            ParserErrorType::ExpectedIdentifier => ("Expected identifier".into(), vec![]),
            ParserErrorType::ExpectedExpression => ("Expected expression".into(), vec![]),
            ParserErrorType::ExpectedLeftParen => ("Expected '('".into(), vec![]),
            ParserErrorType::ExpectedToken { tok } => (format!("Expected '{:#?}'", tok), vec![]),
            ParserErrorType::UnexpectedToken { tok } => {
                (format!("Unexpected '{:#?}'", tok), vec![])
            }
            ParserErrorType::ExpectedFormalParameter => {
                ("Expected formal parameter".into(), vec![])
            }
            ParserErrorType::ExpectedActualParameter => {
                ("Expected actual parameter".into(), vec![])
            }
            ParserErrorType::ExpectedUnlabeledStmt => {
                ("Expected unlabeled statement".into(), vec![])
            }
            ParserErrorType::ExpectedBlock => ("Expected block".into(), vec![]),
            ParserErrorType::ExpectedTypeDeclaration => {
                ("Expected type declaration".into(), vec![])
            }
            ParserErrorType::ExpectedCaseLabel => ("Expected case label".into(), vec![]),
            ParserErrorType::ExpectedTerm => ("Expected term".into(), vec![]),
            ParserErrorType::ExpectedSimpleExpression => ("Expected simple expr".into(), vec![]),
            ParserErrorType::ExpectedFactor => ("Expected factor".into(), vec![]),
            ParserErrorType::ExpectedDesignator => ("Expected designator".into(), vec![]),
            ParserErrorType::UnexpectedEof => ("Unexpected EOF".into(), vec![]),
        }
    }
}

fn parser_error(kind: ParserErrorType, location: SrcSpan) -> ParserError {
    ParserError { kind, location }
}

fn error_tok(kind: ParserErrorType, prev_tok: &Token) -> ParserError {
    parser_error(
        kind,
        SrcSpan {
            start: prev_tok.start_pos,
            end: prev_tok.end_pos,
        },
    )
}

// TODO: make two types of functions:
// expected_* - try to parse rule. If rule is not acceptable, then return Error
// parse_* - try to parse rule. If rule is not acceptable, then return Ok(None)

pub struct Parser<T: Iterator<Item = Token>> {
    istream: T,
    tok0: Option<Token>,
    tok1: Option<Token>,
    errors: Vec<ParserError>,
}

impl<T> Parser<T>
where
    T: Iterator<Item = Token>,
{
    pub fn new(input: T) -> Self {
        let mut parser = Parser {
            istream: input,
            tok0: None,
            tok1: None,
            errors: vec![],
        };
        parser.advance();
        parser.advance();
        parser
    }

    pub fn parse(&mut self) -> Result<Program, ParserError> {
        self.program()?.ok_or(parser_error(
            ParserErrorType::ExpectedProgram,
            SrcSpan { start: 0, end: 0 },
        ))
    }

    fn peek(&mut self) -> Option<Token> {
        self.tok0.as_mut().cloned()
    }

    fn advance(&mut self) -> Option<Token> {
        let t = self.tok0.take();
        let mut nxt;
        loop {
            match self.istream.next() {
                Some(Token {
                    kind: TokenType::Comment,
                    ..
                }) => {
                    continue;
                }
                Some(Token {
                    kind: TokenType::CommentDoc { .. },
                    ..
                }) => {
                    continue;
                }
                Some(tok) => {
                    nxt = Some(tok);
                    break;
                }
                None => {
                    nxt = None;
                    break;
                }
            };
        }
        self.tok0 = self.tok1.take();
        self.tok1 = nxt.take();
        t
    }

    // TODO: rename to "expected_token"
    fn consume(&mut self, expected: TokenType, pos: SrcSpan) -> Result<Token, ParserError> {
        match self.advance() {
            Some(token) if token.kind == expected => Ok(token),
            Some(token) => Err(parser_error(
                ParserErrorType::ExpectedToken { tok: expected },
                SrcSpan {
                    start: token.start_pos,
                    end: token.end_pos,
                },
            )),
            None => Err(parser_error(
                ParserErrorType::ExpectedToken { tok: expected },
                pos,
            )),
        }
    }

    fn synchronize(&mut self) {
        self.advance();
        while let Some(token) = self.peek() {
            if token.kind == TokenType::Semicolon {
                return;
            }
            match token.kind {
                TokenType::Procedure
                | TokenType::Function
                | TokenType::Var
                | TokenType::For
                | TokenType::If => {
                    break;
                }
                _ => {
                    let _ = self.advance();
                }
            }
        }
    }

    fn program_parm_seq(&mut self) -> Result<Vec<String>, ParserError> {
        let mut idents = vec![];
        if let Some(
            lp_tok @ Token {
                kind: TokenType::LeftParen,
                ..
            },
        ) = self.peek()
        {
            self.advance();
            while let Some(identifier) = self.identifier() {
                idents.push(identifier);
                match self.peek() {
                    Some(Token {
                        kind: TokenType::Comma,
                        ..
                    }) => {
                        self.advance();
                    }
                    _ => break,
                }
            }
            self.consume(TokenType::RightParen, lp_tok.as_ref().into())?;
        }
        Ok(idents)
    }

    fn program_head(&mut self) -> Result<Option<(String, Vec<String>)>, ParserError> {
        if let Some(
            program_tok @ Token {
                kind: TokenType::Program,
                ..
            },
        ) = self.peek()
        {
            self.advance();
            let namespace_name = self
                .identifier()
                .ok_or(error_tok(ParserErrorType::ExpectedIdentifier, &program_tok))?;
            let params = self.program_parm_seq()?;
            self.consume(TokenType::Semicolon, program_tok.as_ref().into())?;
            Ok(Some((namespace_name, params)))
        } else {
            Ok(None)
        }
    }

    fn program(&mut self) -> Result<Option<Program>, ParserError> {
        let head = self.program_head()?;
        if let Some(block) = self.block()? {
            self.consume(TokenType::Dot, SrcSpan { start: 0, end: 0 })?;
            Ok(Some(Program {
                head,
                block: Box::new(block),
            }))
        } else {
            Err(parser_error(
                ParserErrorType::ExpectedBlock,
                SrcSpan { start: 0, end: 0 },
            ))
        }
    }

    fn block(&mut self) -> Result<Option<Block>, ParserError> {
        let decl_sections = self.decl_sections()?;
        if let Some(stmt) = self.compound_statement()? {
            Ok(Some(Block {
                decl_sections,
                body: Box::new(Stmt {
                    label: None,
                    statement: Box::new(stmt),
                }),
            }))
        } else {
            Ok(None)
        }
    }

    fn decl_sections(&mut self) -> Result<Vec<DeclSection>, ParserError> {
        let mut declarations = vec![];
        loop {
            if let Some(label_declarations) = self.label_decl_section()? {
                declarations.push(DeclSection::Label(label_declarations));
            } else if let Some(const_declarations) = self.const_decl_section()? {
                declarations.push(DeclSection::Const(const_declarations));
            } else if let Some(type_declarations) = self.type_decl_section()? {
                declarations.push(DeclSection::Type(type_declarations));
            } else if let Some(declaration) = self.var_decl_section()? {
                declarations.push(DeclSection::Variable(declaration));
            } else if let Some(declaration) = self.proc_decl_section()? {
                declarations.push(DeclSection::Procedure(declaration));
            } else {
                break;
            }
        }
        Ok(declarations)
    }

    fn identifier(&mut self) -> Option<String> {
        match self.peek() {
            Some(Token {
                kind: TokenType::Identifier(ref name),
                ..
            }) => {
                self.advance();
                return Some(name.clone());
            }
            _ => None,
        }
    }

    fn expected_identifier(&mut self, prev_tok: &Token) -> Result<(Token, String), ParserError> {
        match self.advance() {
            Some(
                ref ident_token @ Token {
                    kind: TokenType::Identifier(ref name),
                    ..
                },
            ) => {
                return Ok((ident_token.clone(), name.to_string()));
            }
            _ => return Err(error_tok(ParserErrorType::ExpectedIdentifier, &prev_tok)),
        }
    }

    fn var_decl_section(&mut self) -> Result<Option<Vec<VarDeclaration>>, ParserError> {
        if let Some(
            token @ Token {
                kind: TokenType::Var,
                ..
            },
        ) = self.peek()
        {
            self.advance();
            if let Some(identifier) = self.identifier() {
                let mut idents = vec![identifier];
                while let Some(identifier) = self.identifier() {
                    idents.push(identifier);
                    match self.peek() {
                        Some(Token {
                            kind: TokenType::Comma,
                            ..
                        }) => {
                            self.advance();
                        }
                        _ => break,
                    }
                }
                self.consume(TokenType::Colon, token.as_ref().into())?;
                if let Some(type_decl) = self.type_decl()? {
                    let mut result = vec![];
                    for ident in idents.into_iter() {
                        result.push(VarDeclaration {
                            name: ident.to_string(),
                            // TODO: all vars must reference to the same type and expr
                            var_type: type_decl.clone(),
                            init_value: None,
                        });
                    }
                    if let Some(Token {
                        kind: TokenType::Equal,
                        ..
                    }) = self.peek()
                    {
                        self.advance();
                        if let Some(expr) = self.expression()? {
                            for var in result.iter_mut() {
                                var.init_value = Some(Box::new(expr.clone()));
                            }
                            self.consume(TokenType::Semicolon, token.as_ref().into())?;
                            Ok(Some(result))
                        } else {
                            Err(parser_error(
                                ParserErrorType::ExpectedExpression,
                                token.as_ref().into(),
                            ))
                        }
                    } else {
                        self.consume(TokenType::Semicolon, token.as_ref().into())?;
                        Ok(Some(result))
                    }
                } else {
                    Err(parser_error(
                        ParserErrorType::ExpectedTypeDeclaration,
                        token.as_ref().into(),
                    ))
                }
            } else {
                Err(parser_error(
                    ParserErrorType::ExpectedIdentifier,
                    token.as_ref().into(),
                ))
            }
        } else {
            Ok(None)
        }
    }

    fn const_decl_section(&mut self) -> Result<Option<Vec<(String, Box<Expr>)>>, ParserError> {
        Ok(None)
    }

    fn label_decl_section(&mut self) -> Result<Option<Vec<String>>, ParserError> {
        Ok(None)
    }

    fn type_declaration(&mut self) -> Result<Option<TypeDeclaration>, ParserError> {
        if let Some(ident) = self.identifier() {
            let tok = self.consume(TokenType::Equal, SrcSpan { start: 0, end: 0 })?;
            let decl = self.expected_type_decl(&tok)?;
            self.consume(TokenType::Semicolon, SrcSpan { start: 0, end: 0 })?;
            Ok(Some(TypeDeclaration { decl, ident }))
        } else {
            Ok(None)
        }
    }

    fn type_decl_section(&mut self) -> Result<Option<Vec<TypeDeclaration>>, ParserError> {
        match self.peek() {
            Some(
                ref token @ Token {
                    kind: TokenType::Type,
                    ..
                },
            ) => {
                self.advance();
                if let Some(type_declaration) = self.type_declaration()? {
                    let mut type_declarations = vec![type_declaration];
                    while let Some(type_declaration) = self.type_declaration()? {
                        type_declarations.push(type_declaration);
                    }
                    Ok(Some(type_declarations))
                } else {
                    Ok(None)
                }
            }
            _ => Ok(None),
        }
    }

    fn array_type_decl(&mut self) -> Result<Option<TypeDecl>, ParserError> {
        match self.peek() {
            Some(Token {
                kind: TokenType::Array,
                ..
            }) => {
                self.advance();
                Ok(None)
            }
            _ => Ok(None),
        }
    }

    fn class_type_type_decl(&mut self) -> Result<Option<TypeDecl>, ParserError> {
        if let Some(Token {
            kind: TokenType::Class,
            ..
        }) = self.peek()
        {
            self.advance();
            Ok(None)
        } else {
            Ok(None)
        }
    }

    fn class_type_decl(&mut self) -> Result<Option<TypeDecl>, ParserError> {
        if let Some(Token {
            kind: TokenType::Class,
            ..
        }) = self.peek()
        {
            self.advance();
            Ok(None)
        } else {
            Ok(None)
        }
    }

    fn class_helper_decl(&mut self) -> Result<Option<TypeDecl>, ParserError> {
        if let Some(Token {
            kind: TokenType::Class,
            ..
        }) = self.peek()
        {
            self.advance();
            Ok(None)
        } else {
            Ok(None)
        }
    }

    fn interface_type_decl(&mut self) -> Result<Option<TypeDecl>, ParserError> {
        if let Some(Token {
            kind: TokenType::Interface,
            ..
        }) = self.peek()
        {
            self.advance();
            Ok(None)
        } else {
            Ok(None)
        }
    }

    fn object_decl(&mut self) -> Result<Option<TypeDecl>, ParserError> {
        if let Some(Token {
            kind: TokenType::Interface,
            ..
        }) = self.peek()
        {
            self.advance();
            Ok(None)
        } else {
            Ok(None)
        }
    }

    fn record_field(&mut self) -> Result<Option<Vec<(String, Box<TypeDecl>)>>, ParserError> {
        let identities = self.ident_list()?;
        if identities.len() == 0 {
            Ok(None)
        } else {
            let colon_tok = self.consume(TokenType::Colon, SrcSpan { start: 0, end: 0 })?;
            let type_decl = self.expected_type_decl(&colon_tok)?;
            let identities = identities
                .into_iter()
                .map(|ident| (ident, Box::new(type_decl.clone())))
                .collect::<Vec<_>>();
            self.consume(TokenType::Semicolon, colon_tok.as_ref().into())?;
            Ok(Some(identities))
        }
    }

    fn record_decl(&mut self) -> Result<Option<TypeDecl>, ParserError> {
        match self.peek() {
            Some(
                record_tok @ Token {
                    kind: TokenType::Record,
                    ..
                },
            ) => {
                self.advance();
                let mut fields = vec![];
                while let Some(record) = self.record_field()? {
                    for field in record.into_iter() {
                        fields.push(field);
                    }
                }
                self.consume(TokenType::End, record_tok.as_ref().into());
                Ok(Some(TypeDecl::RecordType { fields }))
            }
            _ => Ok(None),
        }
    }

    fn record_helper_decl(&mut self) -> Result<Option<TypeDecl>, ParserError> {
        if let Some(Token {
            kind: TokenType::Interface,
            ..
        }) = self.peek()
        {
            self.advance();
            Ok(None)
        } else {
            Ok(None)
        }
    }

    fn class_decl(&mut self) -> Result<Option<TypeDecl>, ParserError> {
        if let Some(rec_decl) = self.class_type_type_decl()? {
            Ok(Some(rec_decl))
        } else if let Some(rec_decl) = self.class_type_decl()? {
            Ok(Some(rec_decl))
        } else if let Some(rec_decl) = self.class_helper_decl()? {
            Ok(Some(rec_decl))
        } else if let Some(rec_decl) = self.interface_type_decl()? {
            Ok(Some(rec_decl))
        } else if let Some(rec_decl) = self.object_decl()? {
            Ok(Some(rec_decl))
        } else if let Some(rec_decl) = self.record_decl()? {
            Ok(Some(rec_decl))
        } else if let Some(rec_decl) = self.record_helper_decl()? {
            Ok(Some(rec_decl))
        } else {
            Ok(None)
        }
    }

    fn set_type_decl(&mut self) -> Result<Option<TypeDecl>, ParserError> {
        match self.peek() {
            Some(Token {
                kind: TokenType::Set,
                ..
            }) => {
                self.advance();
                Ok(None)
            }
            _ => Ok(None),
        }
    }

    fn file_type_decl(&mut self) -> Result<Option<TypeDecl>, ParserError> {
        match self.peek() {
            Some(Token {
                kind: TokenType::File,
                ..
            }) => {
                self.advance();
                Ok(None)
            }
            _ => Ok(None),
        }
    }

    fn pointer_type_decl(&mut self) -> Result<Option<TypeDecl>, ParserError> {
        Ok(None)
    }

    fn subrange_type_decl(&mut self) -> Result<Option<TypeDecl>, ParserError> {
        Ok(None)
    }

    fn enum_type_decl(&mut self) -> Result<Option<TypeDecl>, ParserError> {
        Ok(None)
    }

    fn simple_type_decl(&mut self) -> Result<Option<TypeDecl>, ParserError> {
        match self.peek() {
            Some(Token {
                kind: TokenType::Identifier(name),
                ..
            }) => {
                self.advance();
                Ok(Some(TypeDecl::SimpleType { ident: name }))
            }
            _ => Ok(None),
        }
    }

    fn type_decl(&mut self) -> Result<Option<TypeDecl>, ParserError> {
        if let Some(arr_decl) = self.array_type_decl()? {
            Ok(Some(arr_decl))
        } else if let Some(set_decl) = self.class_decl()? {
            Ok(Some(set_decl))
        } else if let Some(set_decl) = self.set_type_decl()? {
            Ok(Some(set_decl))
        } else if let Some(file_decl) = self.file_type_decl()? {
            Ok(Some(file_decl))
        } else if let Some(file_decl) = self.pointer_type_decl()? {
            Ok(Some(file_decl))
        } else if let Some(file_decl) = self.subrange_type_decl()? {
            Ok(Some(file_decl))
        } else if let Some(file_decl) = self.enum_type_decl()? {
            Ok(Some(file_decl))
        } else if let Some(simple_decl) = self.simple_type_decl()? {
            Ok(Some(simple_decl))
        } else {
            Ok(None)
        }
    }

    fn parameter_modifier(&mut self) -> Option<ParamModifier> {
        match self.peek() {
            Some(Token {
                kind: TokenType::Const,
                ..
            }) => Some(ParamModifier::Const),
            Some(Token {
                kind: TokenType::Var,
                ..
            }) => Some(ParamModifier::Var),
            Some(Token {
                kind: TokenType::Out,
                ..
            }) => Some(ParamModifier::Out),
            Some(other) => None,
            None => None,
        }
    }

    fn formal_parameter(&mut self) -> Result<Option<Param>, ParserError> {
        let modifier = self.parameter_modifier();
        if let Some(ident) = self.identifier() {
            let mut param = Param {
                modifier,
                ident,
                type_decl: None,
                init_value: None,
            };
            if let Some(
                token @ Token {
                    kind: TokenType::Colon,
                    ..
                },
            ) = self.peek()
            {
                self.advance();
                param.type_decl = Some(self.expected_type_decl(&token)?);
            }
            if let Some(
                token @ Token {
                    kind: TokenType::Equal,
                    ..
                },
            ) = self.peek()
            {
                self.advance();
                param.init_value = Some(Box::new(self.expected_expression(&token)?));
            }
            Ok(Some(param))
        } else {
            Ok(None)
        }
    }

    fn formal_parameter_list(&mut self) -> Result<Vec<Param>, ParserError> {
        let mut parameters = vec![];
        if let Some(parameter) = self.formal_parameter()? {
            parameters.push(parameter);
            while let Some(token) = self.peek() {
                if token.kind == TokenType::Semicolon {
                    self.advance();
                    if let Some(parameter) = self.formal_parameter()? {
                        parameters.push(parameter);
                    } else {
                        return Err(error_tok(ParserErrorType::ExpectedFormalParameter, &token));
                    }
                } else {
                    break;
                }
            }
        }
        return Ok(parameters);
    }

    fn expected_type_decl(&mut self, prev_tok: &Token) -> Result<TypeDecl, ParserError> {
        if let Some(type_decl) = self.type_decl()? {
            Ok(type_decl)
        } else {
            return Err(error_tok(
                ParserErrorType::ExpectedTypeDeclaration,
                prev_tok,
            ));
        }
    }

    fn add_proc_head_decl(
        &mut self,
        mut head: ProcedureHeadDeclaration,
        prev_tok: &Token,
    ) -> Result<Option<ProcedureHeadDeclaration>, ParserError> {
        if let Some(
            ref colon_tok @ Token {
                kind: TokenType::Colon,
                ..
            },
        ) = self.peek()
        {
            self.advance();
            head.return_type = Some(self.expected_type_decl(&colon_tok)?);
        }
        self.consume(TokenType::Semicolon, prev_tok.as_ref().into())?;
        Ok(Some(head))
    }

    fn proc_decl_heading(&mut self) -> Result<Option<ProcedureHeadDeclaration>, ParserError> {
        if let Some(
            proc_token @ Token {
                kind: TokenType::Procedure,
                ..
            },
        ) = self.peek()
        {
            self.advance();
            let (ident_token, name) = self.expected_identifier(&proc_token)?;

            let mut head_declaration = ProcedureHeadDeclaration {
                name,
                params: vec![],
                return_type: None,
            };
            match self.peek() {
                Some(Token {
                    kind: TokenType::LeftParen,
                    ..
                }) => {
                    head_declaration.params = self.formal_parameter_list()?;
                    self.consume(TokenType::RightParen, ident_token.as_ref().into())?;
                    self.add_proc_head_decl(head_declaration, &ident_token)
                }
                Some(Token {
                    kind: TokenType::Colon,
                    ..
                }) => self.add_proc_head_decl(head_declaration, &ident_token),
                Some(tok) => Err(parser_error(
                    ParserErrorType::UnexpectedToken { tok: tok.kind },
                    SrcSpan { start: 0, end: 0 },
                )),
                None => Err(parser_error(
                    ParserErrorType::UnexpectedEof,
                    SrcSpan { start: 0, end: 0 },
                )),
            }
        } else {
            Ok(None)
        }
    }

    fn proc_decl_section(&mut self) -> Result<Option<ProcedureDeclaration>, ParserError> {
        if let Some(head) = self.proc_decl_heading()? {
            if let Some(body) = self.compound_statement()? {
                self.consume(TokenType::Semicolon, SrcSpan { start: 0, end: 0 })?;
                Ok(Some(ProcedureDeclaration {
                    head,
                    body: Box::new(Stmt {
                        label: None,
                        statement: Box::new(body),
                    }),
                }))
            } else {
                Ok(None)
            }
        } else {
            Ok(None)
        }
    }

    fn statement(&mut self) -> Result<Stmt, ParserError> {
        if let Some(token) = self.peek() {
            if token.kind == TokenType::Label {
                self.advance();
                self.consume(TokenType::Colon, token.as_ref().into())?;
                let statement = self.unlabeled_statement()?;
                Ok(Stmt {
                    label: Some(token),
                    statement: Box::new(statement),
                })
            } else {
                let statement = self.unlabeled_statement()?;
                Ok(Stmt {
                    label: Some(token),
                    statement: Box::new(statement),
                })
            }
        } else {
            Err(parser_error(
                ParserErrorType::UnexpectedEof,
                SrcSpan { start: 0, end: 0 },
            ))
        }
    }

    fn unlabeled_statement(&mut self) -> Result<UnlabeledStmt, ParserError> {
        if let Some(stmt) = self.if_statement()? {
            Ok(stmt)
        } else if let Some(stmt) = self.case_statement()? {
            Ok(stmt)
        } else if let Some(stmt) = self.repeat_statement()? {
            Ok(stmt)
        } else if let Some(stmt) = self.while_statement()? {
            Ok(stmt)
        } else if let Some(stmt) = self.for_statement()? {
            Ok(stmt)
        } else if let Some(stmt) = self.with_statement()? {
            Ok(stmt)
        } else if let Some(stmt) = self.try_statement()? {
            Ok(stmt)
        } else if let Some(stmt) = self.raise_statement()? {
            Ok(stmt)
        } else if let Some(stmt) = self.compound_statement()? {
            Ok(stmt)
        } else if let Some(stmt) = self.simple_statement()? {
            Ok(stmt)
        } else {
            Ok(UnlabeledStmt::Empty)
        }
    }

    fn goto_statement(&mut self) -> Result<Option<UnlabeledStmt>, ParserError> {
        if let Some(token) = self.peek() {
            match token {
                Token {
                    kind: TokenType::Goto,
                    ..
                } => return Ok(Some(UnlabeledStmt::Goto { label: token })),
                Token {
                    kind: TokenType::Break,
                    ..
                } => return Ok(Some(UnlabeledStmt::Break)),
                Token {
                    kind: TokenType::Continue,
                    ..
                } => return Ok(Some(UnlabeledStmt::Continue)),
                _ => return Ok(None),
            }
        }
        Ok(None)
    }

    fn colon_construct(&mut self) -> Result<Vec<Box<Expr>>, ParserError> {
        let mut expressions = vec![];
        loop {
            if let Some(token) = self.peek() {
                if token.kind == TokenType::Colon {
                    self.advance();
                    let expression = Box::new(self.expected_expression(&token)?);
                    expressions.push(expression)
                } else {
                    break;
                }
            }
        }
        Ok(expressions)
    }

    fn actual_parameter(&mut self) -> Result<Option<(Box<Expr>, Vec<Box<Expr>>)>, ParserError> {
        if let Some(expr) = self.expression()? {
            let parameter = Box::new(expr);
            let colon_construct = self.colon_construct()?;
            Ok(Some((parameter, colon_construct)))
        } else {
            Ok(None)
        }
    }

    fn expected_actual_parameter(
        &mut self,
        prev_tok: &Token,
    ) -> Result<(Box<Expr>, Vec<Box<Expr>>), ParserError> {
        if let Some(parameter) = self.actual_parameter()? {
            Ok(parameter)
        } else {
            return Err(error_tok(
                ParserErrorType::ExpectedActualParameter,
                &prev_tok,
            ));
        }
    }

    fn parameter_list(&mut self) -> Result<Vec<(Box<Expr>, Vec<Box<Expr>>)>, ParserError> {
        let mut parameters = vec![];
        if let Some(parameter) = self.actual_parameter()? {
            parameters.push(parameter);
            while let Some(token) = self.peek() {
                if token.kind == TokenType::Comma {
                    self.advance();
                    let parameter = self.expected_actual_parameter(&token)?;
                    parameters.push(parameter);
                } else {
                    break;
                }
            }
        }
        return Ok(parameters);
    }

    fn procedure_call(&mut self) -> Result<Option<DesignatorItem>, ParserError> {
        match self.peek() {
            Some(
                token @ Token {
                    kind: TokenType::LeftParen,
                    ..
                },
            ) => {
                self.advance();
                let arguments = self.parameter_list()?;
                self.consume(TokenType::RightParen, token.as_ref().into())?;
                Ok(Some(DesignatorItem::Call { arguments }))
            }
            _ => Ok(None),
        }
    }

    fn array_access(&mut self) -> Result<Option<DesignatorItem>, ParserError> {
        match self.peek() {
            Some(
                token @ Token {
                    kind: TokenType::LeftBrack,
                    ..
                },
            ) => {
                self.advance();
                let indexes = self.expression_list()?;
                self.consume(TokenType::RightBrack, token.as_ref().into())?;
                Ok(Some(DesignatorItem::ArrayAccess { indexes }))
            }
            _ => Ok(None),
        }
    }

    fn designator_item(&mut self) -> Result<Option<DesignatorItem>, ParserError> {
        if let Some(array_access) = self.array_access()? {
            Ok(Some(array_access))
        } else if let Some(procedure_call) = self.procedure_call()? {
            Ok(Some(procedure_call))
        } else {
            Ok(None)
        }
    }

    fn qualified_ident(&mut self) -> Result<Option<String>, ParserError> {
        match self.peek() {
            Some(Token {
                kind: TokenType::Identifier(name),
                ..
            }) => {
                self.advance();
                return Ok(Some(name));
            }
            _ => Ok(None),
        }
    }

    fn designator(&mut self) -> Result<Option<Designator>, ParserError> {
        if let Some(name) = self.qualified_ident()? {
            let mut items = vec![];
            while let Some(item) = self.designator_item()? {
                items.push(item);
            }
            Ok(Some(Designator { name, items }))
        } else {
            Ok(None)
        }
    }

    fn expected_designator(&mut self, prev_tok: &Token) -> Result<Designator, ParserError> {
        if let Some(designator) = self.designator()? {
            Ok(designator)
        } else {
            Err(error_tok(ParserErrorType::ExpectedDesignator, &prev_tok))
        }
    }

    // TODO: add structured identifier
    fn simple_statement(&mut self) -> Result<Option<UnlabeledStmt>, ParserError> {
        if let Some(designator) = self.designator()? {
            if let Some(token) = self.peek() {
                if token.kind == TokenType::Assignment {
                    self.advance();
                    let expression = self.expected_expression(&token)?;
                    return Ok(Some(UnlabeledStmt::Assigment {
                        left: designator,
                        right: Box::new(expression),
                    }));
                } else {
                    return Ok(Some(UnlabeledStmt::ProcedureCall { designator }));
                }
            }
        } else if let Some(stmt) = self.goto_statement()? {
            return Ok(Some(stmt));
        }
        Ok(None)
    }

    fn statement_list(&mut self) -> Result<Vec<Box<Stmt>>, ParserError> {
        let mut statements = vec![Box::new(self.statement()?)];
        while let Some(token) = self.peek() {
            if token.kind == TokenType::Semicolon {
                self.advance();
                let statement = self.statement()?;
                statements.push(Box::new(statement));
            } else {
                break;
            }
        }
        return Ok(statements);
    }

    fn compound_statement(&mut self) -> Result<Option<UnlabeledStmt>, ParserError> {
        if let Some(
            begin_token @ Token {
                kind: TokenType::Begin,
                ..
            },
        ) = self.peek()
        {
            self.advance();
            let statements = self.statement_list()?;
            self.consume(TokenType::End, begin_token.as_ref().into())?;
            Ok(Some(UnlabeledStmt::Compound { statements }))
        } else {
            Ok(None)
        }
    }

    fn if_statement(&mut self) -> Result<Option<UnlabeledStmt>, ParserError> {
        if let Some(token) = self.peek() {
            if token.kind == TokenType::If {
                self.advance();
                let condition = Box::new(self.expected_expression(&token)?);
                self.consume(TokenType::Then, token.as_ref().into())?;
                let then_branch = Box::new(self.statement()?);
                let mut else_branch = None;
                if let Some(token) = self.peek() {
                    if token.kind == TokenType::Else {
                        self.advance();
                        else_branch = Some(Box::new(self.statement()?));
                    }
                    return Ok(Some(UnlabeledStmt::If {
                        condition,
                        then_branch,
                        else_branch,
                    }));
                }
            }
        }
        Ok(None)
    }

    fn case_label(&mut self) -> Result<Option<CaseLabel>, ParserError> {
        if let Some(expr) = self.expression()? {
            let from = Box::new(expr);
            if let Some(token) = self.peek() {
                if token.kind == TokenType::DotDot {
                    self.advance();
                    let to = Box::new(self.expected_expression(&token)?);
                    return Ok(Some(CaseLabel::Range((from, to))));
                }
            }
            return Ok(Some(CaseLabel::Simple(from)));
        } else {
            Ok(None)
        }
    }

    fn case_item(&mut self) -> Result<Option<CaseItem>, ParserError> {
        if let Some(lbl) = self.case_label()? {
            let mut labels = vec![lbl];
            while let Some(token) = self.peek() {
                if token.kind == TokenType::Comma {
                    self.advance();
                    if let Some(lbl) = self.case_label()? {
                        labels.push(lbl);
                    } else {
                        return Err(error_tok(ParserErrorType::ExpectedCaseLabel, &token));
                    }
                } else {
                    break;
                }
            }
            self.consume(TokenType::Colon, SrcSpan { start: 0, end: 0 })?;
            let statement = Box::new(self.statement()?);
            Ok(Some(CaseItem { labels, statement }))
        } else {
            Ok(None)
        }
    }

    fn case_statement(&mut self) -> Result<Option<UnlabeledStmt>, ParserError> {
        if let Some(token) = self.peek() {
            if token.kind == TokenType::Case {
                self.advance();
                let condition = Box::new(self.expected_expression(&token)?);
                self.consume(TokenType::Of, token.as_ref().into())?;
                let mut case_items = vec![];
                while let Some(item) = self.case_item()? {
                    case_items.push(Box::new(item));
                }
                let mut else_branch = None;
                if let Some(token) = self.peek() {
                    if token.kind == TokenType::Else {
                        self.advance();
                        else_branch = Some(Box::new(self.statement()?));
                    }
                }
                self.consume(TokenType::End, token.as_ref().into())?;
                return Ok(Some(UnlabeledStmt::Case {
                    condition,
                    case_items,
                    else_branch,
                }));
            }
        }
        Ok(None)
    }

    fn repeat_statement(&mut self) -> Result<Option<UnlabeledStmt>, ParserError> {
        if let Some(token) = self.peek() {
            if token.kind == TokenType::Repeat {
                self.advance();
                let statements = self.statement_list()?;
                self.consume(TokenType::Until, token.as_ref().into())?;
                let expr = self.expected_expression(&token)?;
                return Ok(Some(UnlabeledStmt::Repeat {
                    statements,
                    condition: Box::new(expr),
                }));
            }
        }
        Ok(None)
    }

    fn while_statement(&mut self) -> Result<Option<UnlabeledStmt>, ParserError> {
        if let Some(token) = self.peek() {
            if token.kind == TokenType::While {
                self.advance();
                let condition = Box::new(self.expected_expression(&token)?);
                self.consume(TokenType::Do, token.as_ref().into())?;
                let statement = Box::new(self.statement()?);
                return Ok(Some(UnlabeledStmt::While {
                    condition,
                    statement,
                }));
            }
        }
        Ok(None)
    }

    fn for_statement(&mut self) -> Result<Option<UnlabeledStmt>, ParserError> {
        if let Some(token) = self.peek() {
            if token.kind == TokenType::For {
                self.advance();
                let var = Box::new(self.expected_designator(&token)?);
                self.consume(TokenType::Assignment, token.as_ref().into())?;
                let init = Box::new(self.expected_expression(&token)?);
                self.consume(TokenType::To, token.as_ref().into())?;
                let to = Box::new(self.expected_expression(&token)?);
                self.consume(TokenType::Do, token.as_ref().into())?;
                let statement = Box::new(self.statement()?);
                return Ok(Some(UnlabeledStmt::For {
                    var,
                    init,
                    to,
                    statement,
                    is_down_to: false,
                }));
            }
        }
        Ok(None)
    }

    fn with_statement(&mut self) -> Result<Option<UnlabeledStmt>, ParserError> {
        Ok(None)
    }

    fn try_statement(&mut self) -> Result<Option<UnlabeledStmt>, ParserError> {
        Ok(None)
    }

    fn raise_statement(&mut self) -> Result<Option<UnlabeledStmt>, ParserError> {
        Ok(None)
    }

    fn ident_list(&mut self) -> Result<Vec<String>, ParserError> {
        let mut identities = vec![];
        while let Some(ident) = self.identifier() {
            identities.push(ident)
        }
        Ok(identities)
    }

    fn expression_list(&mut self) -> Result<Vec<Box<Expr>>, ParserError> {
        let mut exprs = vec![];
        while let Some(expr) = self.expression()? {
            exprs.push(Box::new(expr))
        }
        Ok(exprs)
    }

    // Как же это убого выглядит
    fn expression(&mut self) -> Result<Option<Expr>, ParserError> {
        if let Some(expr) = self.simple_expression()? {
            let mut expr = expr;
            if let Some(token) = self.peek() {
                match token.kind {
                    TokenType::Equal
                    | TokenType::NotEqual
                    | TokenType::Less
                    | TokenType::Greater
                    | TokenType::LessEqual
                    | TokenType::GreaterEqual
                    | TokenType::In => {
                        self.advance();
                        let right = self.expected_expression(&token)?;
                        expr = Expr::Binary {
                            left: Box::new(expr),
                            operator: token.clone(),
                            right: Box::new(right),
                        };
                    }
                    _ => {}
                }
            }
            Ok(Some(expr))
        } else {
            Ok(None)
        }
    }

    fn expected_expression(&mut self, prev_tok: &Token) -> Result<Expr, ParserError> {
        if let Some(expr) = self.expression()? {
            Ok(expr)
        } else {
            Err(error_tok(ParserErrorType::ExpectedExpression, prev_tok))
        }
    }

    fn simple_expression(&mut self) -> Result<Option<Expr>, ParserError> {
        if let Some(expr) = self.term()? {
            let mut expr = expr;
            if let Some(token) = self.peek() {
                match token.kind {
                    TokenType::Plus | TokenType::Minus | TokenType::Or => {
                        self.advance();
                        let right = self.expected_simple_expression(&token)?;
                        expr = Expr::Binary {
                            left: Box::new(expr),
                            operator: token.clone(),
                            right: Box::new(right),
                        };
                    }
                    _ => {}
                }
            }
            Ok(Some(expr))
        } else {
            Ok(None)
        }
    }

    fn expected_simple_expression(&mut self, prev_tok: &Token) -> Result<Expr, ParserError> {
        if let Some(expr) = self.simple_expression()? {
            Ok(expr)
        } else {
            Err(error_tok(
                ParserErrorType::ExpectedSimpleExpression,
                prev_tok,
            ))
        }
    }

    fn term(&mut self) -> Result<Option<Expr>, ParserError> {
        if let Some(expr) = self.signed_factor()? {
            let mut expr = expr;
            while let Some(token) = self.peek() {
                match token.kind {
                    TokenType::Star
                    | TokenType::Slash
                    | TokenType::Div
                    | TokenType::Mod
                    | TokenType::And => {
                        self.advance();
                        let right = self.expected_term(&token)?;
                        expr = Expr::Binary {
                            left: Box::new(expr),
                            operator: token.clone(),
                            right: Box::new(right),
                        };
                    }
                    _ => break,
                }
            }
            Ok(Some(expr))
        } else {
            Ok(None)
        }
    }

    fn expected_term(&mut self, prev_tok: &Token) -> Result<Expr, ParserError> {
        if let Some(expr) = self.term()? {
            Ok(expr)
        } else {
            Err(error_tok(ParserErrorType::ExpectedTerm, prev_tok))
        }
    }

    fn signed_factor(&mut self) -> Result<Option<Expr>, ParserError> {
        if let Some(token) = self.peek() {
            match token.kind {
                TokenType::Plus | TokenType::Minus => {
                    self.advance();
                    let right = self.expected_factor(&token)?;
                    Ok(Some(Expr::Unary {
                        operator: token.clone(),
                        right: Box::new(right),
                    }))
                }
                _ => self.factor(),
            }
        } else {
            Ok(None)
        }
    }

    /// TODO: необходимо разработать для себя "особые" методы анализа кода на Rust
    fn factor(&mut self) -> Result<Option<Expr>, ParserError> {
        let result = match self.peek() {
            // '@' factor
            Some(
                token @ Token {
                    kind: TokenType::Bleat,
                    ..
                },
            ) => {
                self.advance();
                let expr = self.expected_factor(&token)?;
                Ok(Some(Expr::Unary {
                    operator: token.clone(),
                    right: Box::new(expr),
                }))
            }
            // DOUBLEAT factor
            Some(
                token @ Token {
                    kind: TokenType::DoubleBleat,
                    ..
                },
            ) => {
                self.advance();
                let _expr = self.expected_factor(&token)?;
                Ok(Some(Expr::Literal {
                    value: token.clone(),
                }))
            }
            // 'not' factor
            Some(
                token @ Token {
                    kind: TokenType::Not,
                    ..
                },
            ) => {
                self.advance();
                let expr = self.expected_factor(&token)?;
                Ok(Some(Expr::Unary {
                    operator: token.clone(),
                    right: Box::new(expr),
                }))
            }
            // '^' factor
            Some(
                token @ Token {
                    kind: TokenType::Pointer2,
                    ..
                },
            ) => {
                self.advance();
                let expr = self.expected_factor(&token)?;
                Ok(Some(Expr::Unary {
                    operator: token.clone(),
                    right: Box::new(expr),
                }))
            }
            // unsignedConstant
            Some(
                token @ Token {
                    kind:
                        TokenType::UnsignedInteger(_)
                        | TokenType::UnsignedReal(_)
                        | TokenType::StringLiteral(_)
                        | TokenType::Nil,
                    ..
                },
            ) => {
                self.advance();
                Ok(Some(Expr::Literal {
                    value: token.clone(),
                }))
            }
            // bool_
            Some(
                token @ Token {
                    kind: TokenType::True | TokenType::False,
                    ..
                },
            ) => {
                self.advance();
                Ok(Some(Expr::Literal {
                    value: token.clone(),
                }))
            }
            // LPAREN expression RPAREN
            Some(
                token @ Token {
                    kind: TokenType::LeftParen,
                    ..
                },
            ) => {
                self.advance();
                let expr = self.expected_expression(&token)?;
                self.consume(TokenType::RightParen, token.as_ref().into())?;
                Ok(Some(expr))
            }
            _ => Ok(None),
        };

        if let Some(result) = result? {
            Ok(Some(result))
        } else if let Some(designator) = self.designator()? {
            Ok(Some(Expr::Designator { designator }))
        } else {
            Ok(None)
        }
    }

    fn expected_factor(&mut self, prev_tok: &Token) -> Result<Expr, ParserError> {
        if let Some(expr) = self.factor()? {
            Ok(expr)
        } else {
            Err(error_tok(ParserErrorType::ExpectedFactor, prev_tok))
        }
    }
}
