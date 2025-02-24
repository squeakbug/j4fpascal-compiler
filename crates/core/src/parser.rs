use thiserror::Error;

use crate::{
    ast::{Block, CaseItem, CaseLabel, DeclSection, Designator, DesignatorItem, Expr, ProcedureDeclaration, Stmt, UnlabeledStmt, VarDeclaration},
    lexer::{Token, TokenType},
};

pub struct Parser {
    istream: Option<Vec<Token>>,
    current: usize,
}

#[derive(Error, Debug)]
pub enum ParserError {
    #[error("Unexpected token at position {0}")]
    UnexpectedToken(usize),

    #[error("Unexpected EOF at position {0}")]
    UnexpectedEOF(usize),
}

impl Parser {
    pub fn new() -> Self {
        Parser {
            istream: None,
            current: 0,
        }
    }

    pub fn parse(&mut self, tokens: Vec<Token>) -> Result<Option<Block>, ParserError> {
        self.istream = Some(tokens);
        self.block()
    }

    fn peek(&mut self) -> Option<Token> {
        self.istream.as_mut().unwrap().get(self.current).cloned()
    }

    fn advance(&mut self) -> Option<Token> {
        let result = self.istream.as_mut().unwrap().get(self.current).cloned();
        self.current += 1;
        result
    }

    fn consume(&mut self, expected: TokenType) -> Result<Token, ParserError> {
        match self.advance() {
            Some(ref token@Token { ref token_type, .. }) if *token_type == expected => {
                Ok(token.clone())
            },
            None => Err(ParserError::UnexpectedToken(self.current)),
            _ => Err(ParserError::UnexpectedToken(self.current)),
        }
    }

    fn synchronize(&mut self) {
        self.advance();
        while let Some(token) = self.peek() {
            if token.token_type == TokenType::Semicolon {
                return;
            }
            match token.token_type {
                TokenType::Procedure
                | TokenType::Function
                | TokenType::Var
                | TokenType::For
                | TokenType::If => {
                    break;
                },
                _ => { let _ = self.advance(); }
            }
        }
    }

    fn block(&mut self) -> Result<Option<Block>, ParserError> {
        let mut decl_sections = vec![];
        while let Some(declaration) = self.decl_section()? {
            decl_sections.push(declaration);
        }
        let body = Box::new(self.compound_statement()?.unwrap().into());
        Ok(Some(Block {
            decl_sections,
            body,
        }))
    }

    fn decl_section(&mut self) -> Result<Option<DeclSection>, ParserError> {
        let mut declarations = vec![];
        while let Some(declaration) = self.procedure_declaration()? {
            declarations.push(declaration);
        }
        Ok(Some(DeclSection::Procedure(declarations)))
    }

    fn procedure_declaration(&mut self) -> Result<Option<ProcedureDeclaration>, ParserError> {
        Ok(None)
    }

    fn statement(&mut self) -> Result<Option<Stmt>, ParserError> {
        if let Some(token) = self.peek() {
            if token.token_type == TokenType::Label {
                self.advance();
                self.consume(TokenType::Colon)?;
                match self.unlabeled_statement()? {
                    Some(statement) => {
                        return Ok(Some(Stmt {
                            label: Some(token),
                            statement: Box::new(statement),
                        }));
                    },
                    None => return Err(ParserError::UnexpectedToken(self.current)),
                }
            } else {
                return Ok(Some(Stmt {
                    label: Some(token),
                    statement: Box::new(self.unlabeled_statement()?.unwrap()),
                }));
            }
        }
        Ok(None)
    }

    fn unlabeled_statement(&mut self) -> Result<Option<UnlabeledStmt>, ParserError> {
        if let Some(stmt) = self.if_statement()? {
            Ok(Some(stmt))
        } else if let Some(stmt) = self.case_statement()? {
            Ok(Some(stmt))
        } else if let Some(stmt) = self.repeat_statement()? {
            Ok(Some(stmt))
        } else if let Some(stmt) = self.while_statement()? {
            Ok(Some(stmt))
        } else if let Some(stmt) = self.for_statement()? {
            Ok(Some(stmt))
        } else if let Some(stmt) = self.with_statement()? {
            Ok(Some(stmt))
        } else if let Some(stmt) = self.try_statement()? {
            Ok(Some(stmt))
        } else if let Some(stmt) = self.raise_statement()? {
            Ok(Some(stmt))
        } else if let Some(stmt) = self.compound_statement()? {
            Ok(Some(stmt))
        } else if let Some(stmt) = self.simple_statement()? {
            Ok(Some(stmt))
        } else {
            Ok(None)
        }
    }

    fn goto_statement(&mut self) -> Result<Option<UnlabeledStmt>, ParserError> {
        if let Some(token) = self.peek() {
            match token {
                Token { token_type: TokenType::Goto, .. } => {
                    return Ok(Some(UnlabeledStmt::Goto { label: token }))
                },
                Token { token_type: TokenType::Break, .. } => {
                    return Ok(Some(UnlabeledStmt::Break))
                },
                Token { token_type: TokenType::Continue, .. } => {
                    return Ok(Some(UnlabeledStmt::Continue))
                },
                _ => return Ok(None)
            }
        }
        Ok(None)
    }

    fn colon_construct(&mut self) -> Result<Vec<Box<Expr>>, ParserError> {
        let mut expressions = vec![];
        loop {
            if let Some(token) = self.peek() {
                if token.token_type == TokenType::Colon {
                    self.advance();
                    let expression = Box::new(self.expression()?.unwrap());
                    expressions.push(expression)
                } else {
                    break;
                }
            }
        }
        Ok(expressions)
    }

    fn actual_parameter(&mut self) -> Result<Option<(Box<Expr>, Vec<Box<Expr>>)>, ParserError> {
        let parameter = Box::new(self.expression()?.unwrap());
        let colon_construct = self.colon_construct()?;
        Ok(Some((parameter, colon_construct)))
    }

    fn parameter_list(&mut self) -> Result<Vec<(Box<Expr>, Vec<Box<Expr>>)>, ParserError> {
        let mut parameters = vec![];
        if let Some(parameter) = self.actual_parameter()? {
            parameters.push(parameter);
            while let Some(token) = self.peek() {
                if token.token_type == TokenType::Comma {
                    self.advance();
                    if let Some(parameter) = self.actual_parameter()? {
                        parameters.push(parameter);
                    } else {
                        return Err(ParserError::UnexpectedToken(self.current));
                    }
                } else {
                    break;
                }
            }
        }
        return Ok(parameters);
    }

    fn procedure_call(&mut self) -> Result<Option<DesignatorItem>, ParserError> {
        match self.peek() {
            Some(Token { token_type: TokenType::LeftParen,.. }) => {
                self.advance();
                let arguments = self.parameter_list()?;
                self.consume(TokenType::RightParen)?;
                Ok(Some(DesignatorItem::Call {
                    arguments,
                }))
            }
            _ => Err(ParserError::UnexpectedEOF(self.current)),
        }
    }

    fn array_access(&mut self) -> Result<Option<DesignatorItem>, ParserError> {
        match self.peek() {
            Some(Token { token_type: TokenType::LeftBrack,.. }) => {
                self.advance();
                let indexes = self.expression_list()?;
                self.consume(TokenType::RightBrack)?;
                Ok(Some(DesignatorItem::ArrayAccess {
                    indexes,
                }))
            }
            _ => Err(ParserError::UnexpectedEOF(self.current)),
        }
    }

    fn designator_item(&mut self) -> Result<Option<DesignatorItem>, ParserError> {
        if let Some(array_access) = self.array_access()? {
            return Ok(Some(array_access));
        } else if let Some(procedure_call) = self.procedure_call()? {
            return Ok(Some(procedure_call));
        } else {
            Ok(None)
        }
    }

    fn qualified_ident(&mut self) -> Result<Option<String>, ParserError> {
        match self.peek() {
            Some(Token { token_type: TokenType::Identifier(name), .. }) => {
                self.advance();
                return Ok(Some(name));
            },
            None => Err(ParserError::UnexpectedEOF(self.current)),
            _ => Ok(None),
        }
    }

    fn designator(&mut self) -> Result<Option<Designator>, ParserError> {
        let name = self.qualified_ident()?.unwrap();
        let mut items = vec![];
        while let Some(item) = self.designator_item()? {
            items.push(item);
        }
        Ok(Some(Designator {
            name,
            items,
        }))
    }

    // TODO: add structured identifier
    fn simple_statement(&mut self) -> Result<Option<UnlabeledStmt>, ParserError> {
        if let Some(designator) = self.designator()? {
            if let Some(token) = self.peek() {
                if token.token_type == TokenType::Assignment {
                    self.advance();
                    let expression = self.expression()?;
                    return Ok(Some(UnlabeledStmt::Assigment {
                        left: designator,
                        right: Box::new(expression.unwrap()),
                    }));
                } else {
                    return Ok(Some(UnlabeledStmt::ProcedureCall {
                        designator,
                    }));
                }
            }
        } else if let Some(stmt) = self.goto_statement()? {
            return Ok(Some(stmt));
        }
        Ok(None)
    }

    fn statement_list(&mut self) -> Result<Vec<Box<Stmt>>, ParserError> {
        let mut statements = vec![];
        if let Some(statement) = self.statement()? {
            statements.push(Box::new(statement));
            while let Some(token) = self.peek() {
                if token.token_type == TokenType::Semicolon {
                    self.advance();
                    let statement = self.statement()?;
                    match statement {
                        Some(statement) => statements.push(Box::new(statement)),
                        None => continue,
                    }
                } else {
                    break;
                }
            }
        }
        return Ok(statements);
    }

    fn compound_statement(&mut self) -> Result<Option<UnlabeledStmt>, ParserError> {
        self.consume(TokenType::Begin)?;
        let statements = self.statement_list()?;
        self.consume(TokenType::End)?;
        Ok(Some(UnlabeledStmt::Compound {
            statements
        }))
    }

    fn if_statement(&mut self) -> Result<Option<UnlabeledStmt>, ParserError> {
        if let Some(token) = self.peek() {
            if token.token_type == TokenType::If {
                self.advance();
                let condition = Box::new(self.expression()?.unwrap());
                self.consume(TokenType::Then)?;
                let then_branch = Box::new(self.statement()?.unwrap());
                let mut else_branch = None;
                if let Some(token) = self.peek() {
                    if token.token_type == TokenType::Else {
                        self.advance();
                        else_branch = Some(Box::new(self.statement()?.unwrap()));
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

    fn case_label(&mut self) -> Result<CaseLabel, ParserError> {
        let from = Box::new(self.expression()?.unwrap());
        if let Some(token) = self.peek() {
            if token.token_type == TokenType::DotDot {
                self.advance();
                let to = Box::new(self.expression()?.unwrap());
                return Ok(CaseLabel::Range((from, to)));
            }
        }
        return Ok(CaseLabel::Simple(from));
    }

    fn case_item(&mut self) -> Result<CaseItem, ParserError> {
        let mut labels = vec![self.case_label()?];
        while let Some(token) = self.peek() {
            if token.token_type == TokenType::Comma {
                self.advance();
                labels.push(self.case_label()?);
            } else {
                break;
            }
        }
        self.consume(TokenType::Colon)?;
        let statement = Box::new(self.statement()?.unwrap());
        Ok(CaseItem {
            labels,
            statement,
        })
    }

    fn case_statement(&mut self) -> Result<Option<UnlabeledStmt>, ParserError> {
        if let Some(token) = self.peek() {
            if token.token_type == TokenType::Case {
                self.advance();
                let condition = Box::new(self.expression()?.unwrap());
                self.consume(TokenType::Of)?;
                let mut case_items = vec![];
                while let Ok(item) = self.case_item() {
                    case_items.push(Box::new(item));
                }
                let mut else_branch = None;
                if let Some(token) = self.peek() {
                    if token.token_type == TokenType::Else {
                        self.advance();
                        else_branch = Some(Box::new(self.statement()?.unwrap()));
                    }
                }
                self.consume(TokenType::End)?;
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
            if token.token_type == TokenType::Repeat {
                self.advance();
                let statements = self.statement_list()?;
                self.consume(TokenType::Until)?;
                let expr = self.expression()?.unwrap();
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
            if token.token_type == TokenType::While {
                self.advance();
                let condition = Box::new(self.expression()?.unwrap());
                self.consume(TokenType::Do)?;
                let statement = Box::new(self.statement()?.unwrap());
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
            if token.token_type == TokenType::For {
                self.advance();
                let var = Box::new(self.designator()?.unwrap());
                self.consume(TokenType::Assignment)?;
                let init = Box::new(self.expression()?.unwrap());
                self.consume(TokenType::To)?;
                let to = Box::new(self.expression()?.unwrap());
                self.consume(TokenType::Do)?;
                let statement = Box::new(self.statement()?.unwrap());
                return Ok(Some(UnlabeledStmt::For {
                    var,
                    init,
                    to,
                    statement,
                    is_down_to: false
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

    fn expression_list(&mut self) -> Result<Vec<Box<Expr>>, ParserError> {
        todo!();
    }

    // Как же это убого выглядит
    fn expression(&mut self) -> Result<Option<Expr>, ParserError> {
        let mut expr = self.simple_expression()?.unwrap();
        if let Some(token) = self.peek() {
            match token.token_type {
                TokenType::Equal 
                | TokenType::NotEqual
                | TokenType::Less
                | TokenType::Greater
                | TokenType::LessEqual
                | TokenType::GreaterEqual
                | TokenType::In => {
                    self.advance();
                    let right = self.expression()?.unwrap();
                    expr = Expr::Binary {
                        left: Box::new(expr),
                        operator: token.clone(),
                        right: Box::new(right),
                    };
                }
                _ => { },
            }
        }
        Ok(Some(expr))
    }

    fn simple_expression(&mut self) -> Result<Option<Expr>, ParserError> {
        let mut expr = self.term()?.unwrap();
        if let Some(token) = self.peek() {
            match token.token_type {
                TokenType::Plus | TokenType::Minus | TokenType::Or => {
                    self.advance();
                    let right = self.simple_expression()?.unwrap();
                    expr = Expr::Binary {
                        left: Box::new(expr),
                        operator: token.clone(),
                        right: Box::new(right),
                    };
                }
                _ => { },
            }
        }
        Ok(Some(expr))
    }

    fn term(&mut self) -> Result<Option<Expr>, ParserError> {
        let mut expr = self.signed_factor()?.unwrap();
        while let Some(token) = self.peek() {
            match token.token_type {
                TokenType::Star | TokenType::Slash 
                | TokenType::Div | TokenType::Mod
                | TokenType::And => {
                    self.advance();
                    let right = self.term()?.unwrap();
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
    }

    fn signed_factor(&mut self) -> Result<Option<Expr>, ParserError> {
        if let Some(token) = self.peek() {
            match token.token_type {
                TokenType::Plus | TokenType::Minus => {
                    self.advance();
                    let right = self.factor()?.unwrap();
                    Ok(Some(Expr::Unary {
                        operator: token.clone(),
                        right: Box::new(right),
                    }))
                },
                _ => self.factor()
            }
        } else {
            Err(ParserError::UnexpectedEOF(self.current))
        }
    }

    /// TODO: необходимо разработать для себя "особые" методы анализа кода на Rust
    fn factor(&mut self) -> Result<Option<Expr>, ParserError> {
        let result = match self.peek() {
            // '@' factor
            Some(token@Token {
                token_type: TokenType::Bleat,
                ..
            }) => {
                self.advance();
                let expr = self.factor()?.unwrap();
                Ok(Some(Expr::Unary {
                    operator: token.clone(),
                    right: Box::new(expr),
                }))
            },
            // DOUBLEAT factor
            Some(token@Token {
                token_type: TokenType::DoubleBleat,
                ..
            }) => {
                self.advance();
                let expr = self.factor()?;
                Ok(Some(Expr::Literal {
                    value: token.clone(),
                }))
            },
            // 'not' factor
            Some(token@Token {
                token_type: TokenType::Not,
                ..
            }) => {
                self.advance();
                let expr = self.factor()?.unwrap();
                Ok(Some(Expr::Unary {
                    operator: token.clone(),
                    right: Box::new(expr),
                }))
            },
            // '^' factor
            Some(token@Token {
                token_type: TokenType::Pointer2,
                ..
            }) => {
                self.advance();
                let expr = self.factor()?.unwrap();
                Ok(Some(Expr::Unary {
                    operator: token.clone(),
                    right: Box::new(expr),
                }))
            },
            // unsignedConstant
            Some(
                token @ Token {
                    token_type:
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
            },
            // bool_
            Some(
                token @ Token {
                    token_type:
                        TokenType::True
                        | TokenType::False,
                    ..
                },
            ) => {
                self.advance();
                Ok(Some(Expr::Literal {
                    value: token.clone(),
                }))
            },
            // LPAREN expression RPAREN
            Some(Token {
                token_type: TokenType::LeftParen,
                ..
            }) => {
                self.advance();
                let expr = self.expression()?.unwrap();
                self.consume(TokenType::RightParen)?;
                Ok(Some(expr))
            },
            None => Err(ParserError::UnexpectedEOF(self.current)),
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
}
