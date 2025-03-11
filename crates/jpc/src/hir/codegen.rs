use core::{
    ast::{
        Block, DeclSection, Expr, ProcedureDeclaration, Program, Stmt,
        TypeDeclaration, UnlabeledStmt, VarDeclaration,
    },
    lexer,
};
use std::{cell::RefCell, rc::Rc};

use super::basic_block::{BasicBlock, Constant, Function, Instruction, InstructionKind, Module, Type, Var};

#[derive(Debug)]
pub enum CodegenError {
    NotImplemented,
    UndefinedVariable,
    AssigmentToNotDefinedVariable,
    NoParentEnvironment,
    NotCallable,
    MismatchedTypes,
    MismathedArgumentsCount,
    AlreadyDefined,
}

#[derive(Debug, Clone)]
pub struct CodeEmmiter {
    pub module: Rc<Module>,
    pub func: Function,
    pub bb: BasicBlock,
    pub tmp_var_number: usize,
    pub tmp_lbl_number: usize,
}

impl CodeEmmiter {
    fn add_const(&mut self, constant: Constant) -> usize {
        let num = self.func.constants.len();
        self.func.constants.push(constant);
        num
    }

    fn add_tmp_var(&mut self) -> Rc<Var> {
        let var_number = self.tmp_var_number;
        self.tmp_var_number += 1;
        let var = Var {
            name: format!("t{}", var_number),
            type_: Type::Integer,
            use_count: 0,
        };
        Rc::new(var)
    }

    fn add_label(&mut self) -> String {
        let lbl_number = self.tmp_lbl_number;
        self.tmp_lbl_number += 1;
        format!("L{}", lbl_number)
    }

    fn visit_expr(&mut self, dst: Rc<Var>, expr: &Box<Expr>) -> Result<(), CodegenError> {
        match expr.as_ref() {
            Expr::Binary {
                left,
                operator,
                right,
            } => {
                let src1 = self.add_tmp_var();
                self.visit_expr(src1.clone(), left)?;
                let src2 = self.add_tmp_var();
                self.visit_expr(src2.clone(), right)?;
                let inst = match operator.kind {
                    lexer::TokenType::Plus => Ok(InstructionKind::Add {
                        dst: dst.clone(),
                        src1,
                        src2,
                    }),
                    lexer::TokenType::Minus => Ok(InstructionKind::Sub {
                        dst: dst.clone(),
                        src1,
                        src2,
                    }),
                    lexer::TokenType::Star => Ok(InstructionKind::Mul {
                        dst: dst.clone(),
                        src1,
                        src2,
                    }),
                    lexer::TokenType::Slash => Ok(InstructionKind::Div {
                        dst: dst.clone(),
                        src1,
                        src2,
                    }),
                    _ => Err(CodegenError::NotImplemented),
                }?;
                self.bb.add_instruction(inst);
                Ok(())
            }
            Expr::Unary { operator, right } => {
                let src = self.add_tmp_var();
                self.visit_expr(src.clone(), right)?;
                match operator.kind {
                    lexer::TokenType::Minus => {
                        let inst = InstructionKind::Negate {
                            dst: dst.clone(),
                            src,
                        };
                        self.bb.add_instruction(inst);
                        Ok(())
                    }
                    _ => Err(CodegenError::NotImplemented),
                }
            }
            Expr::Literal { value } => match value.kind {
                lexer::TokenType::UnsignedInteger(ref num) => match num.parse::<i64>() {
                    Ok(num) => {
                        let num = self.add_const(Constant::Integer(num));
                        let inst = InstructionKind::LoadConst {
                            dst: dst.clone(),
                            num,
                        };
                        self.bb.add_instruction(inst);
                        Ok(())
                    }
                    Err(_) => Err(CodegenError::NotImplemented),
                },
                lexer::TokenType::UnsignedReal(ref num) => match num.parse::<f64>() {
                    Ok(num) => {
                        let num = self.add_const(Constant::Real(num));
                        let inst = InstructionKind::LoadConst {
                            dst: dst.clone(),
                            num,
                        };
                        self.bb.add_instruction(inst);
                        Ok(())
                    }
                    Err(_) => Err(CodegenError::NotImplemented),
                },
                lexer::TokenType::True => {
                    let num = self.add_const(Constant::Integer(1));
                    let inst = InstructionKind::LoadConst {
                        dst: dst.clone(),
                        num,
                    };
                    self.bb.add_instruction(inst);
                    Ok(())
                }
                lexer::TokenType::False => {
                    let num = self.add_const(Constant::Integer(0));
                    let inst = InstructionKind::LoadConst {
                        dst: dst.clone(),
                        num,
                    };
                    self.bb.add_instruction(inst);
                    Ok(())
                }
                lexer::TokenType::Nil => {
                    let num = self.add_const(Constant::Integer(0));
                    let inst = InstructionKind::LoadConst {
                        dst: dst.clone(),
                        num,
                    };
                    self.bb.add_instruction(inst);
                    Ok(())
                }
                lexer::TokenType::StringLiteral(ref val) => {
                    let num = self.add_const(Constant::String(val.clone()));
                    let inst = InstructionKind::LoadConst {
                        dst: dst.clone(),
                        num,
                    };
                    self.bb.add_instruction(inst);
                    Ok(())
                }
                _ => Err(CodegenError::NotImplemented),
            },
            Expr::Designator { designator } => {
                let src = Var {
                    name: designator.name.clone(),
                    type_: Type::Integer,
                    use_count: 0,
                };
                let src = Rc::new(src);
                let inst = InstructionKind::Assign {
                    dst: dst.clone(),
                    src,
                };
                self.bb.add_instruction(inst);
                Ok(())
            },
        }
    }

    fn visit_type_declaration(&mut self, _decl: &TypeDeclaration) -> Result<(), CodegenError> {
        Ok(())
    }

    fn visit_var_declaration(&mut self, decl: &VarDeclaration) -> Result<(), CodegenError> {
        Ok(())
    }

    fn visit_procedure_declaration(
        &mut self,
        decl: &ProcedureDeclaration,
    ) -> Result<(), CodegenError> {
        let root_block = Rc::new(RefCell::new(BasicBlock::new()));

        let params = decl.head.params.iter().map(|param| {
            let var = Var {
                name: param.ident.clone(),
                type_: Type::Integer,
                use_count: 0,
            };
            var
        }).collect::<Vec<_>>();
        let func = Function {
            name: decl.head.name.clone(),
            params,
            return_var: None,
            root_block: Rc::clone(&root_block),
            exit_block: None,
            constants: vec![],
            variables: vec![],
            belongs_to: Rc::downgrade(&self.module),
        };
        let func = Rc::new(func);
        let mut rb = root_block.borrow_mut();
        rb.belong_to = Rc::downgrade(&func);
        let def_inst = InstructionKind::Def { func_name: decl.head.name.clone() };
        rb.add_instruction(def_inst);

        self.visit_statement(&decl.body)?;
        Ok(())
    }

    fn visit_label_declaration(&mut self, decl: &str) -> Result<(), CodegenError> {
        Ok(())
    }

    fn visit_const_declaration(&mut self, decl: &(String, Box<Expr>)) -> Result<(), CodegenError> {
        Ok(())
    }

    fn visit_decl_section(&mut self, section: &DeclSection) -> Result<(), CodegenError> {
        match section {
            DeclSection::Label(label_decl) => {
                for decl in label_decl.iter() {
                    self.visit_label_declaration(decl)?;
                }
                Ok(())
            }
            DeclSection::Const(const_decl) => {
                for decl in const_decl.iter() {
                    self.visit_const_declaration(decl)?;
                }
                Ok(())
            }
            DeclSection::Type(type_decl) => {
                for decl in type_decl.iter() {
                    self.visit_type_declaration(decl)?;
                }
                Ok(())
            }
            DeclSection::Variable(var_decl) => {
                for decl in var_decl.iter() {
                    self.visit_var_declaration(decl)?;
                }
                Ok(())
            }
            DeclSection::Procedure(proc_decl) => {
                self.visit_procedure_declaration(proc_decl)?;
                Ok(())
            }
        }
    }

    fn visit_statement(&mut self, stmt: &Box<Stmt>) -> Result<(), CodegenError> {
        match stmt.statement.as_ref() {
            UnlabeledStmt::Assigment { left, right } => {
                let dst = Var {
                    name: left.name.clone(),
                    type_: Type::Integer,
                    use_count: 0,
                };
                let dst = Rc::new(dst);
                self.visit_expr(dst, right)?;
                Ok(())
            }
            UnlabeledStmt::Compound { statements } => {
                for statement in statements {
                    self.visit_statement(statement)?;
                }
                Ok(())
            }
            UnlabeledStmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                Ok(())
            }
            UnlabeledStmt::Repeat {
                statements,
                condition,
            } => {
                Ok(())
            }
            UnlabeledStmt::While {
                condition,
                statement,
            } => {
                Ok(())
            }
            UnlabeledStmt::For {
                var,
                init,
                to,
                statement,
                is_down_to,
            } => {
                Ok(())
            }
            _ => Ok(()),
        }
    }

    fn visit_block(&mut self, block: &Box<Block>) -> Result<(), CodegenError> {
        for section in block.decl_sections.iter() {
            self.visit_decl_section(section)?;
        }
        self.visit_statement(&block.body)
    }

    pub fn visit_program(&mut self, program: &Box<Program>) -> Result<(), CodegenError> {
        self.module = Rc::new(Module::new());
        self.visit_block(&program.block)
    }
}
