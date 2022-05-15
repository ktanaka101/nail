mod arena;
mod symbol_table;

use std::{cell::RefCell, rc::Rc};
use thiserror;

use crate::hir;

use crate::ast_parser::ast;

use anyhow::Result;

impl symbol_table::Symbol for hir::Symbol {}
impl symbol_table::Value for hir::Expr<'_> {}

#[derive(thiserror::Error, Debug)]
pub enum ParseError {
    #[error("Bug: {0}")]
    Bug(String),
}

pub struct IdResolver {
    current_id: u64,
}

impl Default for IdResolver {
    fn default() -> Self {
        Self::new()
    }
}

impl IdResolver {
    pub fn new() -> Self {
        Self { current_id: 0 }
    }

    fn next_id(&mut self) -> hir::HirId {
        let id = hir::HirId::new(self.current_id);
        self.current_id += 1;
        id
    }
}

#[derive(Clone)]
pub struct Scope<'ctx> {
    current_variable_table:
        Rc<RefCell<symbol_table::SymbolTable<'ctx, hir::Symbol, hir::Expr<'ctx>>>>,
}

impl<'ctx> Default for Scope<'ctx> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'ctx> Scope<'ctx> {
    pub fn new() -> Self {
        Self {
            current_variable_table: Rc::new(RefCell::new(symbol_table::SymbolTable::new())),
        }
    }

    fn enter_scope(&mut self) {
        let table: symbol_table::SymbolTable<'ctx, hir::Symbol, hir::Expr<'ctx>> =
            symbol_table::SymbolTable::new_enclosed(Rc::clone(&self.current_variable_table));
        self.current_variable_table = Rc::new(RefCell::new(table));
    }

    fn leave_scope(&mut self) {
        let table = Rc::clone(&self.current_variable_table);
        let borrow = table.borrow();
        if let Some(outer) = &borrow.outer {
            self.current_variable_table = Rc::clone(outer);
        } else {
            panic!("no outer scope");
        }
    }

    fn define_into_current_scope(
        &mut self,
        symbol: &'ctx hir::Symbol,
        expr: &'ctx hir::Expr<'ctx>,
    ) {
        Rc::clone(&self.current_variable_table)
            .borrow_mut()
            .define(symbol, expr);
    }

    fn resolve(&self, symbol: &hir::Symbol) -> Option<&'ctx hir::Expr<'ctx>> {
        Rc::clone(&self.current_variable_table)
            .borrow()
            .resolve(symbol)
    }
}

pub struct HirParser<'hir> {
    hir_arena: &'hir arena::Arena<'hir>,
    scope: &'hir mut Scope<'hir>,
    resolver: &'hir mut IdResolver,
}

impl<'hir> HirParser<'hir> {
    pub fn new(
        hir_arena: &'hir arena::Arena<'hir>,
        scope: &'hir mut Scope<'hir>,
        resolver: &'hir mut IdResolver,
    ) -> Self {
        Self {
            hir_arena,
            scope,
            resolver,
        }
    }

    pub fn parse(
        &mut self,
        program: &ast::Program,
    ) -> Result<&'hir hir::Program<'hir>, ParseError> {
        let id = self.resolver.next_id();
        let statements = self.parse_stmts(&program.statements);

        Ok(self.hir_arena.alloc(hir::Program { id, statements }))
    }

    fn parse_stmts(&mut self, stmts: &Vec<ast::Stmt>) -> &'hir [hir::Stmt<'hir>] {
        let mut statements: Vec<hir::Stmt<'hir>> = vec![];

        for stmt in stmts {
            let id = self.resolver.next_id();
            let kind = match stmt {
                ast::Stmt::Block(block) => hir::StmtKind::Block(self.parse_block(block)),
                ast::Stmt::ExprStmt(expr_stmt) => {
                    hir::StmtKind::ExprStmt(self.parse_expr_stmt(expr_stmt))
                }
                ast::Stmt::Let(r#let) => hir::StmtKind::Let(self.parse_let(r#let)),
                ast::Stmt::Return(r#return) => hir::StmtKind::Return(self.parse_return(r#return)),
            };

            statements.push(hir::Stmt { id, kind });
        }

        self.hir_arena.alloc_from_iter(statements)
    }

    fn parse_block(&mut self, block: &ast::Block) -> &'hir hir::Block<'hir> {
        self.scope.enter_scope();

        let stmts = self.parse_stmts(&block.statements);
        let block = self.hir_arena.alloc(hir::Block { statements: stmts });

        self.scope.leave_scope();

        block
    }

    fn parse_return(&mut self, r#return: &ast::Return) -> &'hir hir::Return<'hir> {
        let expr = self.parse_expr(&r#return.return_value);

        self.hir_arena.alloc(hir::Return { expr })
    }

    fn parse_expr_stmt(&mut self, expr_stmt: &ast::ExprStmt) -> &'hir hir::ExprStmt<'hir> {
        let expr = self.parse_expr(&expr_stmt.expr);

        self.hir_arena.alloc(hir::ExprStmt { expr })
    }

    fn parse_expr_no_alloc(&mut self, expr: &ast::Expr) -> hir::ExprKind<'hir> {
        match expr {
            ast::Expr::Array(array) => hir::ExprKind::Array(self.parse_array(array)),
            ast::Expr::StringLit(string) => hir::ExprKind::StringLit(self.parse_string_lit(string)),
            ast::Expr::Boolean(boolean) => hir::ExprKind::Boolean(self.parse_boolean(boolean)),
            ast::Expr::Integer(integer) => hir::ExprKind::Integer(self.parse_integer(integer)),
            ast::Expr::Char(r#char) => hir::ExprKind::Char(self.parse_char(r#char)),
            ast::Expr::Identifier(identifier) => {
                hir::ExprKind::Identifier(self.parse_identifier(identifier))
            }
            ast::Expr::Call(call) => hir::ExprKind::Call(self.parse_call(call)),
            ast::Expr::Function(func) => hir::ExprKind::Function(self.parse_function(func)),
            ast::Expr::Hash(hash) => hir::ExprKind::Hash(self.parse_hash(hash)),
            ast::Expr::If(r#if) => hir::ExprKind::If(self.parse_if(r#if)),
            ast::Expr::Index(index) => hir::ExprKind::Index(self.parse_index(index)),
            ast::Expr::InfixExpr(infix_expr) => {
                hir::ExprKind::InfixExpr(self.parse_infix_expr(infix_expr))
            }
            ast::Expr::PrefixExpr(prefix_expr) => {
                hir::ExprKind::PrefixExpr(self.parse_prefix_expr(prefix_expr))
            }
            ast::Expr::MacroLit(_) => unreachable!(),
        }
    }

    fn parse_expr(&mut self, expr: &ast::Expr) -> &'hir hir::Expr<'hir> {
        let id = self.resolver.next_id();
        let kind = self.parse_expr_no_alloc(expr);

        self.hir_arena.alloc(hir::Expr { id, kind })
    }

    fn parse_exprs(&mut self, exprs: &Vec<ast::Expr>) -> &'hir [hir::Expr<'hir>] {
        let mut expressions: Vec<hir::Expr<'hir>> = vec![];

        for expr in exprs {
            let id = self.resolver.next_id();
            let kind = self.parse_expr_no_alloc(expr);

            expressions.push(hir::Expr { id, kind });
        }

        self.hir_arena.alloc_from_iter(expressions)
    }

    fn parse_array(&mut self, array: &ast::Array) -> &'hir hir::Array<'hir> {
        let elements = self.parse_exprs(&array.elements);

        self.hir_arena.alloc(hir::Array { elements })
    }

    fn parse_string_lit(&mut self, string: &ast::StringLit) -> &'hir hir::StringLit {
        self.hir_arena.alloc(hir::StringLit {
            value: string.value.clone(),
        })
    }

    fn parse_boolean(&mut self, boolean: &ast::Boolean) -> &'hir hir::Boolean {
        self.hir_arena.alloc(hir::Boolean {
            value: boolean.value,
        })
    }

    fn parse_integer(&mut self, integer: &ast::Integer) -> &'hir hir::Integer {
        self.hir_arena.alloc(hir::Integer {
            value: integer.value,
        })
    }

    fn parse_char(&mut self, r#char: &ast::Char) -> &'hir hir::Char {
        self.hir_arena.alloc(hir::Char {
            value: r#char.value.clone(),
        })
    }

    fn parse_identifier(&mut self, identifier: &ast::Identifier) -> &'hir hir::Identifier<'hir> {
        let symbol = self.hir_arena.alloc(hir::Symbol(identifier.value.clone()));
        self.hir_arena.alloc(hir::Identifier {
            name: symbol,
            resolved_expr: self.scope.resolve(symbol),
        })
    }

    fn parse_call(&mut self, call: &ast::Call) -> &'hir hir::Call<'hir> {
        let func = self.parse_expr(&call.func);
        let args = self.parse_exprs(&call.args);

        self.hir_arena.alloc(hir::Call { func, args })
    }

    fn parse_function(&mut self, func: &ast::Function) -> &'hir hir::Function<'hir> {
        self.scope.enter_scope();

        let name = self.hir_arena.alloc(hir::Symbol(func.name.clone()));
        let params = self.parse_params(&func.params);
        for param in params.iter() {
            self.scope.define_into_current_scope(
                param.name,
                self.hir_arena.alloc(hir::Expr {
                    id: self.resolver.next_id(),
                    kind: hir::ExprKind::FunctionParam(param),
                }),
            );
        }

        let body = self.parse_block(&func.body);

        self.scope.leave_scope();

        self.hir_arena.alloc(hir::Function {
            name,
            params,
            body,
            fn_type: match func.fn_type {
                ast::FunctionType::Function => hir::FunctionType::Function,
                ast::FunctionType::Closure => hir::FunctionType::Closure,
            },
        })
    }

    fn parse_hash(&mut self, hash: &ast::Hash) -> &'hir hir::Hash<'hir> {
        let mut pairs: Vec<hir::HashPair<'hir>> = vec![];

        for pair in hash.pairs.iter() {
            let key = self.parse_expr(&pair.key);
            let value = self.parse_expr(&pair.value);

            pairs.push(hir::HashPair { key, value });
        }

        self.hir_arena.alloc(hir::Hash {
            pairs: self.hir_arena.alloc_from_iter(pairs),
        })
    }

    fn parse_if(&mut self, r#if: &ast::If) -> &'hir hir::If<'hir> {
        let condition = self.parse_expr(&r#if.cond);
        let then_branch = self.parse_block(&r#if.consequence);
        let else_branch = r#if
            .alternative
            .as_ref()
            .map(|alternative| self.parse_block(alternative));

        self.hir_arena.alloc(hir::If {
            condition,
            then_branch,
            else_branch,
        })
    }

    fn parse_index(&mut self, index: &ast::Index) -> &'hir hir::Index<'hir> {
        let target = self.parse_expr(&index.left);
        let index = self.parse_expr(&index.index);

        self.hir_arena.alloc(hir::Index { target, index })
    }

    fn parse_infix_expr(&mut self, infix_expr: &ast::InfixExpr) -> &'hir hir::InfixExpr<'hir> {
        let left = self.parse_expr(&infix_expr.left);
        let operator = self.parse_operator(&infix_expr.ope);
        let right = self.parse_expr(&infix_expr.right);

        self.hir_arena.alloc(hir::InfixExpr {
            left,
            operator,
            right,
        })
    }

    fn parse_operator(&mut self, operator: &ast::Operator) -> hir::Operator {
        match operator {
            ast::Operator::Assign => hir::Operator::Assign,
            ast::Operator::Plus => hir::Operator::Plus,
            ast::Operator::Minus => hir::Operator::Minus,
            ast::Operator::Bang => hir::Operator::Bang,
            ast::Operator::Asterisk => hir::Operator::Asterisk,
            ast::Operator::Slash => hir::Operator::Slash,
            ast::Operator::Equal => hir::Operator::Equal,
            ast::Operator::NotEqual => hir::Operator::NotEqual,
            ast::Operator::Lt => hir::Operator::Lt,
            ast::Operator::Gt => hir::Operator::Gt,
        }
    }

    fn parse_prefix_expr(&mut self, prefix_expr: &ast::PrefixExpr) -> &'hir hir::PrefixExpr<'hir> {
        let operator = self.parse_operator(&prefix_expr.ope);
        let target = self.parse_expr(&prefix_expr.right);

        self.hir_arena.alloc(hir::PrefixExpr { operator, target })
    }

    fn parse_let(&mut self, r#let: &ast::Let) -> &'hir hir::Let<'hir> {
        let let_symbol = self.hir_arena.alloc(hir::Symbol(r#let.name.value.clone()));
        let let_value = self.parse_expr(&r#let.value);
        self.scope.define_into_current_scope(let_symbol, let_value);

        self.hir_arena.alloc(hir::Let {
            name: let_symbol,
            value: let_value,
        })
    }

    fn parse_params(
        &mut self,
        identifiers: &Vec<ast::Identifier>,
    ) -> &'hir [hir::FunctionParam<'hir>] {
        let mut params: Vec<hir::FunctionParam<'hir>> = vec![];

        for identifier in identifiers {
            let symbol = self.hir_arena.alloc(hir::Symbol(identifier.value.clone()));

            params.push(hir::FunctionParam { name: symbol });
        }

        self.hir_arena.alloc_from_iter(params)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    struct Context<'a> {
        arena: arena::Arena<'a>,
        scope: Scope<'a>,
        resolver: IdResolver,
    }
    impl<'a> Context<'a> {
        fn new() -> Context<'a> {
            let arena = arena::Arena::new();
            let scope = Scope::new();
            let resolver = IdResolver::new();

            Context {
                arena,
                scope,
                resolver,
            }
        }
    }

    #[test]
    fn test_parse() {
        let mut context = Context::new();
        let mut parser = HirParser::new(&context.arena, &mut context.scope, &mut context.resolver);

        let program = ast::Program {
            statements: vec![
                ast::Stmt::Let(ast::Let {
                    name: ast::Identifier {
                        value: "x".to_string(),
                        mtype: None,
                    },
                    value: ast::Expr::Integer(ast::Integer { value: 1 }),
                }),
                ast::Stmt::Let(ast::Let {
                    name: ast::Identifier {
                        value: "y".to_string(),
                        mtype: None,
                    },
                    value: ast::Expr::Integer(ast::Integer { value: 2 }),
                }),
                ast::Stmt::ExprStmt(ast::ExprStmt {
                    expr: ast::Expr::InfixExpr(ast::InfixExpr {
                        left: Box::new(ast::Expr::Identifier(ast::Identifier {
                            value: "x".to_string(),
                            mtype: None,
                        })),
                        ope: ast::Operator::Plus,
                        right: Box::new(ast::Expr::Identifier(ast::Identifier {
                            value: "y".to_string(),
                            mtype: None,
                        })),
                    }),
                }),
            ],
        };

        let hir = parser.parse(&program);
        let hir = hir.expect("parse error");

        assert_eq!(&hir.id, &hir::HirId::new(0));

        let integer_a = hir::Expr {
            id: hir::HirId::new(2),
            kind: hir::ExprKind::Integer(&hir::Integer { value: 1 }),
        };
        let integer_b = hir::Expr {
            id: hir::HirId::new(4),
            kind: hir::ExprKind::Integer(&hir::Integer { value: 2 }),
        };
        assert_eq!(
            &hir.statements[0],
            &hir::Stmt {
                id: hir::HirId::new(1),
                kind: hir::StmtKind::Let(&hir::Let {
                    name: &hir::Symbol("x".to_string()),
                    value: &integer_a
                })
            }
        );
        assert_eq!(
            &hir.statements[1],
            &hir::Stmt {
                id: hir::HirId::new(3),
                kind: hir::StmtKind::Let(&hir::Let {
                    name: &hir::Symbol("y".to_string()),
                    value: &integer_b
                })
            }
        );
        assert_eq!(
            &hir.statements[2],
            &hir::Stmt {
                id: hir::HirId::new(5),
                kind: hir::StmtKind::ExprStmt(&hir::ExprStmt {
                    expr: &hir::Expr {
                        id: hir::HirId::new(6),
                        kind: hir::ExprKind::InfixExpr(&hir::InfixExpr {
                            left: &hir::Expr {
                                id: hir::HirId::new(7),
                                kind: hir::ExprKind::Identifier(&hir::Identifier {
                                    name: &hir::Symbol("x".to_string()),
                                    resolved_expr: Some(&integer_a)
                                }),
                            },
                            operator: hir::Operator::Plus,
                            right: &hir::Expr {
                                id: hir::HirId::new(8),
                                kind: hir::ExprKind::Identifier(&hir::Identifier {
                                    name: &hir::Symbol("y".to_string()),
                                    resolved_expr: Some(&integer_b)
                                }),
                            },
                        }),
                    }
                })
            }
        )
    }

    #[test]
    fn test_if_with_entered_scope() {
        let mut context = Context::new();
        let mut parser = HirParser::new(&context.arena, &mut context.scope, &mut context.resolver);

        let program = ast::Program {
            statements: vec![
                ast::Stmt::ExprStmt(ast::ExprStmt {
                    expr: ast::Expr::If(ast::If {
                        r#type: None,
                        cond: Box::new(ast::Expr::Boolean(ast::Boolean { value: true })),
                        consequence: ast::Block {
                            tokens: Default::default(),
                            statements: vec![
                                ast::Stmt::Let(ast::Let {
                                    name: ast::Identifier {
                                        value: "x".to_string(),
                                        mtype: None,
                                    },
                                    value: ast::Expr::Integer(ast::Integer { value: 1 }),
                                }),
                                ast::Stmt::ExprStmt(ast::ExprStmt {
                                    expr: ast::Expr::Identifier(ast::Identifier {
                                        value: "x".to_string(),
                                        mtype: None,
                                    }),
                                }),
                            ],
                        },
                        alternative: Some(ast::Block {
                            statements: vec![
                                ast::Stmt::Let(ast::Let {
                                    name: ast::Identifier {
                                        value: "y".to_string(),
                                        mtype: None,
                                    },
                                    value: ast::Expr::Integer(ast::Integer { value: 2 }),
                                }),
                                ast::Stmt::ExprStmt(ast::ExprStmt {
                                    expr: ast::Expr::Identifier(ast::Identifier {
                                        value: "y".to_string(),
                                        mtype: None,
                                    }),
                                }),
                                ast::Stmt::ExprStmt(ast::ExprStmt {
                                    expr: ast::Expr::Identifier(ast::Identifier {
                                        value: "x".to_string(),
                                        mtype: None,
                                    }),
                                }),
                            ],
                            tokens: Default::default(),
                        }),
                    }),
                }),
                ast::Stmt::ExprStmt(ast::ExprStmt {
                    expr: ast::Expr::Identifier(ast::Identifier {
                        value: "x".to_string(),
                        mtype: None,
                    }),
                }),
                ast::Stmt::ExprStmt(ast::ExprStmt {
                    expr: ast::Expr::Identifier(ast::Identifier {
                        value: "y".to_string(),
                        mtype: None,
                    }),
                }),
            ],
        };

        let hir = parser.parse(&program);
        let hir = hir.expect("parse error");

        assert_eq!(&hir.id, &hir::HirId::new(0));

        let integer_a = hir::Expr {
            id: hir::HirId::new(5),
            kind: hir::ExprKind::Integer(&hir::Integer { value: 1 }),
        };
        let integer_b = hir::Expr {
            id: hir::HirId::new(9),
            kind: hir::ExprKind::Integer(&hir::Integer { value: 2 }),
        };
        assert_eq!(
            &hir.statements[0],
            &hir::Stmt {
                id: hir::HirId::new(1),
                kind: hir::StmtKind::ExprStmt(&hir::ExprStmt {
                    expr: &hir::Expr {
                        id: hir::HirId::new(2),
                        kind: hir::ExprKind::If(&hir::If {
                            condition: &hir::Expr {
                                id: hir::HirId::new(3),
                                kind: hir::ExprKind::Boolean(&hir::Boolean { value: true }),
                            },
                            then_branch: &hir::Block {
                                statements: &[
                                    hir::Stmt {
                                        id: hir::HirId::new(4),
                                        kind: hir::StmtKind::Let(&hir::Let {
                                            name: &hir::Symbol("x".to_string()),
                                            value: &integer_a
                                        }),
                                    },
                                    hir::Stmt {
                                        id: hir::HirId::new(6),
                                        kind: hir::StmtKind::ExprStmt(&hir::ExprStmt {
                                            expr: &hir::Expr {
                                                id: hir::HirId::new(7),
                                                kind: hir::ExprKind::Identifier(&hir::Identifier {
                                                    name: &hir::Symbol("x".to_string()),
                                                    resolved_expr: Some(&integer_a)
                                                }),
                                            }
                                        })
                                    },
                                ],
                            },
                            else_branch: Some(&hir::Block {
                                statements: &[
                                    hir::Stmt {
                                        id: hir::HirId::new(8),
                                        kind: hir::StmtKind::Let(&hir::Let {
                                            name: &hir::Symbol("y".to_string()),
                                            value: &integer_b
                                        }),
                                    },
                                    hir::Stmt {
                                        id: hir::HirId::new(10),
                                        kind: hir::StmtKind::ExprStmt(&hir::ExprStmt {
                                            expr: &hir::Expr {
                                                id: hir::HirId::new(11),
                                                kind: hir::ExprKind::Identifier(&hir::Identifier {
                                                    name: &hir::Symbol("y".to_string()),
                                                    resolved_expr: Some(&integer_b)
                                                }),
                                            }
                                        }),
                                    },
                                    hir::Stmt {
                                        id: hir::HirId::new(12),
                                        kind: hir::StmtKind::ExprStmt(&hir::ExprStmt {
                                            expr: &hir::Expr {
                                                id: hir::HirId::new(13),
                                                kind: hir::ExprKind::Identifier(&hir::Identifier {
                                                    name: &hir::Symbol("x".to_string()),
                                                    resolved_expr: None,
                                                }),
                                            }
                                        })
                                    },
                                ]
                            }),
                        }),
                    }
                })
            }
        );
        assert_eq!(
            &hir.statements[1],
            &hir::Stmt {
                id: hir::HirId::new(14),
                kind: hir::StmtKind::ExprStmt(&hir::ExprStmt {
                    expr: &hir::Expr {
                        id: hir::HirId::new(15),
                        kind: hir::ExprKind::Identifier(&hir::Identifier {
                            name: &hir::Symbol("x".to_string()),
                            resolved_expr: None,
                        }),
                    }
                })
            }
        );
        assert_eq!(
            &hir.statements[2],
            &hir::Stmt {
                id: hir::HirId::new(16),
                kind: hir::StmtKind::ExprStmt(&hir::ExprStmt {
                    expr: &hir::Expr {
                        id: hir::HirId::new(17),
                        kind: hir::ExprKind::Identifier(&hir::Identifier {
                            name: &hir::Symbol("y".to_string()),
                            resolved_expr: None,
                        }),
                    }
                })
            }
        );
    }

    #[test]
    fn test_parse_identifier() {
        let mut context = Context::new();
        let mut parser = HirParser::new(&context.arena, &mut context.scope, &mut context.resolver);
        let identifier = ast::Identifier {
            value: "test".to_string(),
            mtype: None,
        };
        let hir_identifier = parser.parse_identifier(&identifier);
        assert_eq!(hir_identifier.name, &hir::Symbol("test".to_string()));
    }

    #[test]
    fn test_parse_identifier_with_resolving() {
        let mut context = Context::new();
        let symbol = hir::Symbol("test".to_string());
        let string = hir::StringLit {
            value: "test string value".to_string(),
        };
        let expr = hir::Expr {
            id: hir::HirId::new(0),
            kind: hir::ExprKind::StringLit(&string),
        };
        context.scope.define_into_current_scope(&symbol, &expr);

        let mut parser = HirParser::new(&context.arena, &mut context.scope, &mut context.resolver);
        let identifier = ast::Identifier {
            value: "test".to_string(),
            mtype: None,
        };

        let hir_identifier = parser.parse_identifier(&identifier);

        assert_eq!(
            hir_identifier,
            &hir::Identifier {
                name: &symbol,
                resolved_expr: Some(&expr),
            }
        );
    }

    #[test]
    fn test_parse_string_lit() {
        let mut context = Context::new();
        let mut parser = HirParser::new(&context.arena, &mut context.scope, &mut context.resolver);
        let string = ast::StringLit {
            value: "test".to_string(),
        };
        let hir_string = parser.parse_string_lit(&string);
        assert_eq!(
            hir_string,
            &hir::StringLit {
                value: "test".to_string(),
            },
        );
    }

    #[test]
    fn test_parse_char() {
        let mut context = Context::new();
        let mut parser = HirParser::new(&context.arena, &mut context.scope, &mut context.resolver);
        let char_lit = ast::Char {
            value: "a".to_string(),
        };
        let hir_char = parser.parse_char(&char_lit);
        assert_eq!(
            hir_char,
            &hir::Char {
                value: "a".to_string(),
            },
        );
    }

    #[test]
    fn test_parse_call() {
        let mut context = Context::new();
        let mut parser = HirParser::new(&context.arena, &mut context.scope, &mut context.resolver);
        let call = ast::Call {
            func: ast::Expr::Identifier(ast::Identifier {
                value: "test".to_string(),
                mtype: None,
            })
            .into(),
            args: vec![ast::Expr::Identifier(ast::Identifier {
                value: "argA".to_string(),
                mtype: None,
            })],
        };
        let hir_call = parser.parse_call(&call);
        let func_symbol = hir::Symbol("test".to_string());
        let arg_symbol = hir::Symbol("argA".to_string());
        assert_eq!(
            hir_call,
            &hir::Call {
                func: &hir::Expr {
                    id: hir::HirId::new(0),
                    kind: hir::ExprKind::Identifier(&hir::Identifier {
                        name: &func_symbol,
                        resolved_expr: None,
                    }),
                },
                args: &[hir::Expr {
                    id: hir::HirId::new(1),
                    kind: hir::ExprKind::Identifier(&hir::Identifier {
                        name: &arg_symbol,
                        resolved_expr: None,
                    }),
                }],
            },
        );
    }

    #[test]
    fn test_parse_function() {
        let mut context = Context::new();
        let mut parser = HirParser::new(&context.arena, &mut context.scope, &mut context.resolver);
        let function = ast::Function {
            name: "test".to_string(),
            params: vec![ast::Identifier {
                value: "argA".to_string(),
                mtype: None,
            }],
            body: ast::Block {
                statements: vec![ast::Stmt::Return(ast::Return {
                    return_value: ast::Expr::Integer(ast::Integer { value: 1 }),
                })],
                tokens: Default::default(),
            },
            fn_type: ast::FunctionType::Function,
        };

        let hir_function = parser.parse_function(&function);

        assert_eq!(
            hir_function,
            &hir::Function {
                name: &hir::Symbol("test".to_string()),
                params: &[hir::FunctionParam {
                    name: &hir::Symbol("argA".to_string()),
                }],
                body: &hir::Block {
                    statements: &[hir::Stmt {
                        id: hir::HirId::new(1),
                        kind: hir::StmtKind::Return(&hir::Return {
                            expr: &hir::Expr {
                                id: hir::HirId::new(2),
                                kind: hir::ExprKind::Integer(&hir::Integer { value: 1 }),
                            },
                        }),
                    }],
                },
                fn_type: hir::FunctionType::Function,
            }
        );
    }

    #[test]
    fn test_access_function_arg() {
        let mut context = Context::new();
        let mut parser = HirParser::new(&context.arena, &mut context.scope, &mut context.resolver);
        let function = ast::Function {
            name: "test".to_string(),
            params: vec![ast::Identifier {
                value: "argA".to_string(),
                mtype: None,
            }],
            body: ast::Block {
                statements: vec![ast::Stmt::Return(ast::Return {
                    return_value: ast::Expr::Identifier(ast::Identifier {
                        value: "argA".to_string(),
                        mtype: None,
                    }),
                })],
                tokens: Default::default(),
            },
            fn_type: ast::FunctionType::Function,
        };

        let hir_function = parser.parse_function(&function);

        assert_eq!(
            hir_function,
            &hir::Function {
                name: &hir::Symbol("test".to_string()),
                params: &[hir::FunctionParam {
                    name: &hir::Symbol("argA".to_string()),
                }],
                body: &hir::Block {
                    statements: &[hir::Stmt {
                        id: hir::HirId::new(1),
                        kind: hir::StmtKind::Return(&hir::Return {
                            expr: &hir::Expr {
                                id: hir::HirId::new(2),
                                kind: hir::ExprKind::Identifier(&hir::Identifier {
                                    name: &hir::Symbol("argA".to_string()),
                                    resolved_expr: Some(&hir::Expr {
                                        id: hir::HirId::new(0),
                                        kind: hir::ExprKind::FunctionParam(&hir::FunctionParam {
                                            name: &hir::Symbol("argA".to_string()),
                                        }),
                                    },),
                                }),
                            },
                        }),
                    }],
                },
                fn_type: hir::FunctionType::Function,
            }
        );
    }

    #[test]
    fn test_function_with_entered_scope() {
        let mut context = Context::new();
        let mut parser = HirParser::new(&context.arena, &mut context.scope, &mut context.resolver);
        let function = ast::Function {
            params: vec![ast::Identifier {
                value: "outer_arg_a".to_string(),
                mtype: None,
            }],
            fn_type: ast::FunctionType::Function,
            name: "outer_function".to_string(),
            body: ast::Block {
                statements: vec![
                    ast::Stmt::ExprStmt(ast::ExprStmt {
                        expr: ast::Expr::Function(ast::Function {
                            name: "inner_function".to_string(),
                            params: vec![ast::Identifier {
                                value: "inner_arg_a".to_string(),
                                mtype: None,
                            }],
                            body: ast::Block {
                                statements: vec![
                                    ast::Stmt::Let(ast::Let {
                                        name: ast::Identifier {
                                            value: "x".to_string(),
                                            mtype: None,
                                        },
                                        value: ast::Expr::Integer(ast::Integer { value: 1 }),
                                    }),
                                    ast::Stmt::ExprStmt(ast::ExprStmt {
                                        expr: ast::Expr::Identifier(ast::Identifier {
                                            value: "x".to_string(),
                                            mtype: None,
                                        }),
                                    }),
                                ],
                                tokens: Default::default(),
                            },
                            fn_type: ast::FunctionType::Function,
                        }),
                    }),
                    ast::Stmt::ExprStmt(ast::ExprStmt {
                        expr: ast::Expr::Identifier(ast::Identifier {
                            value: "x".to_string(),
                            mtype: None,
                        }),
                    }),
                ],
                tokens: Default::default(),
            },
        };

        let hir_function = parser.parse_function(&function);

        assert_eq!(
            hir_function,
            &hir::Function {
                name: &hir::Symbol("outer_function".to_string()),
                params: &[hir::FunctionParam {
                    name: &hir::Symbol("outer_arg_a".to_string()),
                }],
                body: &hir::Block {
                    statements: &[
                        hir::Stmt {
                            id: hir::HirId::new(1),
                            kind: hir::StmtKind::ExprStmt(&hir::ExprStmt {
                                expr: &hir::Expr {
                                    id: hir::HirId::new(2),
                                    kind: hir::ExprKind::Function(&hir::Function {
                                        name: &hir::Symbol("inner_function".to_string()),
                                        params: &[hir::FunctionParam {
                                            name: &hir::Symbol("inner_arg_a".to_string())
                                        }],
                                        body: &hir::Block {
                                            statements: &[
                                                hir::Stmt {
                                                    id: hir::HirId::new(4),
                                                    kind: hir::StmtKind::Let(&hir::Let {
                                                        name: &hir::Symbol("x".to_string()),
                                                        value: &hir::Expr {
                                                            id: hir::HirId::new(5),
                                                            kind: hir::ExprKind::Integer(
                                                                &hir::Integer { value: 1 }
                                                            ),

                                                        },
                                                    }),
                                                },
                                                hir::Stmt {
                                                    id: hir::HirId::new(6),
                                                    kind: hir::StmtKind::ExprStmt(&hir::ExprStmt {
                                                        expr: &hir::Expr {
                                                            id: hir::HirId::new(7),
                                                            kind: hir::ExprKind::Identifier(
                                                                &hir::Identifier {
                                                                    name: &hir::Symbol(
                                                                        "x".to_string()
                                                                    ),
                                                                    resolved_expr: Some(&hir::Expr {
                                                                        id: hir::HirId::new(5),
                                                                        kind: hir::ExprKind::Integer(
                                                                            &hir::Integer { value: 1 }
                                                                        ),

                                                                    }),
                                                                }
                                                            ),

                                                        },
                                                    }),
                                                },
                                            ],
                                        },
                                        fn_type: hir::FunctionType::Function,
                                    }),

                                },
                            }),
                        },
                        hir::Stmt {
                            id: hir::HirId::new(8),
                            kind: hir::StmtKind::ExprStmt(&hir::ExprStmt {
                                expr: &hir::Expr {
                                    id: hir::HirId::new(9),
                                    kind: hir::ExprKind::Identifier(&hir::Identifier {
                                        name: &hir::Symbol("x".to_string()),
                                        resolved_expr: None,
                                    }),

                                },
                            }),
                        },
                    ],
                },
                fn_type: hir::FunctionType::Function,
            }
        );
    }

    #[test]
    fn test_parse_hash() {
        let mut context = Context::new();
        let mut parser = HirParser::new(&context.arena, &mut context.scope, &mut context.resolver);
        let hash = ast::Hash {
            pairs: vec![ast::Pair {
                key: ast::Expr::Identifier(ast::Identifier {
                    value: "key".to_string(),
                    mtype: None,
                }),
                value: ast::Expr::Identifier(ast::Identifier {
                    value: "value".to_string(),
                    mtype: None,
                }),
            }],
        };
        let hir_hash = parser.parse_hash(&hash);
        let key_symbol = hir::Symbol("key".to_string());
        let value_symbol = hir::Symbol("value".to_string());
        assert_eq!(
            hir_hash,
            &hir::Hash {
                pairs: &[hir::HashPair {
                    key: &hir::Expr {
                        id: hir::HirId::new(0),
                        kind: hir::ExprKind::Identifier(&hir::Identifier {
                            name: &key_symbol,
                            resolved_expr: None,
                        }),
                    },
                    value: &hir::Expr {
                        id: hir::HirId::new(1),
                        kind: hir::ExprKind::Identifier(&hir::Identifier {
                            name: &value_symbol,
                            resolved_expr: None,
                        }),
                    },
                }],
            },
        );
    }

    #[test]
    fn test_parse_if() {
        let mut context = Context::new();
        let mut parser = HirParser::new(&context.arena, &mut context.scope, &mut context.resolver);
        let if_stmt = ast::If {
            cond: ast::Expr::Identifier(ast::Identifier {
                value: "test".to_string(),
                mtype: None,
            })
            .into(),
            consequence: ast::Block {
                statements: vec![ast::Stmt::Return(ast::Return {
                    return_value: ast::Expr::Integer(ast::Integer { value: 1 }),
                })],
                tokens: Default::default(),
            },
            alternative: Some(ast::Block {
                statements: vec![ast::Stmt::Return(ast::Return {
                    return_value: ast::Expr::Integer(ast::Integer { value: 2 }),
                })],
                tokens: Default::default(),
            }),
            r#type: None,
        };

        let hir_if = parser.parse_if(&if_stmt);

        let test_symbol = hir::Symbol("test".to_string());
        assert_eq!(
            hir_if,
            &hir::If {
                condition: &hir::Expr {
                    id: hir::HirId::new(0),
                    kind: hir::ExprKind::Identifier(&hir::Identifier {
                        name: &test_symbol,
                        resolved_expr: None,
                    }),
                },
                then_branch: &hir::Block {
                    statements: &[hir::Stmt {
                        id: hir::HirId::new(1),
                        kind: hir::StmtKind::Return(&hir::Return {
                            expr: &hir::Expr {
                                id: hir::HirId::new(2),
                                kind: hir::ExprKind::Integer(&hir::Integer { value: 1 }),
                            },
                        }),
                    }],
                },
                else_branch: Some(&hir::Block {
                    statements: &[hir::Stmt {
                        id: hir::HirId::new(3),
                        kind: hir::StmtKind::Return(&hir::Return {
                            expr: &hir::Expr {
                                id: hir::HirId::new(4),
                                kind: hir::ExprKind::Integer(&hir::Integer { value: 2 }),
                            },
                        }),
                    }],
                }),
            },
        );
    }

    #[test]
    fn test_parse_index() {
        let mut context = Context::new();
        let mut parser = HirParser::new(&context.arena, &mut context.scope, &mut context.resolver);
        let index = ast::Index {
            left: ast::Expr::Identifier(ast::Identifier {
                value: "test".to_string(),
                mtype: None,
            })
            .into(),
            index: ast::Expr::Integer(ast::Integer { value: 1 }).into(),
        };

        let hir_index = parser.parse_index(&index);

        assert_eq!(
            hir_index,
            &hir::Index {
                target: &hir::Expr {
                    id: hir::HirId::new(0),
                    kind: hir::ExprKind::Identifier(&hir::Identifier {
                        name: &hir::Symbol("test".to_string()),
                        resolved_expr: None,
                    }),
                },
                index: &hir::Expr {
                    id: hir::HirId::new(1),
                    kind: hir::ExprKind::Integer(&hir::Integer { value: 1 }),
                },
            }
        );
    }

    #[test]
    fn test_parse_infix_expr() {
        let mut context = Context::new();
        let mut parser = HirParser::new(&context.arena, &mut context.scope, &mut context.resolver);
        let infix_expr = ast::InfixExpr {
            left: ast::Expr::Identifier(ast::Identifier {
                value: "test".to_string(),
                mtype: None,
            })
            .into(),
            ope: ast::Operator::Plus,
            right: ast::Expr::Integer(ast::Integer { value: 1 }).into(),
        };

        let hir_infix_expr = parser.parse_infix_expr(&infix_expr);

        assert_eq!(
            hir_infix_expr,
            &hir::InfixExpr {
                left: &hir::Expr {
                    id: hir::HirId::new(0),
                    kind: hir::ExprKind::Identifier(&hir::Identifier {
                        name: &hir::Symbol("test".to_string()),
                        resolved_expr: None,
                    }),
                },
                operator: hir::Operator::Plus,
                right: &hir::Expr {
                    id: hir::HirId::new(1),
                    kind: hir::ExprKind::Integer(&hir::Integer { value: 1 }),
                },
            }
        );
    }

    #[test]
    fn test_parse_expr() {
        let mut context = Context::new();
        let mut parser = HirParser::new(&context.arena, &mut context.scope, &mut context.resolver);
        let expr = ast::Expr::StringLit(ast::StringLit {
            value: "test".to_string(),
        });
        let hir_expr = parser.parse_expr(&expr);
        assert_eq!(
            hir_expr,
            &hir::Expr {
                id: hir::HirId::new(0),
                kind: hir::ExprKind::StringLit(&hir::StringLit {
                    value: "test".to_string(),
                }),
            },
        );
    }

    #[test]
    fn test_parse_prefix_expr() {
        let mut context = Context::new();
        let mut parser = HirParser::new(&context.arena, &mut context.scope, &mut context.resolver);
        let prefix_expr = ast::PrefixExpr {
            ope: ast::Operator::Minus,
            right: ast::Expr::Integer(ast::Integer { value: 1 }).into(),
        };
        let hir_prefix_expr = parser.parse_prefix_expr(&prefix_expr);
        assert_eq!(
            hir_prefix_expr,
            &hir::PrefixExpr {
                operator: hir::Operator::Minus,
                target: &hir::Expr {
                    id: hir::HirId::new(0),
                    kind: hir::ExprKind::Integer(&hir::Integer { value: 1 }),
                },
            },
        );
    }

    #[test]
    fn test_parse_exprs() {
        let mut context = Context::new();
        let mut parser = HirParser::new(&context.arena, &mut context.scope, &mut context.resolver);
        let exprs = vec![
            ast::Expr::StringLit(ast::StringLit {
                value: "test".to_string(),
            }),
            ast::Expr::StringLit(ast::StringLit {
                value: "test2".to_string(),
            }),
        ];
        let hir_exprs = parser.parse_exprs(&exprs);
        assert_eq!(
            hir_exprs,
            &[
                hir::Expr {
                    id: hir::HirId::new(0),
                    kind: hir::ExprKind::StringLit(&hir::StringLit {
                        value: "test".to_string(),
                    }),
                },
                hir::Expr {
                    id: hir::HirId::new(1),
                    kind: hir::ExprKind::StringLit(&hir::StringLit {
                        value: "test2".to_string(),
                    }),
                },
            ],
        );
    }

    #[test]
    fn test_parse_expr_stmt() {
        let mut context = Context::new();
        let mut parser = HirParser::new(&context.arena, &mut context.scope, &mut context.resolver);
        let expr_stmt = ast::ExprStmt {
            expr: ast::Expr::StringLit(ast::StringLit {
                value: "test".to_string(),
            }),
        };
        let hir_expr_stmt = parser.parse_expr_stmt(&expr_stmt);
        assert_eq!(
            hir_expr_stmt,
            &hir::ExprStmt {
                expr: &hir::Expr {
                    id: hir::HirId::new(0),
                    kind: hir::ExprKind::StringLit(&hir::StringLit {
                        value: "test".to_string(),
                    }),
                },
            },
        );
    }

    #[test]
    fn test_parse_let() {
        let mut context = Context::new();
        let mut parser = HirParser::new(&context.arena, &mut context.scope, &mut context.resolver);
        let let_stmt = ast::Let {
            name: ast::Identifier {
                value: "test".to_string(),
                mtype: None,
            },
            value: ast::Expr::StringLit(ast::StringLit {
                value: "test_value".to_string(),
            }),
        };
        let hir_let = parser.parse_let(&let_stmt);

        let symbol = hir::Symbol("test".to_string());
        let expected_string = hir::StringLit {
            value: "test_value".to_string(),
        };
        let expected_expr = hir::Expr {
            id: hir::HirId::new(0),
            kind: hir::ExprKind::StringLit(&expected_string),
        };

        assert_eq!(
            hir_let,
            &hir::Let {
                name: &symbol,
                value: &expected_expr,
            }
        );

        // scope should have been updated
        assert_eq!(parser.scope.resolve(&symbol), Some(&expected_expr));
    }

    #[test]
    fn test_parse_return() {
        let mut context = Context::new();
        let mut parser = HirParser::new(&context.arena, &mut context.scope, &mut context.resolver);
        let return_stmt = ast::Return {
            return_value: ast::Expr::StringLit(ast::StringLit {
                value: "test".to_string(),
            }),
        };
        let hir_return = parser.parse_return(&return_stmt);

        assert_eq!(
            hir_return,
            &hir::Return {
                expr: &hir::Expr {
                    id: hir::HirId::new(0),
                    kind: hir::ExprKind::StringLit(&hir::StringLit {
                        value: "test".to_string(),
                    }),
                },
            }
        );
    }

    #[test]
    fn test_parse_stmts() {
        let mut context = Context::new();
        let mut parser = HirParser::new(&context.arena, &mut context.scope, &mut context.resolver);
        let stmts = vec![
            ast::Stmt::ExprStmt(ast::ExprStmt {
                expr: ast::Expr::StringLit(ast::StringLit {
                    value: "test1".to_string(),
                }),
            }),
            ast::Stmt::ExprStmt(ast::ExprStmt {
                expr: ast::Expr::StringLit(ast::StringLit {
                    value: "test2".to_string(),
                }),
            }),
        ];
        let hir_stmts = parser.parse_stmts(&stmts);
        assert_eq!(hir_stmts.len(), 2);

        let first_stmt = &hir_stmts[0];
        assert_eq!(
            first_stmt,
            &hir::Stmt {
                id: hir::HirId::new(0),
                kind: hir::StmtKind::ExprStmt(&hir::ExprStmt {
                    expr: &hir::Expr {
                        id: hir::HirId::new(1),
                        kind: hir::ExprKind::StringLit(&hir::StringLit {
                            value: "test1".to_string(),
                        }),
                    },
                }),
            }
        );

        let second_stmt = &hir_stmts[1];
        assert_eq!(
            second_stmt,
            &hir::Stmt {
                id: hir::HirId::new(2),
                kind: hir::StmtKind::ExprStmt(&hir::ExprStmt {
                    expr: &hir::Expr {
                        id: hir::HirId::new(3),
                        kind: hir::ExprKind::StringLit(&hir::StringLit {
                            value: "test2".to_string(),
                        }),
                    },
                }),
            }
        );
    }

    #[test]
    fn test_parse_block() {
        let mut context = Context::new();
        let mut parser = HirParser::new(&context.arena, &mut context.scope, &mut context.resolver);
        let block = ast::Block {
            statements: vec![ast::Stmt::ExprStmt(ast::ExprStmt {
                expr: ast::Expr::StringLit(ast::StringLit {
                    value: "test1".to_string(),
                }),
            })],
            tokens: Default::default(),
        };
        let hir_block = parser.parse_block(&block);
        assert_eq!(
            hir_block,
            &hir::Block {
                statements: &[hir::Stmt {
                    id: hir::HirId::new(0),
                    kind: hir::StmtKind::ExprStmt(&hir::ExprStmt {
                        expr: &hir::Expr {
                            id: hir::HirId::new(1),
                            kind: hir::ExprKind::StringLit(&hir::StringLit {
                                value: "test1".to_string(),
                            }),
                        },
                    }),
                },],
            }
        );
    }
}
