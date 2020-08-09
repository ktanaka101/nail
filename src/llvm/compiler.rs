use anyhow::Result;
use std::convert::TryInto;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use inkwell::module::Module;
use inkwell::values;
use inkwell::OptimizationLevel;

use crate::parser::ast;

type MainFunc = unsafe extern "C" fn() -> i64;

pub struct Compiler<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    execution_engine: ExecutionEngine<'ctx>,
}

impl<'ctx> Compiler<'ctx> {
    pub fn new(context: &'ctx Context) -> Self {
        let module = context.create_module("top");
        let execution_engine = module
            .create_jit_execution_engine(OptimizationLevel::None)
            .unwrap();

        Compiler {
            context: &context,
            module,
            builder: context.create_builder(),
            execution_engine,
        }
    }

    pub fn compile(&self, node: &ast::Node) -> Result<JitFunction<MainFunc>> {
        let fn_type = self.context.void_type().fn_type(&[], false);
        let main_fn = self.module.add_function("main", fn_type, None);
        let basic_block = self.context.append_basic_block(main_fn, "entry");

        self.builder.position_at_end(basic_block);

        println!("{:?}", node);

        match self.compile_node(node)? {
            Some(v) => self.builder.build_return(Some(&v)),
            None => self.builder.build_return(None),
        };

        self.module.print_to_stderr();

        Ok(unsafe { self.execution_engine.get_function("main")? })
    }

    fn compile_node(&self, node: &ast::Node) -> Result<Option<values::IntValue>> {
        Ok(match node {
            ast::Node::Expr(expr) => Some(self.compile_expr(expr)?),
            ast::Node::Stmt(stmt) => Some(self.compile_stmt(stmt)?),
            ast::Node::Program(pg) => {
                let mut last: Option<values::IntValue> = None;
                pg.statements.iter().try_for_each::<_, Result<()>>(|stmt| {
                    last = Some(self.compile_stmt(stmt)?);
                    Ok(())
                })?;

                last
            }
        })
    }

    fn compile_stmt(&self, stmt: &ast::Stmt) -> Result<values::IntValue> {
        Ok(match stmt {
            ast::Stmt::ExprStmt(expr_stmt) => self.compile_expr(&expr_stmt.expr)?,
            _ => unimplemented!(),
        })
    }

    fn compile_expr(&self, expr: &ast::Expr) -> Result<values::IntValue> {
        Ok(match expr {
            ast::Expr::Integer(int) => self
                .context
                .i64_type()
                .const_int(int.value.try_into().unwrap(), false),
            ast::Expr::InfixExpr(infix_expr) => {
                let lvalue = self.compile_expr(&*infix_expr.left)?;
                let rvalue = self.compile_expr(&*infix_expr.right)?;
                match infix_expr.ope {
                    ast::Operator::Plus => self.builder.build_int_add(lvalue, rvalue, "sum"),
                    _ => unimplemented!(),
                }
            }
            _ => unimplemented!(),
        })
    }
}
