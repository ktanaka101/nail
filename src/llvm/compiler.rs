use std::collections::HashMap;
use std::convert::TryInto;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use inkwell::module::Module;
use inkwell::values;

use crate::parser::ast;

type MainFunc = unsafe extern "C" fn() -> i64;

pub struct Compiler<'a, 'ctx> {
    context: &'ctx Context,
    module: &'a Module<'ctx>,
    builder: &'a Builder<'ctx>,
    execution_engine: &'a ExecutionEngine<'ctx>,

    variables: HashMap<String, PointerValue<'ctx>>,
    fn_value_opt: Option<FunctionValue<'ctx>>,
}

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    pub fn new(
        context: &'ctx Context,
        module: &'a Module<'ctx>,
        builder: &'a Builder<'ctx>,
        execution_engine: &'a ExecutionEngine<'ctx>,
    ) -> Self {
        Compiler {
            context: &context,
            module,
            builder,
            execution_engine,
            variables: HashMap::new(),
            fn_value_opt: None,
        }
    }

    pub fn compile(&mut self, node: &ast::Node) -> Result<JitFunction<MainFunc>> {
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

    fn compile_node(&mut self, node: &ast::Node) -> Result<Option<IntValue<'ctx>>> {
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

    fn compile_stmt(&mut self, stmt: &ast::Stmt) -> Result<IntValue<'ctx>> {
        Ok(match stmt {
            ast::Stmt::ExprStmt(expr_stmt) => self.compile_expr(&expr_stmt.expr)?,
            _ => unimplemented!(),
        })
    }

    fn compile_expr(&mut self, expr: &ast::Expr) -> Result<IntValue<'ctx>> {
        Ok(match expr {
            ast::Expr::Integer(int) => self
                .context
                .i64_type()
                .const_int(int.value.try_into().unwrap(), false),
            ast::Expr::InfixExpr(infix_expr) => {
                let lvalue = self.compile_expr(&*infix_expr.left)?;
                let rvalue = self.compile_expr(&*infix_expr.right)?;
                match infix_expr.ope {
                    ast::Operator::Plus => self.builder.build_int_add(lvalue, rvalue, "tmpadd"),
                    ast::Operator::Minus => self.builder.build_int_sub(lvalue, rvalue, "tmpsub"),
                    ast::Operator::Asterisk => self.builder.build_int_mul(lvalue, rvalue, "tmpmul"),
                    ast::Operator::Slash => {
                        self.builder.build_int_signed_div(lvalue, rvalue, "tmpdiv")
                    }
                    _ => unimplemented!(),
                }
            }
            _ => unimplemented!(),
        })
    }
}
