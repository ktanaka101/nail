use std::collections::HashMap;
use std::convert::TryInto;

use anyhow::Result;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use inkwell::module::Module;
use inkwell::values::{BasicValueEnum, FunctionValue, PointerValue};

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

    pub fn compile(&mut self, node: &ast::Node, output_ir: bool) -> Result<JitFunction<MainFunc>> {
        let fn_type = self.context.void_type().fn_type(&[], false);
        let main_fn = self.module.add_function("main", fn_type, None);
        let basic_block = self.context.append_basic_block(main_fn, "entry");

        self.builder.position_at_end(basic_block);

        match self.compile_node(node)? {
            Some(ref v) => self.builder.build_return(Some(v)),
            None => self.builder.build_return(None),
        };

        if output_ir {
            self.module.print_to_stderr();
        }

        Ok(unsafe { self.execution_engine.get_function("main")? })
    }

    fn compile_node(&mut self, node: &ast::Node) -> Result<Option<BasicValueEnum<'ctx>>> {
        Ok(match node {
            ast::Node::Expr(expr) => Some(self.compile_expr(expr)?),
            ast::Node::Stmt(stmt) => Some(self.compile_stmt(stmt)?),
            ast::Node::Program(pg) => {
                let mut last: Option<BasicValueEnum> = None;
                pg.statements.iter().try_for_each::<_, Result<()>>(|stmt| {
                    last = Some(self.compile_stmt(stmt)?);
                    Ok(())
                })?;

                last
            }
        })
    }

    fn compile_stmt(&mut self, stmt: &ast::Stmt) -> Result<BasicValueEnum<'ctx>> {
        Ok(match stmt {
            ast::Stmt::ExprStmt(expr_stmt) => self.compile_expr(&expr_stmt.expr)?,
            ast::Stmt::Let(l) => {
                let i64type = self.context.i64_type();
                let alloca = self.builder.build_alloca(i64type, &l.name.value);

                let value = self.compile_expr(&l.value)?;
                self.builder.build_store(alloca, value);

                self.variables.insert(l.name.value.clone(), alloca);

                value
            }
            _ => unimplemented!(),
        })
    }

    fn compile_expr(&mut self, expr: &ast::Expr) -> Result<BasicValueEnum<'ctx>> {
        Ok(match expr {
            ast::Expr::Integer(int) => self
                .context
                .i64_type()
                .const_int(int.value.try_into().unwrap(), false)
                .into(),
            ast::Expr::Identifier(id) => {
                let name = id.value.as_str();
                let id = self
                    .variables
                    .get(name)
                    .ok_or_else(|| anyhow::format_err!("undefined '{}'", name))?;

                self.builder.build_load(*id, name).into_int_value().into()
            }
            ast::Expr::InfixExpr(infix_expr) => {
                let lvalue = self.compile_expr(&*infix_expr.left)?;
                let rvalue = self.compile_expr(&*infix_expr.right)?;
                if !(lvalue.is_int_value() && rvalue.is_int_value()) {
                    Err(anyhow::format_err!(
                        "expect Integer {} Integer. received {:?} {} {:?}",
                        infix_expr.ope,
                        lvalue.get_type(),
                        infix_expr.ope,
                        rvalue.get_type()
                    ))?;
                }

                let lvalue = lvalue.into_int_value();
                let rvalue = rvalue.into_int_value();
                match infix_expr.ope {
                    ast::Operator::Plus => {
                        self.builder.build_int_add(lvalue, rvalue, "tmpadd").into()
                    }
                    ast::Operator::Minus => {
                        self.builder.build_int_sub(lvalue, rvalue, "tmpsub").into()
                    }
                    ast::Operator::Asterisk => {
                        self.builder.build_int_mul(lvalue, rvalue, "tmpmul").into()
                    }
                    ast::Operator::Slash => self
                        .builder
                        .build_int_signed_div(lvalue, rvalue, "tmpdiv")
                        .into(),
                    ast::Operator::Equal => self
                        .builder
                        .build_int_compare(inkwell::IntPredicate::EQ, lvalue, rvalue, "tmpeq")
                        .const_unsigned_to_float(self.context.f64_type())
                        .const_to_signed_int(self.context.i64_type())
                        .into(),
                    ast::Operator::Gt => self
                        .builder
                        .build_int_compare(inkwell::IntPredicate::SGT, lvalue, rvalue, "tmpgt")
                        .const_unsigned_to_float(self.context.f64_type())
                        .const_to_signed_int(self.context.i64_type())
                        .into(),
                    ast::Operator::Lt => self
                        .builder
                        .build_int_compare(inkwell::IntPredicate::SLT, lvalue, rvalue, "tmplt")
                        .const_unsigned_to_float(self.context.f64_type())
                        .const_to_signed_int(self.context.i64_type())
                        .into(),
                    _ => unimplemented!(),
                }
            }
            ast::Expr::PrefixExpr(prefix_expr) => match &prefix_expr.ope {
                ast::Operator::Bang => {
                    let expr = self.compile_expr(&*prefix_expr.right)?;
                    if !expr.is_int_value() {
                        Err(anyhow::format_err!(
                            "expect Integer. received {:?}",
                            expr.get_type()
                        ))?;
                    }

                    let int = expr.into_int_value();
                    let zero = self
                        .context
                        .i64_type()
                        .const_int(0.try_into().unwrap(), false)
                        .into();

                    self.builder
                        .build_int_compare(inkwell::IntPredicate::EQ, zero, int, "tmpnot")
                        .const_unsigned_to_float(self.context.f64_type())
                        .const_to_signed_int(self.context.i64_type())
                        .into()
                }
                ast::Operator::Minus => {
                    let expr = self.compile_expr(&*prefix_expr.right)?;
                    if !expr.is_int_value() {
                        Err(anyhow::format_err!(
                            "expect Integer. received {:?}",
                            expr.get_type()
                        ))?;
                    }
                    let int = expr.into_int_value();
                    self.builder.build_int_neg(int, "tmpneg").into()
                }
                unknown => Err(anyhow::format_err!("unknown operator {}", unknown))?,
            },
            _ => unimplemented!(),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer;
    use crate::parser;

    #[test]
    fn test_number_literal() {
        let tests = vec![("10", 10), ("20", 20), ("-10", -10), ("-20", -20)];
        run_llvm_tests(tests);
    }

    #[test]
    fn test_prefix_minus() {
        let tests = vec![("-10", -10), ("let a = 10; -a", -10), ("-(10 + 20)", -30)];
        run_llvm_tests(tests);
    }

    #[test]
    fn test_number_formula() {
        let tests = vec![
            ("5 + 5 + 5 + 5 - 10", 10),
            ("2 * 2 * 2 * 2 * 2", 32),
            ("-50 + 100 + -50", 0),
            ("5 * 2 + 10", 20),
            ("5 + 2 * 10", 25),
            ("20 + 2 * -10", 0),
            ("50 / 2 * 2 + 10", 60),
            ("2 * (5 + 10)", 30),
            ("3 * 3 * 3 + 10", 37),
            ("3 * (3 * 3) + 10", 37),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
        ];
        run_llvm_tests(tests);
    }

    #[test]
    fn test_let_binding() {
        let tests = vec![
            ("let a = 10", 10),
            ("let a = 10; a", 10),
            ("let a = 10; a + 5", 15),
            ("let a = 10; let b = 20; a + b", 30),
        ];
        run_llvm_tests(tests);
    }

    #[test]
    fn test_compare() {
        let tests = vec![
            ("0 > 0", 0),
            ("0 < 0", 0),
            ("0 > 1", 0),
            ("0 < 1", 1),
            ("0 == 0", 1),
            ("0 == 1", 0),
            ("let a = 10; a > 0", 1),
            ("let a = 10; a < 0", 0),
            ("let a = 10; (a < 10) < 10", 1),
            ("let a = 10; (a > 10) > 10", 0),
            ("let a = 10; (a == 10) == 10", 0),
            ("let a = 10; (a == a) == 1", 1),
        ];
        run_llvm_tests(tests);
    }

    #[test]
    fn test_bang() {
        let tests = vec![
            ("!0", 1),
            ("!1", 0),
            ("!10", 0),
            ("!!0", 0),
            ("!!1", 1),
            ("!!10", 1),
            ("let a = 10; !a", 0),
            ("let a = 10; !!a", 1),
        ];
        run_llvm_tests(tests);
    }

    fn run_llvm_tests(tests: Vec<(&'static str, i64)>) {
        tests.into_iter().for_each(|(input, expected)| {
            let context = Context::create();
            let module = context.create_module("top");
            let builder = context.create_builder();
            let execution_engine = module
                .create_jit_execution_engine(inkwell::OptimizationLevel::None)
                .unwrap();
            let mut compiler = Compiler::new(&context, &module, &builder, &execution_engine);
            let lexer = lexer::Lexer::new(input.to_string());
            let mut parser = parser::Parser::new(lexer);

            let program = match parser.parse_program() {
                Ok(p) => p,
                Err(e) => panic!("Parser error: {} by {}", e, input),
            };

            let main_fn = match compiler.compile(&program.into(), false) {
                Ok(f) => f,
                Err(e) => panic!("LLVM error: {} by {}", e, input),
            };

            let result = unsafe { main_fn.call() };
            assert_eq!(result, expected)
        });
    }
}
