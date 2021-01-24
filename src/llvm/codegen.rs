use std::collections::HashMap;
use std::convert::{TryFrom, TryInto};
use std::ffi::CString;
use std::fs::File;
use std::io::prelude::*;

use anyhow::Result;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use inkwell::module::Module;
use inkwell::types::StructType;
use inkwell::values::{
    ArrayValue, BasicValueEnum, FloatValue, FunctionValue, IntValue, PointerValue, StructValue,
    VectorValue,
};
use inkwell::AddressSpace;

use crate::parser::ast;

type MainFunc = unsafe extern "C" fn() -> *mut i8;

pub enum Output {
    File { suffix: u32 },
    StdOut,
    CStringPtr,
}

enum PrimitiveType {
    Integer = 1,
    Char = 2,
    IntegerArray = 3,
    CharArray = 4,
    String = 5,
}

impl TryFrom<i8> for PrimitiveType {
    type Error = anyhow::Error;

    fn try_from(value: i8) -> Result<Self> {
        Ok(match value {
            1 => PrimitiveType::Integer,
            2 => PrimitiveType::Char,
            3 => PrimitiveType::IntegerArray,
            4 => PrimitiveType::CharArray,
            5 => PrimitiveType::String,
            other => {
                return Err(anyhow::format_err!(
                    "Expected 1, 2, 3, 4, 5. Received {}",
                    other
                ))
            }
        })
    }
}

impl Into<u64> for PrimitiveType {
    fn into(self) -> u64 {
        self as u64
    }
}

fn to_string(ptr: *const i64, length: i64, primitive_type: PrimitiveType) -> String {
    match primitive_type {
        PrimitiveType::Integer => {
            let ptr: *const i64 = ptr.cast();
            let int = unsafe { *ptr as i64 };

            int.to_string()
        }
        PrimitiveType::Char => {
            let ptr: *const char = ptr.cast();

            unsafe { *ptr as char }.to_string()
        }
        PrimitiveType::IntegerArray => {
            let length = usize::try_from(length).unwrap();
            let ptr: *const i64 = ptr.cast();
            let mut string = String::new();
            string.push('[');
            for i in 0..length {
                let int = unsafe { *ptr.add(i) as i64 };
                string.push_str(&int.to_string());
                if i != length - 1 {
                    string.push_str(", ")
                }
            }
            string.push(']');

            string
        }
        PrimitiveType::CharArray => {
            let length = usize::try_from(length).unwrap();
            let ptr: *const u8 = ptr.cast();
            let mut string = String::new();
            string.push('[');
            for i in 0..length {
                let c = unsafe { *ptr.add(i) as char };
                string.push(c);
                if i != length - 1 {
                    string.push_str(", ")
                }
            }
            string.push(']');

            string
        }
        // TODO: core library by nail
        PrimitiveType::String => {
            let length = usize::try_from(length).unwrap();
            let ptr: *const u8 = ptr.cast();
            let mut bytes: Vec<u8> = vec![];

            for i in 0..length {
                let v = unsafe { *ptr.add(i) as u8 };
                bytes.push(v);
            }

            String::from_utf8(bytes).unwrap()
        }
    }
}

#[no_mangle]
extern "C" fn puts(ptr: *const i64, length: i64, primitive_type: i8) {
    let s = to_string(ptr, length, primitive_type.try_into().unwrap());
    println!("{}", s);
}

#[no_mangle]
extern "C" fn to_file(ptr: *const i64, length: i64, primitive_type: i8, file_name_suffix: i64) {
    let s = to_string(ptr, length, primitive_type.try_into().unwrap());
    let mut file = File::create(format!("output/{}.output", file_name_suffix)).unwrap();
    file.write_all(s.as_bytes()).unwrap();
}

#[no_mangle]
extern "C" fn return_to_string(ptr: *const i64, length: i64, primitive_type: i8) -> *const i8 {
    let s = to_string(ptr, length, primitive_type.try_into().unwrap());
    let s = CString::new(s).unwrap();
    s.into_raw()
}

#[derive(thiserror::Error, Debug, Clone)]
pub enum Error {
    #[error("Expected type {0}. Received type {1}")]
    DifferentType(String, String),
    #[error("Undefined identifier `{0}`")]
    UndefinedIdentfier(String),
    #[error("Expected {1} {0} {1}. Received {2} {0} {2}")]
    DifferentInfixType(ast::Operator, String, String),
    #[error("Expected {}. Received {0}", "`-` or `!`")]
    UnknownInfixOperator(ast::Operator),
}

fn get_type_string(basic_value: &BasicValueEnum) -> String {
    match basic_value {
        BasicValueEnum::IntValue(_) => "IntValue",
        BasicValueEnum::FloatValue(_) => "FloatValue",
        BasicValueEnum::ArrayValue(_) => "ArrayValue",
        BasicValueEnum::PointerValue(_) => "PointerValue",
        BasicValueEnum::StructValue(_) => "StructValue",
        BasicValueEnum::VectorValue(_) => "VectorValue",
    }
    .to_string()
}

pub struct Codegen<'a, 'ctx> {
    context: &'ctx Context,
    module: &'a Module<'ctx>,
    builder: &'a Builder<'ctx>,
    execution_engine: &'a ExecutionEngine<'ctx>,

    variables: HashMap<String, PointerValue<'ctx>>,
    fn_value_opt: Option<FunctionValue<'ctx>>,
    builtin_functions: HashMap<String, FunctionValue<'ctx>>,
    _builtin_structs: HashMap<String, StructType<'ctx>>,
}

impl<'a, 'ctx> Codegen<'a, 'ctx> {
    pub fn new(
        context: &'ctx Context,
        module: &'a Module<'ctx>,
        builder: &'a Builder<'ctx>,
        execution_engine: &'a ExecutionEngine<'ctx>,
    ) -> Self {
        Codegen {
            context: &context,
            module,
            builder,
            execution_engine,
            variables: HashMap::new(),
            fn_value_opt: None,
            builtin_functions: HashMap::new(),
            _builtin_structs: HashMap::new(),
        }
    }

    #[inline]
    fn fn_value(&self) -> FunctionValue<'ctx> {
        self.fn_value_opt.unwrap()
    }

    fn add_builtin_function(&mut self) {
        {
            let fn_type = self.context.void_type().fn_type(
                &[
                    self.context
                        .i64_type()
                        .ptr_type(AddressSpace::Generic)
                        .into(),
                    self.context.i64_type().into(),
                    self.context.i8_type().into(),
                ],
                false,
            );
            let puts_fn = self.module.add_function("puts", fn_type, None);
            self.execution_engine
                .add_global_mapping(&puts_fn, puts as usize);
            self.builtin_functions.insert("puts".to_string(), puts_fn);
        }

        {
            let fn_type = self.context.void_type().fn_type(
                &[
                    self.context
                        .i64_type()
                        .ptr_type(AddressSpace::Generic)
                        .into(),
                    self.context.i64_type().into(),
                    self.context.i8_type().into(),
                    self.context.i64_type().into(),
                ],
                false,
            );
            let func_value = self.module.add_function("to_file", fn_type, None);
            self.execution_engine
                .add_global_mapping(&func_value, to_file as usize);
            self.builtin_functions
                .insert("to_file".to_string(), func_value);
        }

        {
            let return_ty = self.context.i8_type().ptr_type(AddressSpace::Generic);
            let fn_type = return_ty.fn_type(
                &[
                    self.context
                        .i64_type()
                        .ptr_type(AddressSpace::Generic)
                        .into(),
                    self.context.i64_type().into(),
                    self.context.i8_type().into(),
                ],
                false,
            );
            let func_value = self.module.add_function("return_to_string", fn_type, None);
            self.execution_engine
                .add_global_mapping(&func_value, return_to_string as usize);
            self.builtin_functions
                .insert("return_to_string".to_string(), func_value);
        }
    }

    fn add_builtin_struct(&mut self) {
        // {
        //     // NailResult
        //     let ptr_ty = self
        //         .context
        //         .i64_type()
        //         .ptr_type(AddressSpace::Generic);
        //     let length_ty = self.context.i64_type();
        //     let primitive_type_ty = self.context.i8_type();
        //     let struct_type = self.context.struct_type(
        //         &[ptr_ty.into(), length_ty.into(), primitive_type_ty.into()],
        //         false,
        //     );

        //     self.builtin_structs
        //         .insert("TestStruct".to_string(), struct_type);
        // }
    }

    pub fn compile(
        &mut self,
        node: &ast::Node,
        output_ir: bool,
        output: Output,
    ) -> Result<JitFunction<MainFunc>> {
        self.add_builtin_struct();
        self.add_builtin_function();

        let fn_type = self.context.void_type().fn_type(&[], false);
        let main_fn = self.module.add_function("main", fn_type, None);
        let basic_block = self.context.append_basic_block(main_fn, "entry");

        self.builder.position_at_end(basic_block);

        self.fn_value_opt = Some(main_fn);

        match self.compile_node(node)? {
            Some(res) => {
                let (ptr, length, primitive_type) = match res {
                    BasicValueEnum::IntValue(i) => {
                        let ptr = self.builder.build_alloca(i.get_type(), "alloca_i");
                        self.builder.build_store(ptr, i);

                        (
                            ptr,
                            self.context.i64_type().const_int(1, false),
                            PrimitiveType::Integer,
                        )
                    }
                    BasicValueEnum::VectorValue(vec) => {
                        let ptr = self.builder.build_alloca(vec.get_type(), "test");
                        self.builder.build_store(ptr, vec);

                        let ptr = unsafe {
                            self.builder.build_in_bounds_gep(
                                ptr,
                                &[
                                    self.context.i32_type().const_int(0, false),
                                    self.context.i32_type().const_int(0, false),
                                ],
                                "ptr",
                            )
                        }
                        .const_cast(self.context.i64_type().ptr_type(AddressSpace::Generic));

                        let arr_size = self
                            .context
                            .i64_type()
                            .const_int(vec.get_type().get_size().into(), false);

                        let primitive_type = if vec.is_const_string() {
                            PrimitiveType::String
                        } else {
                            PrimitiveType::IntegerArray
                        };

                        (ptr, arr_size, primitive_type)
                    }
                    BasicValueEnum::ArrayValue(arr) => {
                        let ptr = self.builder.build_alloca(arr.get_type(), "test");
                        self.builder.build_store(ptr, BasicValueEnum::from(arr));

                        let ptr = unsafe {
                            self.builder.build_in_bounds_gep(
                                ptr,
                                &[
                                    self.context.i32_type().const_int(0, false),
                                    self.context.i32_type().const_int(0, false),
                                ],
                                "ptr",
                            )
                        }
                        .const_cast(self.context.i64_type().ptr_type(AddressSpace::Generic));

                        let arr_size = self
                            .context
                            .i64_type()
                            .const_int(arr.get_type().len().into(), false);

                        (ptr, arr_size, PrimitiveType::IntegerArray)
                    }
                    _ => unimplemented!(),
                };

                match output {
                    Output::File { suffix } => {
                        let suffix = self.context.i64_type().const_int(suffix.into(), false);
                        self.builder.build_call(
                            #[allow(clippy::clone_on_copy)]
                            self.builtin_functions.get("to_file").unwrap().clone(),
                            &[
                                ptr.into(),
                                length.into(),
                                self.context
                                    .i8_type()
                                    .const_int(primitive_type.into(), false)
                                    .into(),
                                suffix.into(),
                            ],
                            "to_file",
                        );

                        let return_v = self
                            .context
                            .i8_type()
                            .ptr_type(AddressSpace::Generic)
                            .const_null();
                        self.builder.build_return(Some(&return_v));
                    }
                    Output::StdOut => {
                        self.builder.build_call(
                            self.builtin_functions.get("puts").unwrap().to_owned(),
                            &[
                                ptr.into(),
                                length.into(),
                                self.context
                                    .i8_type()
                                    .const_int(primitive_type.into(), false)
                                    .into(),
                            ],
                            "puts",
                        );

                        let return_v = self
                            .context
                            .i8_type()
                            .ptr_type(AddressSpace::Generic)
                            .const_null();
                        self.builder.build_return(Some(&return_v));
                    }
                    Output::CStringPtr => {
                        let call_v = self.builder.build_call(
                            #[allow(clippy::clone_on_copy)]
                            self.builtin_functions
                                .get("return_to_string")
                                .unwrap()
                                .clone(),
                            &[
                                ptr.into(),
                                length.into(),
                                self.context
                                    .i8_type()
                                    .const_int(primitive_type.into(), false)
                                    .into(),
                            ],
                            "return_to_string",
                        );
                        let return_v = call_v.try_as_basic_value().left().unwrap();

                        self.builder.build_return(Some(&return_v));
                    }
                };
            }
            None => {
                self.builder.build_return(None);
            }
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
                let value = self.compile_expr(&l.value)?;
                let alloca = self.builder.build_alloca(value.get_type(), "let_ptr");

                self.builder.build_store(alloca, value);

                self.variables.insert(l.name.value.clone(), alloca);

                value
            }
            ast::Stmt::Block(block) => {
                let mut last = self.context.i64_type().const_zero().into();
                block
                    .statements
                    .iter()
                    .try_for_each::<_, Result<()>>(|stmt| {
                        last = self.compile_stmt(stmt)?;
                        Ok(())
                    })?;

                last
            }
            _ => unimplemented!(),
        })
    }

    fn infix_int(
        &self,
        ope: &ast::Operator,
        lv: IntValue<'ctx>,
        rv: IntValue<'ctx>,
    ) -> IntValue<'ctx> {
        match ope {
            ast::Operator::Plus => self.builder.build_int_add(lv, rv, "tmpadd"),
            ast::Operator::Minus => self.builder.build_int_sub(lv, rv, "tmpsub"),
            ast::Operator::Asterisk => self.builder.build_int_mul(lv, rv, "tmpmul"),
            ast::Operator::Slash => self.builder.build_int_signed_div(lv, rv, "tmpdiv"),
            ast::Operator::Equal => self
                .builder
                .build_int_compare(inkwell::IntPredicate::EQ, lv, rv, "tmpeq")
                .const_unsigned_to_float(self.context.f64_type())
                .const_to_signed_int(self.context.i64_type()),
            ast::Operator::Gt => self
                .builder
                .build_int_compare(inkwell::IntPredicate::SGT, lv, rv, "tmpgt")
                .const_unsigned_to_float(self.context.f64_type())
                .const_to_signed_int(self.context.i64_type()),
            ast::Operator::Lt => self
                .builder
                .build_int_compare(inkwell::IntPredicate::SLT, lv, rv, "tmplt")
                .const_unsigned_to_float(self.context.f64_type())
                .const_to_signed_int(self.context.i64_type()),
            _ => unimplemented!(),
        }
    }

    fn infix_float(
        &self,
        _ope: &ast::Operator,
        _lv: FloatValue<'ctx>,
        _rv: FloatValue<'ctx>,
    ) -> FloatValue<'ctx> {
        unimplemented!()
    }

    fn infix_array(
        &self,
        _ope: &ast::Operator,
        _lv: ArrayValue<'ctx>,
        _rv: ArrayValue<'ctx>,
    ) -> ArrayValue<'ctx> {
        unimplemented!()
    }

    fn infix_vector(
        &self,
        _ope: &ast::Operator,
        _lv: VectorValue<'ctx>,
        _rv: VectorValue<'ctx>,
    ) -> VectorValue<'ctx> {
        unimplemented!()
    }

    fn infix_struct(
        &self,
        _ope: &ast::Operator,
        _lv: StructValue<'ctx>,
        _rv: StructValue<'ctx>,
    ) -> StructValue<'ctx> {
        unimplemented!()
    }

    fn infix_pointer(
        &self,
        _ope: &ast::Operator,
        _lv: PointerValue<'ctx>,
        _rv: PointerValue<'ctx>,
    ) -> PointerValue<'ctx> {
        unimplemented!()
    }

    fn compile_expr(&mut self, expr: &ast::Expr) -> Result<BasicValueEnum<'ctx>> {
        Ok(match expr {
            ast::Expr::Integer(int) => self
                .context
                .i64_type()
                .const_int(int.value.try_into().unwrap(), false)
                .into(),
            ast::Expr::StringLit(string) => self
                .context
                .const_string(string.value.as_bytes(), false)
                .into(),
            ast::Expr::Char(c) => self.context.const_string(c.value.as_bytes(), false).into(),
            ast::Expr::Identifier(id) => {
                let name = id.value.as_str();
                let id = self
                    .variables
                    .get(name)
                    .ok_or_else(|| Error::UndefinedIdentfier(name.to_string()))?;

                self.builder.build_load(*id, name)
            }
            ast::Expr::Array(arr) => {
                let size = arr.elements.len();
                let ty = self.context.i64_type().array_type(size.try_into()?);
                let size = self.context.i64_type().const_int(size.try_into()?, false);
                let arr_ptr = self.builder.build_array_alloca(ty, size, "array_alloca");
                let mut array = self.builder.build_load(arr_ptr, "array").into_array_value();

                for (i, element) in arr.elements.iter().enumerate() {
                    array = self
                        .builder
                        .build_insert_value(
                            array,
                            self.compile_expr(element)?,
                            i.try_into()?,
                            "array_insert_value",
                        )
                        .unwrap()
                        .into_array_value();
                }

                array.into()
            }
            ast::Expr::InfixExpr(infix_expr) => {
                let lvalue = self.compile_expr(&*infix_expr.left)?;
                let rvalue = self.compile_expr(&*infix_expr.right)?;

                match (lvalue, rvalue) {
                    (BasicValueEnum::IntValue(lv), BasicValueEnum::IntValue(rv)) => {
                        self.infix_int(&infix_expr.ope, lv, rv).into()
                    }
                    (BasicValueEnum::FloatValue(lv), BasicValueEnum::FloatValue(rv)) => {
                        self.infix_float(&infix_expr.ope, lv, rv).into()
                    }
                    (BasicValueEnum::ArrayValue(lv), BasicValueEnum::ArrayValue(rv)) => {
                        self.infix_array(&infix_expr.ope, lv, rv).into()
                    }
                    (BasicValueEnum::VectorValue(lv), BasicValueEnum::VectorValue(rv)) => {
                        self.infix_vector(&infix_expr.ope, lv, rv).into()
                    }
                    (BasicValueEnum::StructValue(lv), BasicValueEnum::StructValue(rv)) => {
                        self.infix_struct(&infix_expr.ope, lv, rv).into()
                    }
                    (BasicValueEnum::PointerValue(lv), BasicValueEnum::PointerValue(rv)) => {
                        self.infix_pointer(&infix_expr.ope, lv, rv).into()
                    }
                    (lvalue, rvalue) => {
                        return Err(Error::DifferentInfixType(
                            infix_expr.ope.clone(),
                            get_type_string(&lvalue),
                            get_type_string(&rvalue),
                        )
                        .into());
                    }
                }
            }
            ast::Expr::PrefixExpr(prefix_expr) => match &prefix_expr.ope {
                ast::Operator::Bang => {
                    let expr = self.compile_expr(&*prefix_expr.right)?;
                    if !expr.is_int_value() {
                        return Err(Error::DifferentType(
                            "IntValue".to_string(),
                            get_type_string(&expr),
                        )
                        .into());
                    }

                    let int = expr.into_int_value();
                    let zero = self.context.i64_type().const_zero();

                    self.builder
                        .build_int_compare(inkwell::IntPredicate::EQ, zero, int, "tmpnot")
                        .const_unsigned_to_float(self.context.f64_type())
                        .const_to_signed_int(self.context.i64_type())
                        .into()
                }
                ast::Operator::Minus => {
                    let expr = self.compile_expr(&*prefix_expr.right)?;
                    if !expr.is_int_value() {
                        return Err(Error::DifferentType(
                            "IntValue".to_string(),
                            get_type_string(&expr),
                        )
                        .into());
                    }
                    let int = expr.into_int_value();
                    self.builder.build_int_neg(int, "tmpneg").into()
                }
                unknown => return Err(Error::UnknownInfixOperator(unknown.clone()).into()),
            },
            ast::Expr::If(if_expr) => {
                let parent = self.fn_value();
                let zero = self.context.i64_type().const_zero();

                let cond = self.compile_expr(&*if_expr.cond)?;
                if !cond.is_int_value() {
                    return Err(Error::DifferentType(
                        "IntValue".to_string(),
                        get_type_string(&cond),
                    )
                    .into());
                }
                let cond = cond.into_int_value();
                let cond =
                    self.builder
                        .build_int_compare(inkwell::IntPredicate::NE, cond, zero, "ifcond");

                // build branch
                let then_bb = self.context.append_basic_block(parent, "then");
                let else_bb = self.context.append_basic_block(parent, "else");
                let cont_bb = self.context.append_basic_block(parent, "ifcont");

                self.builder
                    .build_conditional_branch(cond, then_bb, else_bb);

                let ast::If {
                    consequence,
                    alternative,
                    ..
                } = if_expr;

                // build then block
                self.builder.position_at_end(then_bb);
                let then_val = self.compile_stmt(&consequence.to_owned().into())?;
                self.builder.build_unconditional_branch(cont_bb);

                let then_bb = self.builder.get_insert_block().unwrap();

                // build else block
                self.builder.position_at_end(else_bb);
                let else_val = if let Some(alternative) = alternative {
                    self.compile_stmt(&alternative.clone().into())?
                } else {
                    zero.into()
                };
                self.builder.build_unconditional_branch(cont_bb);

                let else_bb = self.builder.get_insert_block().unwrap();

                // emit merge block
                self.builder.position_at_end(cont_bb);

                let phi = self.builder.build_phi(self.context.i64_type(), "iftmp");

                phi.add_incoming(&[(&then_val, then_bb), (&else_val, else_bb)]);

                phi.as_basic_value()
            }
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
        let tests = vec![("10", "10"), ("20", "20"), ("-10", "-10"), ("-20", "-20")];
        run_llvm_tests(tests);
    }

    #[test]
    fn test_char_literal() {
        let tests = vec![("'1'", "1"), ("'a'", "a"), ("'-'", "-")];
        run_llvm_tests(tests);
    }

    #[test]
    fn test_prefix_minus() {
        let tests = vec![
            ("-10", "-10"),
            ("let a = 10; -a", "-10"),
            ("-(10 + 20)", "-30"),
        ];
        run_llvm_tests(tests);
    }

    #[test]
    fn test_string_literal() {
        let tests = vec![
            (r#""1""#, "1"),
            (r#""a""#, "a"),
            (r#""abcdefg100""#, "abcdefg100"),
        ];
        run_llvm_tests(tests);
    }

    #[test]
    fn test_number_formula() {
        let tests = vec![
            ("5 + 5 + 5 + 5 - 10", "10"),
            ("2 * 2 * 2 * 2 * 2", "32"),
            ("-50 + 100 + -50", "0"),
            ("5 * 2 + 10", "20"),
            ("5 + 2 * 10", "25"),
            ("20 + 2 * -10", "0"),
            ("50 / 2 * 2 + 10", "60"),
            ("2 * (5 + 10)", "30"),
            ("3 * 3 * 3 + 10", "37"),
            ("3 * (3 * 3) + 10", "37"),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", "50"),
        ];
        run_llvm_tests(tests);
    }

    #[test]
    fn test_let_binding() {
        let tests = vec![
            ("let a = 10", "10"),
            ("let a = 10; a", "10"),
            ("let a = 10; a + 5", "15"),
            ("let a = 10; let b = 20; a + b", "30"),
            ("let a = 1 < 2; a", "1"),
            ("let a = 1 > 2; a", "0"),
            ("let a = 2 > 1; a", "1"),
            ("let a = 1 > 2; a", "0"),
            ("let a = 1 == 1; a", "1"),
            ("let a = 1 == 2; a", "0"),
            ("let a = if 1 { 10 } else { 20 }; a", "10"),
            ("let a = if 0 { 10 } else { 20 }; a", "20"),
            ("let a = \"abcdefg100\"", "abcdefg100"),
        ];
        run_llvm_tests(tests);
    }

    #[test]
    fn test_compare() {
        let tests = vec![
            ("0 > 0", "0"),
            ("0 < 0", "0"),
            ("0 > 1", "0"),
            ("0 < 1", "1"),
            ("0 == 0", "1"),
            ("0 == 1", "0"),
            ("let a = 10; a > 0", "1"),
            ("let a = 10; a < 0", "0"),
            ("let a = 10; (a < 10) < 10", "1"),
            ("let a = 10; (a > 10) > 10", "0"),
            ("let a = 10; (a == 10) == 10", "0"),
            ("let a = 10; (a == a) == 1", "1"),
        ];
        run_llvm_tests(tests);
    }

    #[test]
    fn test_bang() {
        let tests = vec![
            ("!0", "1"),
            ("!1", "0"),
            ("!10", "0"),
            ("!!0", "0"),
            ("!!1", "1"),
            ("!!10", "1"),
            ("let a = 10; !a", "0"),
            ("let a = 10; !!a", "1"),
        ];
        run_llvm_tests(tests);
    }

    #[test]
    fn test_if_else() {
        let tests = vec![
            ("if 1 { 10 } else { 20 }", "10"),
            ("if 0 { 10 } else { 20 }", "20"),
            ("if 1 { 10 }", "10"),
            ("if 2 { 10 } else { 20 }", "10"),
        ];
        run_llvm_tests(tests);
    }

    #[test]
    fn test_if_cond_by_comparing() {
        let tests = vec![
            ("if 5 > 2 { 10 } else { 20 }", "10"),
            ("if 2 > 5 { 10 } else { 20 }", "20"),
            ("if 5 < 8 { 10 } else { 20 }", "10"),
            ("if 8 < 5 { 10 } else { 20 }", "20"),
            ("if 5 == 5 { 10 } else { 20 }", "10"),
            ("if 5 == 6 { 10 } else { 20 }", "20"),
        ];
        run_llvm_tests(tests);
    }

    #[test]
    fn test_let_binding_in_if_else_block() {
        let tests = vec![
            ("if 1 { let a = 10; a } else { let b = 20; b }", "10"),
            ("if 0 { let a = 10; a } else { let b = 20; b }", "20"),
            ("if 1 { let a = 10; a } else { let a = 20; a }", "10"),
            ("if 0 { let a = 10; a } else { let a = 20; a }", "20"),
        ];
        run_llvm_tests(tests);
    }

    #[test]
    fn test_if_else_cond_by_identifier() {
        let tests = vec![
            ("let a = 1; if a { 10 } else { 20 }", "10"),
            ("let a = 0; if a { 10 } else { 20 }", "20"),
            ("let a = 2; if a { 10 } else { 20 }", "10"),
            ("let a = 2; if a == 2 { 10 } else { 20 }", "10"),
            ("let a = 2; if a == 3 { 10 } else { 20 }", "20"),
        ];
        run_llvm_tests(tests);
    }

    #[test]
    fn test_if_else_default_return() {
        let tests = vec![
            ("if 1 { }", "0"),
            ("if 0 { 10 } else { }", "0"),
            ("if 0 { 10 }", "0"),
        ];
        run_llvm_tests(tests);
    }

    #[test]
    fn test_if_else_scope() {
        let tests = vec![
            (
                "
                    let a = 10;
                    if 1 {
                        let b = 20;
                        a + b + 30
                    }
                ",
                "60",
            ),
            (
                "
                    let a = 10;
                    if 0 { }
                    else {
                        let b = 20;
                        a + b + 30
                    }
                ",
                "60",
            ),
            // TODO: create scope in if-else
            (
                "
                    if 1 {
                        let a = 10;
                    }
                    a
                ",
                "10",
            ),
            (
                "
                    if 0 { }
                    else {
                        let a = 10;
                    }
                    a
                ",
                "10",
            ),
        ];
        run_llvm_tests(tests);
    }

    fn run_llvm_tests(tests: Vec<(&'static str, &'static str)>) {
        tests.into_iter().for_each(|(input, expected)| {
            let context = Context::create();
            let module = context.create_module("top");
            let builder = context.create_builder();
            let execution_engine = module
                .create_jit_execution_engine(inkwell::OptimizationLevel::None)
                .unwrap();
            let mut compiler = Codegen::new(&context, &module, &builder, &execution_engine);
            let lexer = lexer::Lexer::new(input.to_string());
            let mut parser = parser::Parser::new(lexer);

            let program = match parser.parse_program() {
                Ok(p) => p,
                Err(e) => panic!("Parser error: {} by {}", e, input),
            };

            let main_fn = match compiler.compile(&program.into(), false, Output::CStringPtr) {
                Ok(f) => f,
                Err(e) => panic!("LLVM error: {} by {}", e, input),
            };

            let result_string = {
                let c_string_ptr = unsafe { main_fn.call() };
                unsafe { CString::from_raw(c_string_ptr) }
                    .into_string()
                    .unwrap()
            };

            assert_eq!(result_string, expected);
        });
    }
}
