/// MIRのテスト用フォーマット
pub struct Pretty<'a> {
    db: &'a dyn hir::HirMasterDatabase,
    mir_result: &'a crate::LowerResult,
}
impl<'a> Pretty<'a> {
    /// MIRのテスト用フォーマッタを作成します。
    pub fn new(db: &'a dyn hir::HirMasterDatabase, mir_result: &'a crate::LowerResult) -> Self {
        Self { db, mir_result }
    }

    /// MIRのテスト用フォーマットを返します。
    pub fn format(&self) -> String {
        let mut msg = "".to_string();

        for (_body_idx, body) in self.mir_result.bodies.iter() {
            let path = self.format_path(&body.path);
            let name = body.name.text(self.db);
            if path.is_empty() {
                msg.push_str(&format!("fn {name}("));
            } else {
                msg.push_str(&format!("fn {path}::{name}("));
            }

            msg.push_str(
                &self.format_params(
                    body.params
                        .iter()
                        .map(|(_idx, param)| param)
                        .collect::<Vec<_>>(),
                ),
            );

            let return_local = &body.locals[body.return_local];
            msg.push_str(&format!(") -> {} {{\n", self.debug_ty(&return_local.ty)));

            for (_variable_idx, variable) in body.locals.iter() {
                msg.push_str(&format!(
                    "{}let _{}: {}\n",
                    hir::testing::Pretty::format_indent(1),
                    variable.idx,
                    self.debug_ty(&variable.ty)
                ));
            }

            for (_basic_block_idx, basic_block) in body.blocks.iter() {
                msg.push('\n');

                msg.push_str(&format!(
                    "{}{}: {{\n",
                    hir::testing::Pretty::format_indent(1),
                    self.format_bb_name(basic_block)
                ));

                for statement in &basic_block.statements {
                    msg.push_str(&format!(
                        "{}{}\n",
                        hir::testing::Pretty::format_indent(2),
                        self.format_statement(statement, body)
                    ));
                }

                if let Some(termination) = &basic_block.termination {
                    msg.push_str(&format!(
                        "{}{}\n",
                        hir::testing::Pretty::format_indent(2),
                        self.format_termination(termination, body)
                    ));
                }

                msg.push_str(&format!("{}}}\n", hir::testing::Pretty::format_indent(1)));
            }

            msg.push_str("}\n");
        }

        msg
    }

    fn format_path(&self, path: &hir::Path) -> String {
        let mut msg = "".to_string();
        for (idx, segment) in path.segments(self.db).iter().enumerate() {
            if idx > 0 {
                msg.push_str("::");
            }
            msg.push_str(segment.text(self.db));
        }
        msg
    }

    fn format_params(&self, params: Vec<&crate::Param>) -> String {
        params
            .iter()
            .map(|param| self.format_param(param))
            .collect::<Vec<String>>()
            .join(", ")
    }

    fn format_param(&self, param: &crate::Param) -> String {
        format!("_{}: {}", param.idx, self.debug_ty(&param.ty))
    }

    fn format_statement(&self, statement: &crate::Statement, body: &crate::Body) -> String {
        match statement {
            crate::Statement::Assign { place, value } => {
                let place_msg = self.format_place(place, body);
                let value_msg = self.format_value(value, body);

                format!("{place_msg} = {value_msg}")
            }
        }
    }

    fn format_place(&self, place: &crate::Place, body: &crate::Body) -> String {
        match place {
            crate::Place::Param(param_idx) => {
                let param = &body.params[*param_idx];
                format!("_{}", param.idx)
            }
            crate::Place::Local(local_idx) => {
                let local = &body.locals[*local_idx];
                format!("_{}", local.idx)
            }
        }
    }

    fn format_value(&self, value: &crate::Value, body: &crate::Body) -> String {
        match value {
            crate::Value::Operand(operand) => self.format_operand(operand, body),
            crate::Value::BinaryOp { op, left, right } => {
                let function_name = match op {
                    crate::BinaryOp::Add => "add",
                    crate::BinaryOp::Sub => "sub",
                    crate::BinaryOp::Mul => "mul",
                    crate::BinaryOp::Div => "div",
                    crate::BinaryOp::Equal => "equal",
                    crate::BinaryOp::NotEq => "not_equal",
                    crate::BinaryOp::GreaterThan => "greater_than",
                    crate::BinaryOp::LessThan => "less_than",
                    crate::BinaryOp::GtEq => "gteq",
                    crate::BinaryOp::LtEq => "lteq",
                }
                .to_string();
                let left = self.format_operand(left, body);
                let right = self.format_operand(right, body);

                format!("{function_name}({left}, {right})")
            }
            crate::Value::UnaryOp { op, expr } => {
                let function_name = match op {
                    crate::UnaryOp::Neg => "negative",
                    crate::UnaryOp::Not => "not",
                }
                .to_string();
                let expr = self.format_operand(expr, body);

                format!("{function_name}({expr})")
            }
            crate::Value::Aggregate { kind, operands } => match kind {
                crate::AggregateKind::Struct(struct_) => {
                    let struct_name = struct_.name(self.db).text(self.db);

                    match struct_.kind(self.db) {
                        hir::StructKind::Tuple(_) => {
                            let operands = operands
                                .iter()
                                .map(|operand| self.format_operand(operand, body))
                                .collect::<Vec<String>>()
                                .join(", ");
                            format!("{struct_name}({operands})")
                        }
                        hir::StructKind::Record(fields) => {
                            let operands = operands
                                .iter()
                                .zip(fields.iter())
                                .map(|(operand, field)| {
                                    format!(
                                        "{}: {}",
                                        field.name.text(self.db),
                                        self.format_operand(operand, body)
                                    )
                                })
                                .collect::<Vec<String>>()
                                .join(", ");
                            format!("{struct_name} {{ {operands} }}")
                        }
                        hir::StructKind::Unit => {
                            assert!(operands.is_empty());
                            struct_name.to_string()
                        }
                    }
                }
            },
        }
    }

    fn format_constant(&self, constant: &crate::Constant) -> String {
        let const_value = match constant {
            crate::Constant::Integer(integer) => integer.to_string(),
            crate::Constant::Boolean(boolean) => boolean.to_string(),
            crate::Constant::String(string) => format!("\"{string}\""),
            crate::Constant::Unit => "()".to_string(),
        };
        format!("const {const_value}")
    }

    fn format_operand(&self, operand: &crate::Operand, body: &crate::Body) -> String {
        match operand {
            crate::Operand::Place(place) => self.format_place(place, body),
            crate::Operand::Constant(constant) => self.format_constant(constant),
        }
    }

    fn format_termination(&self, termination: &crate::Termination, body: &crate::Body) -> String {
        match termination {
            crate::Termination::Return(return_local_idx) => {
                let return_local = &body.locals[*return_local_idx];
                format!("return _{}", return_local.idx)
            }
            crate::Termination::Goto(to_bb_idx) => {
                let to_bb = &body.blocks[*to_bb_idx];
                format!("goto -> {}", self.format_bb_name(to_bb))
            }
            crate::Termination::Switch {
                condition,
                then_bb,
                else_bb,
            } => {
                let condition = self.format_place(condition, body);
                let then_bb_name = self.format_bb_name_by_idx(*then_bb, body);
                let else_bb_name = self.format_bb_name_by_idx(*else_bb, body);
                format!("switch({condition}) -> [true: {then_bb_name}, false: {else_bb_name}]")
            }
            crate::Termination::Call {
                function,
                args,
                destination,
                target,
            } => {
                let function = self.mir_result.body_by_function[function];
                let function_name = self.mir_result.bodies[function].name;
                let function_name = function_name.text(self.db);
                let args = self.format_args(args, body);
                let dest = self.format_place(destination, body);
                let target_bb_name = self.format_bb_name_by_idx(*target, body);

                format!("{dest} = {function_name}({args}) -> [return: {target_bb_name}]")
            }
        }
    }

    fn format_args(&self, args: &[crate::Operand], body: &crate::Body) -> String {
        args.iter()
            .map(|arg| self.format_operand(arg, body))
            .collect::<Vec<String>>()
            .join(", ")
    }

    fn debug_ty(&self, ty: &hir_ty::Monotype) -> String {
        match ty {
            hir_ty::Monotype::Unit => "()",
            hir_ty::Monotype::Integer => "int",
            hir_ty::Monotype::Bool => "bool",
            hir_ty::Monotype::Char => "char",
            hir_ty::Monotype::String => "string",
            hir_ty::Monotype::Never => "!",
            hir_ty::Monotype::Unknown => "unknown",
            hir_ty::Monotype::Struct(struct_) => {
                let struct_name = struct_.name(self.db).text(self.db);
                return format!("struct {}", struct_name);
            }
            hir_ty::Monotype::Variable(_) => unreachable!(),
            hir_ty::Monotype::Function(_) => todo!(),
        }
        .to_string()
    }

    fn format_bb_name_by_idx(
        &self,
        basic_block_idx: crate::Idx<crate::BasicBlock>,
        body: &crate::Body,
    ) -> String {
        let basic_block = &body.blocks[basic_block_idx];
        self.format_bb_name(basic_block)
    }

    fn format_bb_name(&self, basic_block: &crate::BasicBlock) -> String {
        basic_block.name()
    }
}
