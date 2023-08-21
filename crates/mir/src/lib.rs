//! MIRを構築するためのモジュールです。
//!
//! MIRはLLVM IRに変換しやすくするための中間表現です。
//! このモジュールでは、HIRとTyped HIRをMIRに変換するための機能を提供します。
//!
//! MIRに変換する際に以下を行います。
//! - 脱糖
//!   例えば、`for`式は`while`式に変換されます。
//!   脱糖することにより、MIRからLLVM IRへの変換が容易になります。
//! - 制御フローグラフの構築
//!   制御フローグラフは、基本ブロックとその間の制御フローからなります。
//!   例えば、`if`式は、`then`ブロックと`else`ブロックを持つ基本ブロックに変換されます。
//!   この構造は、LLVM IRの基本ブロックと制御フローに対応しており、LLVM IRへの変換が容易になります。
#![warn(missing_docs)]

use std::collections::HashMap;

use hir_ty::ResolvedType;
use la_arena::{Arena, Idx};

/// HIRとTyped HIRからMIRを構築する
pub fn lower(
    salsa_db: &dyn hir::Db,
    hir_result: &hir::LowerResult,
    hir_ty_result: &hir_ty::TyLowerResult,
) -> LowerResult {
    let mir_lower = MirLower {
        hir_result,
        hir_ty_result,
    };

    mir_lower.lower(salsa_db)
}

struct FunctionLower<'a> {
    hir_result: &'a hir::LowerResult,
    hir_ty_result: &'a hir_ty::TyLowerResult,
    function_id_by_hir_function: &'a HashMap<hir::FunctionId, FunctionId>,
    path: hir::Path,
    function_id: hir::FunctionId,

    return_local: Idx<Local>,
    params: Arena<Param>,
    param_by_hir: HashMap<hir::ParamId, Idx<Param>>,
    locals: Arena<Local>,
    local_idx: u64,
    blocks: Arena<BasicBlock>,
    switch_idx: u64,
    current_bb: Option<Idx<BasicBlock>>,
    block_idx: u64,
    local_by_hir: HashMap<hir::ExprId, Idx<Local>>,
    exit_bb_idx: Option<Idx<BasicBlock>>,
}

impl<'a> FunctionLower<'a> {
    fn new(
        hir_result: &'a hir::LowerResult,
        hir_ty_result: &'a hir_ty::TyLowerResult,
        function_id_by_hir_function: &'a HashMap<hir::FunctionId, FunctionId>,
        path: hir::Path,
        function_id: hir::FunctionId,
    ) -> Self {
        let mut locals = Arena::new();
        let signature = &hir_ty_result.signature_by_function(function_id);
        let return_local = locals.alloc(Local {
            ty: signature.return_type,
            idx: 0,
        });

        FunctionLower {
            hir_result,
            hir_ty_result,
            function_id_by_hir_function,
            path,
            function_id,
            local_idx: 1,
            return_local,
            params: Arena::new(),
            param_by_hir: HashMap::new(),
            locals,
            blocks: Arena::new(),
            switch_idx: 0,
            current_bb: None,
            block_idx: 0,
            local_by_hir: HashMap::new(),
            exit_bb_idx: None,
        }
    }

    fn exit_bb_idx(&self) -> Idx<BasicBlock> {
        self.exit_bb_idx.unwrap()
    }

    fn add_statement_to_current_bb(&mut self, statement: Statement) {
        let current_bb = &mut self.blocks[self.current_bb.unwrap()];
        current_bb.add_statement(statement);
    }

    fn add_termination_to_current_bb(&mut self, termination: Termination) {
        let current_bb = &mut self.blocks[self.current_bb.unwrap()];
        assert!(matches!(current_bb.termination, None));
        current_bb.termination = Some(termination);
    }

    fn alloc_local(&mut self, expr: hir::ExprId) -> Idx<Local> {
        let ty = self.hir_ty_result.type_by_expr(expr);
        let local_idx = self.alloc_local_by_ty(ty);
        self.local_by_hir.insert(expr, local_idx);

        local_idx
    }

    fn alloc_local_by_ty(&mut self, ty: ResolvedType) -> Idx<Local> {
        let local = Local {
            ty,
            idx: self.local_idx,
        };
        self.local_idx += 1;

        self.locals.alloc(local)
    }

    fn get_local_by_expr(&self, expr: hir::ExprId) -> Idx<Local> {
        *self.local_by_hir.get(&expr).unwrap()
    }

    fn get_param_by_expr(&self, param: hir::ParamId) -> Idx<Param> {
        self.param_by_hir[&param]
    }

    fn alloc_entry_bb(&mut self) -> Idx<BasicBlock> {
        let entry_bb = BasicBlock::new_entry_bb(0);
        self.blocks.alloc(entry_bb)
    }

    fn alloc_exit_bb(&mut self) -> Idx<BasicBlock> {
        assert!(matches!(self.exit_bb_idx, None));

        let mut exit_bb = BasicBlock::new_exit_bb(0);
        exit_bb.termination = Some(Termination::Return(self.return_local));

        let exit_bb_idx = self.blocks.alloc(exit_bb);
        self.exit_bb_idx = Some(exit_bb_idx);

        exit_bb_idx
    }

    fn alloc_switch_bb(&mut self) -> AllocatedSwitchBB {
        let then_bb = BasicBlock::new_then_bb(self.switch_idx);
        let then_bb_idx = self.blocks.alloc(then_bb);

        let else_bb = BasicBlock::new_else_bb(self.switch_idx);
        let else_bb_idx = self.blocks.alloc(else_bb);

        self.switch_idx += 1;

        AllocatedSwitchBB {
            then_bb_idx,
            else_bb_idx,
        }
    }

    fn alloc_dest_bb_and_result_local(
        &mut self,
        expr: hir::ExprId,
    ) -> (Idx<BasicBlock>, Idx<Local>) {
        let dest_bb_idx = {
            let dest_bb = BasicBlock::new_standard_bb(self.block_idx);
            self.block_idx += 1;
            self.blocks.alloc(dest_bb)
        };

        let result_local_idx = self.alloc_local(expr);

        (dest_bb_idx, result_local_idx)
    }

    fn alloc_standard_bb(&mut self) -> Idx<BasicBlock> {
        let dest_bb = BasicBlock::new_standard_bb(self.block_idx);
        self.block_idx += 1;
        self.blocks.alloc(dest_bb)
    }

    fn lower_expr(&mut self, expr_id: hir::ExprId) -> LoweredExpr {
        let expr = expr_id.lookup(&self.hir_result.shared_ctx);
        match expr {
            hir::Expr::Symbol(symbol) => match symbol {
                hir::Symbol::Param { name: _, param } => LoweredExpr::Operand(Operand::Place(
                    Place::Param(self.get_param_by_expr(*param)),
                )),
                hir::Symbol::Local { name: _, expr } => LoweredExpr::Operand(Operand::Place(
                    Place::Local(self.get_local_by_expr(*expr)),
                )),
                hir::Symbol::Function { .. } => todo!(),
                hir::Symbol::Missing { .. } => unreachable!(),
            },
            hir::Expr::Literal(literal) => match literal {
                hir::Literal::Integer(value) => {
                    LoweredExpr::Operand(Operand::Constant(Constant::Integer(*value)))
                }
                hir::Literal::Bool(value) => {
                    LoweredExpr::Operand(Operand::Constant(Constant::Boolean(*value)))
                }
                hir::Literal::String(string) => {
                    LoweredExpr::Operand(Operand::Constant(Constant::String(string.to_owned())))
                }
                _ => todo!(),
            },
            hir::Expr::Return { value } => {
                if let Some(value) = value {
                    let operand = match self.lower_expr(*value) {
                        LoweredExpr::Operand(operand) => operand,
                        LoweredExpr::Return => return LoweredExpr::Return,
                    };
                    let return_value_place = Place::Local(self.return_local);
                    self.add_statement_to_current_bb(Statement::Assign {
                        place: return_value_place,
                        value: operand.into(),
                    });
                }

                self.add_termination_to_current_bb(Termination::Goto(self.exit_bb_idx()));
                self.current_bb = Some(self.exit_bb_idx());

                LoweredExpr::Return
            }
            hir::Expr::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond_local_idx = {
                    let cond_local_idx = self.alloc_local(*condition);
                    let cond_operand = match self.lower_expr(*condition) {
                        LoweredExpr::Operand(operand) => operand,
                        LoweredExpr::Return => return LoweredExpr::Return,
                    };
                    let place = Place::Local(cond_local_idx);
                    self.add_statement_to_current_bb(Statement::Assign {
                        place,
                        value: cond_operand.into(),
                    });

                    cond_local_idx
                };

                let mut dest_bb_and_result_local_idx: Option<(Idx<BasicBlock>, Idx<Local>)> = None;

                let switch_bb = self.alloc_switch_bb();
                self.add_termination_to_current_bb(Termination::Switch {
                    condition: Place::Local(cond_local_idx),
                    then_bb: switch_bb.then_bb_idx,
                    else_bb: switch_bb.else_bb_idx,
                });

                {
                    self.current_bb = Some(switch_bb.then_bb_idx);

                    let then_block = match then_branch.lookup(&self.hir_result.shared_ctx) {
                        hir::Expr::Block(block) => block,
                        _ => unreachable!(),
                    };

                    let mut has_return = false;
                    for stmt in &then_block.stmts {
                        match self.lower_stmt(stmt) {
                            LoweredStmt::Return => {
                                has_return = true;
                                break;
                            }
                            LoweredStmt::Unit => (),
                        }
                    }
                    if !has_return {
                        if let Some(tail) = then_block.tail {
                            match self.lower_expr(tail) {
                                LoweredExpr::Operand(operand) => {
                                    let (dest_bb_idx, result_local_idx) =
                                        match dest_bb_and_result_local_idx {
                                            Some(idxes) => idxes,
                                            None => {
                                                let idxes = self
                                                    .alloc_dest_bb_and_result_local(*then_branch);
                                                dest_bb_and_result_local_idx = Some(idxes);

                                                idxes
                                            }
                                        };

                                    self.add_statement_to_current_bb(Statement::Assign {
                                        place: Place::Local(result_local_idx),
                                        value: operand.into(),
                                    });
                                    self.add_termination_to_current_bb(Termination::Goto(
                                        dest_bb_idx,
                                    ));
                                }
                                LoweredExpr::Return => (),
                            };
                        }
                    }
                }

                {
                    self.current_bb = Some(switch_bb.else_bb_idx);

                    match else_branch {
                        Some(else_block) => {
                            let else_block = match else_block.lookup(&self.hir_result.shared_ctx) {
                                hir::Expr::Block(block) => block,
                                _ => unreachable!(),
                            };

                            let mut has_return = false;
                            for stmt in &else_block.stmts {
                                match self.lower_stmt(stmt) {
                                    LoweredStmt::Return => {
                                        has_return = true;
                                        break;
                                    }
                                    LoweredStmt::Unit => (),
                                }
                            }

                            if !has_return {
                                if let Some(tail) = else_block.tail {
                                    match self.lower_expr(tail) {
                                        LoweredExpr::Operand(operand) => {
                                            let (dest_bb_idx, result_local_idx) =
                                                match dest_bb_and_result_local_idx {
                                                    Some(idxes) => idxes,
                                                    None => {
                                                        let idxes = self
                                                            .alloc_dest_bb_and_result_local(
                                                                *then_branch,
                                                            );
                                                        dest_bb_and_result_local_idx = Some(idxes);

                                                        idxes
                                                    }
                                                };

                                            self.add_statement_to_current_bb(Statement::Assign {
                                                place: Place::Local(result_local_idx),
                                                value: operand.into(),
                                            });
                                            self.add_termination_to_current_bb(Termination::Goto(
                                                dest_bb_idx,
                                            ));
                                        }
                                        LoweredExpr::Return => (),
                                    };
                                }
                            }

                            if let Some((dest_bb_idx, result_local_idx)) =
                                dest_bb_and_result_local_idx
                            {
                                self.current_bb = Some(dest_bb_idx);
                                LoweredExpr::Operand(Operand::Place(Place::Local(result_local_idx)))
                            } else {
                                LoweredExpr::Return
                            }
                        }
                        None => match dest_bb_and_result_local_idx {
                            Some((dest_bb_idx, result_local_idx)) => {
                                let unit = Operand::Constant(Constant::Unit);
                                self.add_statement_to_current_bb(Statement::Assign {
                                    place: Place::Local(result_local_idx),
                                    value: unit.into(),
                                });

                                self.add_termination_to_current_bb(Termination::Goto(dest_bb_idx));
                                self.current_bb = Some(dest_bb_idx);

                                LoweredExpr::Operand(Operand::Place(Place::Local(result_local_idx)))
                            }
                            None => {
                                let dest_bb_idx = self.alloc_standard_bb();
                                self.add_termination_to_current_bb(Termination::Goto(dest_bb_idx));
                                self.current_bb = Some(dest_bb_idx);

                                LoweredExpr::Operand(Operand::Constant(Constant::Unit))
                            }
                        },
                    }
                }
            }
            hir::Expr::Binary { op, lhs, rhs } => {
                let lhs = match self.lower_expr(*lhs) {
                    LoweredExpr::Return => return LoweredExpr::Return,
                    LoweredExpr::Operand(operand) => operand,
                };

                let rhs = match self.lower_expr(*rhs) {
                    LoweredExpr::Return => return LoweredExpr::Return,
                    LoweredExpr::Operand(operand) => operand,
                };

                let op = match op {
                    ast::BinaryOp::Add(_) => BinaryOp::Add,
                    ast::BinaryOp::Sub(_) => BinaryOp::Sub,
                    ast::BinaryOp::Mul(_) => BinaryOp::Mul,
                    ast::BinaryOp::Div(_) => BinaryOp::Div,
                    ast::BinaryOp::Equal(_) => BinaryOp::Equal,
                    ast::BinaryOp::GreaterThan(_) => BinaryOp::GreaterThan,
                    ast::BinaryOp::LessThan(_) => BinaryOp::LessThan,
                };

                let local = self.alloc_local(expr_id);
                let value = Value::BinaryOp {
                    op,
                    left: lhs,
                    right: rhs,
                };
                let place = Place::Local(local);
                self.add_statement_to_current_bb(Statement::Assign { place, value });

                LoweredExpr::Operand(Operand::Place(place))
            }
            hir::Expr::Unary { op, expr } => {
                let expr = match self.lower_expr(*expr) {
                    LoweredExpr::Return => return LoweredExpr::Return,
                    LoweredExpr::Operand(operand) => operand,
                };
                let op = match op {
                    ast::UnaryOp::Neg(_) => UnaryOp::Neg,
                    ast::UnaryOp::Not(_) => UnaryOp::Not,
                };
                let local = self.alloc_local(expr_id);
                let value = Value::UnaryOp { op, expr };
                let place = Place::Local(local);
                self.add_statement_to_current_bb(Statement::Assign { place, value });

                LoweredExpr::Operand(Operand::Place(place))
            }
            hir::Expr::Block(block) => {
                for stmt in &block.stmts {
                    match self.lower_stmt(stmt) {
                        LoweredStmt::Return => return LoweredExpr::Return,
                        LoweredStmt::Unit => (),
                    }
                }

                if let Some(tail) = block.tail {
                    self.lower_expr(tail)
                } else {
                    LoweredExpr::Operand(Operand::Constant(Constant::Unit))
                }
            }
            hir::Expr::Call { callee, args } => {
                let mut arg_operands = vec![];
                for arg in args {
                    match self.lower_expr(*arg) {
                        LoweredExpr::Return => return LoweredExpr::Return,
                        LoweredExpr::Operand(operand) => {
                            arg_operands.push(operand);
                        }
                    }
                }

                match callee {
                    hir::Symbol::Param { .. } => unimplemented!(),
                    hir::Symbol::Local { .. } => unimplemented!(),
                    hir::Symbol::Function { path: _, function } => {
                        let function_id = self.function_id_by_hir_function[function];

                        let signature = self.hir_ty_result.signature_by_function(*function);
                        let called_local = self.alloc_local_by_ty(signature.return_type);
                        let dest_place = Place::Local(called_local);

                        let target_bb = self.alloc_standard_bb();

                        self.add_termination_to_current_bb(Termination::Call {
                            function: function_id,
                            args: arg_operands,
                            destination: dest_place,
                            target: target_bb,
                        });
                        self.current_bb = Some(target_bb);

                        LoweredExpr::Operand(Operand::Place(dest_place))
                    }
                    hir::Symbol::Missing { .. } => unreachable!(),
                }
            }
            hir::Expr::Missing => unreachable!(),
        }
    }

    fn lower_stmt(&mut self, stmt: &hir::Stmt) -> LoweredStmt {
        match stmt {
            hir::Stmt::VariableDef { name: _, value } => {
                let local_idx = self.alloc_local(*value);
                let operand = match self.lower_expr(*value) {
                    LoweredExpr::Operand(operand) => operand,
                    LoweredExpr::Return => {
                        return LoweredStmt::Return;
                    }
                };
                self.add_statement_to_current_bb(Statement::Assign {
                    place: Place::Local(local_idx),
                    value: operand.into(),
                });
            }
            hir::Stmt::ExprStmt { expr, .. } => {
                match self.lower_expr(*expr) {
                    LoweredExpr::Operand(operand) => operand,
                    LoweredExpr::Return => {
                        return LoweredStmt::Return;
                    }
                };
            }
            hir::Stmt::Item { item } => match item {
                hir::ItemDefId::Function(_) => unreachable!(),
                hir::ItemDefId::Module(_) | hir::ItemDefId::UseItem(_) => {
                    return LoweredStmt::Unit;
                }
            },
        }

        LoweredStmt::Unit
    }

    fn lower(mut self) -> Body {
        let function = self.function_id.lookup(&self.hir_result.db);

        for param in &function.params {
            let param_ty = self.hir_ty_result.type_by_param(*param);
            let param_idx = self.params.alloc(Param {
                ty: param_ty,
                idx: self.local_idx,
                pos: param.lookup(&self.hir_result.db).pos.try_into().unwrap(),
            });
            self.param_by_hir.insert(*param, param_idx);

            self.local_idx += 1;
        }

        let body_block = self
            .hir_result
            .function_body_by_function(self.function_id)
            .unwrap();
        let body_block = match body_block {
            hir::Expr::Block(block) => block,
            _ => unreachable!(),
        };

        let entry_bb_idx = self.alloc_entry_bb();
        let exit_bb_idx = self.alloc_exit_bb();

        self.current_bb = Some(entry_bb_idx);

        let mut has_return = false;
        for stmt in &body_block.stmts {
            match self.lower_stmt(stmt) {
                LoweredStmt::Return => {
                    has_return = true;
                    break;
                }
                LoweredStmt::Unit => (),
            }
        }

        if !has_return {
            if let Some(tail) = body_block.tail {
                match self.lower_expr(tail) {
                    LoweredExpr::Operand(operand) => {
                        self.add_statement_to_current_bb(Statement::Assign {
                            place: Place::Local(self.return_local),
                            value: operand.into(),
                        });
                        self.add_termination_to_current_bb(Termination::Goto(exit_bb_idx));
                        self.current_bb = Some(exit_bb_idx);
                    }
                    LoweredExpr::Return => (),
                };
            } else {
                self.add_termination_to_current_bb(Termination::Goto(exit_bb_idx));
                self.current_bb = Some(exit_bb_idx);
            }
        }

        Body {
            path: self.path,
            name: function.name.unwrap(),
            params: self.params,
            return_local: self.return_local,
            locals: self.locals,
            blocks: self.blocks,
        }
    }
}

/// 関数ID
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FunctionId(u32);

/// 関数IDを生成する
///
/// 生成されるIDは0から始まり、1ずつ増えていきます。
struct FunctionIdGenerator {
    id: u32,
}
impl FunctionIdGenerator {
    fn new() -> Self {
        Self { id: 0 }
    }

    fn gen(&mut self) -> FunctionId {
        let id = self.id;
        self.id += 1;

        FunctionId(id)
    }
}

struct MirLower<'a> {
    hir_result: &'a hir::LowerResult,
    hir_ty_result: &'a hir_ty::TyLowerResult,
}

impl<'a> MirLower<'a> {
    fn lower(self, salsa_db: &dyn hir::Db) -> LowerResult {
        let mut entry_point: Option<Idx<Body>> = None;
        let mut bodies = Arena::new();

        let function_id_by_hir_function = {
            let mut function_id_resolver = FunctionIdGenerator::new();
            let mut function_id_by_hir_function = HashMap::<hir::FunctionId, FunctionId>::new();
            for (function_id, _function) in self.hir_result.db.functions() {
                function_id_by_hir_function.insert(function_id, function_id_resolver.gen());
            }

            function_id_by_hir_function
        };

        let mut body_by_function = HashMap::<FunctionId, Idx<Body>>::new();
        let mut function_by_body = HashMap::<Idx<Body>, FunctionId>::new();
        for (function_id, function) in self.hir_result.db.functions() {
            let lower = FunctionLower::new(
                self.hir_result,
                self.hir_ty_result,
                &function_id_by_hir_function,
                function.path.clone(),
                function_id,
            );
            let body = lower.lower();
            let body_idx = bodies.alloc(body);

            body_by_function.insert(
                *function_id_by_hir_function.get(&function_id).unwrap(),
                body_idx,
            );
            function_by_body.insert(
                body_idx,
                *function_id_by_hir_function.get(&function_id).unwrap(),
            );

            let name = function.name.unwrap().text(salsa_db);
            if name == "main" {
                assert_eq!(entry_point, None);
                entry_point = Some(body_idx);
            }
        }

        LowerResult {
            entry_point,
            bodies,
            body_by_function,
            function_by_body,
        }
    }
}

/// MIRの構築結果
#[derive(Debug)]
pub struct LowerResult {
    entry_point: Option<Idx<Body>>,
    bodies: Arena<Body>,
    body_by_function: HashMap<FunctionId, Idx<Body>>,
    function_by_body: HashMap<Idx<Body>, FunctionId>,
}

impl LowerResult {
    /// エントリポイントのボディを返す
    pub fn entry_point(&self) -> Option<&Body> {
        self.entry_point.map(|idx| &self.bodies[idx])
    }

    /// エントリポイントの関数IDを返す
    pub fn function_id_of_entry_point(&self) -> Option<FunctionId> {
        self.entry_point.map(|idx| self.function_by_body[&idx])
    }

    /// 関数IDからボディを返す
    pub fn body_by_function_id(&self, function_id: FunctionId) -> &Body {
        let body_idx = self.body_by_function.get(&function_id).unwrap();
        &self.bodies[*body_idx]
    }

    /// ボディから関数IDを返す
    pub fn function_id_by_body_idx(&self, idx: Idx<Body>) -> FunctionId {
        self.function_by_body[&idx]
    }

    /// (ボディインデックス, ボディ)のイテレータを返す
    pub fn ref_bodies(&self) -> impl Iterator<Item = (Idx<Body>, &Body)> {
        self.bodies.iter()
    }
}

/// 関数のボディ
#[derive(Debug)]
pub struct Body {
    /// 関数のパス
    pub path: hir::Path,
    /// 関数名
    pub name: hir::Name,
    /// 関数のパラメータ一覧
    pub params: Arena<Param>,
    /// 関数の戻り値
    pub return_local: LocalIdx,
    /// 関数内のローカル変数一覧
    pub locals: Arena<Local>,
    /// 関数内の基本ブロック一覧
    pub blocks: Arena<BasicBlock>,
}
impl Body {
    /// 関数のシグネチャを返す
    pub fn signature(&self) -> Signature {
        let params = self.params.iter().map(|(idx, _param)| idx).collect();
        Signature {
            params,
            return_value: self.return_local,
        }
    }
}

/// 関数のシグネチャ
#[derive(Debug)]
pub struct Signature {
    /// パラメータ一覧
    pub params: Vec<ParamIdx>,
    /// 戻り値
    pub return_value: LocalIdx,
}

/// 関数のパラメータ
#[derive(Debug)]
pub struct Param {
    /// パラメータの型
    pub ty: ResolvedType,
    /// パラメータのローカル変数のインデックス
    pub idx: u64,
    /// パラメータの位置
    pub pos: u32,
}
/// パラメータの保持インデックス
pub type ParamIdx = Idx<Param>;

/// 関数内のローカル変数
#[derive(Debug)]
pub struct Local {
    /// ローカル変数の型
    pub ty: ResolvedType,
    /// ローカル変数のインデックス
    pub idx: u64,
}
/// ローカル変数の保持インデックス
pub type LocalIdx = Idx<Local>;

/// 基本ブロック
#[derive(Debug)]
pub struct BasicBlock {
    /// 基本ブロックの種類
    pub kind: BasicBlockKind,
    /// 基本ブロック内の文一覧
    pub statements: Vec<Statement>,
    /// 基本ブロックの終端
    pub termination: Option<Termination>,
    /// 基本ブロックのインデックス
    idx: u64,
}

impl BasicBlock {
    /// 基本ブロックの名前を返す
    pub fn name(&self) -> String {
        match self.kind {
            crate::BasicBlockKind::Entry => "entry".to_string(),
            crate::BasicBlockKind::Exit => "exit".to_string(),
            crate::BasicBlockKind::Standard => {
                format!("bb{}", self.idx)
            }
            crate::BasicBlockKind::Then => {
                format!("then{}", self.idx)
            }
            crate::BasicBlockKind::Else => {
                format!("else{}", self.idx)
            }
        }
    }

    fn new_entry_bb(idx: u64) -> Self {
        Self {
            kind: BasicBlockKind::Entry,
            statements: vec![],
            termination: None,
            idx,
        }
    }

    fn new_exit_bb(idx: u64) -> Self {
        Self {
            kind: BasicBlockKind::Exit,
            statements: vec![],
            termination: None,
            idx,
        }
    }

    fn new_then_bb(idx: u64) -> Self {
        Self {
            kind: BasicBlockKind::Then,
            statements: vec![],
            termination: None,
            idx,
        }
    }

    fn new_else_bb(idx: u64) -> Self {
        Self {
            kind: BasicBlockKind::Else,
            statements: vec![],
            termination: None,
            idx,
        }
    }

    fn new_standard_bb(idx: u64) -> Self {
        Self {
            kind: BasicBlockKind::Standard,
            statements: vec![],
            termination: None,
            idx,
        }
    }

    fn add_statement(&mut self, stmt: Statement) {
        self.statements.push(stmt);
    }
}

/// 基本ブロックの保持インデックス
pub type BasicBlockIdx = Idx<BasicBlock>;

struct AllocatedSwitchBB {
    then_bb_idx: Idx<BasicBlock>,
    else_bb_idx: Idx<BasicBlock>,
}

/// 値の位置を指す
/// 参照のようなもの
#[derive(Debug, Clone, Copy)]
pub enum Place {
    /// 関数パラメータ
    Param(Idx<Param>),
    /// ローカル変数
    Local(Idx<Local>),
}

/// 値
#[derive(Debug)]
pub enum Value {
    /// オペランド(値)
    Operand(Operand),
    /// 二項演算
    BinaryOp {
        /// 二項演算子
        op: BinaryOp,
        /// 左辺
        left: Operand,
        /// 右辺
        right: Operand,
    },
    /// 単項演算
    UnaryOp {
        /// 単項演算子
        op: UnaryOp,
        /// 式
        expr: Operand,
    },
}

/// 二項演算子
#[derive(Debug)]
pub enum BinaryOp {
    /// 加算
    Add,
    /// 減算
    Sub,
    /// 乗算
    Mul,
    /// 除算
    Div,
    /// 等価
    Equal,
    /// 大なり(>)
    GreaterThan,
    /// 小なり(<)
    LessThan,
}

/// 単項演算子
#[derive(Debug)]
pub enum UnaryOp {
    /// 負符号(-)
    Neg,
    /// 論理否定(!)
    Not,
}

/// オペランド
#[derive(Debug)]
pub enum Operand {
    /// 値の位置
    Place(Place),
    /// 定数
    Constant(Constant),
}
impl From<Operand> for Value {
    fn from(value: Operand) -> Self {
        Self::Operand(value)
    }
}

#[derive(Debug)]
enum LoweredExpr {
    Return,
    Operand(Operand),
}

#[derive(Debug)]
enum LoweredStmt {
    Return,
    Unit,
}

/// 定数
#[derive(Debug)]
pub enum Constant {
    /// 整数
    Integer(u64),
    /// 真偽値
    Boolean(bool),
    /// 文字列
    String(String),
    /// 単値
    Unit,
}

/// ステートメント
#[derive(Debug)]
pub enum Statement {
    /// 値の代入
    Assign {
        /// 代入先
        place: Place,
        /// 代入する値
        value: Value,
    },
}

/// 終端
#[derive(Debug)]
pub enum Termination {
    /// 戻り値
    Return(Idx<Local>),
    /// ジャンプ
    Goto(Idx<BasicBlock>),
    /// 条件分岐
    Switch {
        /// 条件
        condition: Place,
        /// 条件が真の場合にジャンプする先の基本ブロック
        then_bb: Idx<BasicBlock>,
        /// 条件が偽の場合にジャンプする先の基本ブロック
        else_bb: Idx<BasicBlock>,
    },
    /// 関数呼び出し
    Call {
        /// 呼び出し対象の関数
        function: FunctionId,
        /// 引数
        args: Vec<Operand>,
        /// 戻り値の代入先
        destination: Place,
        /// 呼び出し後にジャンプする先の基本ブロック
        target: Idx<BasicBlock>,
    },
}

/// 基本ブロックの種類
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BasicBlockKind {
    /// 関数の開始を表す基本ブロック
    Entry,
    /// 関数の終了を表す基本ブロック
    Exit,
    /// 標準的な基本ブロック
    Standard,
    /// 条件分岐の真の場合の基本ブロック
    Then,
    /// 条件分岐の偽の場合の基本ブロック
    Else,
}

#[cfg(test)]
mod tests {
    use expect_test::{expect, Expect};
    use hir::SourceDatabaseTrait;
    use hir_ty::ResolvedType;

    fn check_in_root_file(fixture: &str, expect: Expect) {
        let mut fixture = fixture.to_string();
        fixture.insert_str(0, "//- /main.nail\n");

        let salsa_db = hir::SalsaDatabase::default();
        let source_db = hir::FixtureDatabase::new(&salsa_db, &fixture);

        let ast = hir::parse_to_ast(&salsa_db, source_db.source_root());
        let hir_result = hir::build_hir(&salsa_db, ast);
        let ty_hir_result = hir_ty::lower(&hir_result);

        let mir_result = crate::lower(&salsa_db, &hir_result, &ty_hir_result);

        expect.assert_eq(&debug(&salsa_db, &hir_result, &ty_hir_result, &mir_result));
    }

    fn indent(nesting: usize) -> String {
        "    ".repeat(nesting)
    }

    fn debug_path(salsa_db: &dyn hir::Db, path: &hir::Path) -> String {
        let mut msg = "".to_string();
        for (idx, segment) in path.segments().iter().enumerate() {
            if idx > 0 {
                msg.push_str("::");
            }
            msg.push_str(segment.text(salsa_db));
        }
        msg
    }

    fn debug(
        salsa_db: &dyn hir::Db,
        _hir_result: &hir::LowerResult,
        _hir_ty_result: &hir_ty::TyLowerResult,
        mir_result: &crate::LowerResult,
    ) -> String {
        let mut msg = "".to_string();

        for (_body_idx, body) in mir_result.bodies.iter() {
            let path = debug_path(salsa_db, &body.path);
            let name = body.name.text(salsa_db);
            if path.is_empty() {
                msg.push_str(&format!("fn {name}("));
            } else {
                msg.push_str(&format!("fn {path}::{name}("));
            }

            msg.push_str(&debug_params(
                body.params
                    .iter()
                    .map(|(_idx, param)| param)
                    .collect::<Vec<_>>(),
            ));

            let return_local = &body.locals[body.return_local];
            msg.push_str(&format!(") -> {} {{\n", debug_ty(&return_local.ty)));

            for (_variable_idx, variable) in body.locals.iter() {
                msg.push_str(&format!(
                    "{}let _{}: {}\n",
                    indent(1),
                    variable.idx,
                    debug_ty(&variable.ty)
                ));
            }

            for (_basic_block_idx, basic_block) in body.blocks.iter() {
                msg.push('\n');

                msg.push_str(&format!(
                    "{}{}: {{\n",
                    indent(1),
                    debug_bb_name(basic_block)
                ));

                for statement in &basic_block.statements {
                    msg.push_str(&format!(
                        "{}{}\n",
                        indent(2),
                        debug_statement(statement, body)
                    ));
                }

                if let Some(termination) = &basic_block.termination {
                    msg.push_str(&format!(
                        "{}{}\n",
                        indent(2),
                        debug_termination(salsa_db, termination, body, mir_result)
                    ));
                }

                msg.push_str(&format!("{}}}\n", indent(1)));
            }

            msg.push_str("}\n");
        }

        msg
    }

    fn debug_params(params: Vec<&crate::Param>) -> String {
        params
            .iter()
            .map(|param| debug_param(param))
            .collect::<Vec<String>>()
            .join(", ")
    }

    fn debug_param(param: &crate::Param) -> String {
        format!("_{}: {}", param.idx, debug_ty(&param.ty))
    }

    fn debug_statement(statement: &crate::Statement, body: &crate::Body) -> String {
        match statement {
            crate::Statement::Assign { place, value } => {
                let place_msg = debug_place(place, body);
                let value_msg = debug_value(value, body);

                format!("{place_msg} = {value_msg}")
            }
        }
    }

    fn debug_place(place: &crate::Place, body: &crate::Body) -> String {
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

    fn debug_value(value: &crate::Value, body: &crate::Body) -> String {
        match value {
            crate::Value::Operand(operand) => debug_operand(operand, body),
            crate::Value::BinaryOp { op, left, right } => {
                let function_name = match op {
                    crate::BinaryOp::Add => "add",
                    crate::BinaryOp::Sub => "sub",
                    crate::BinaryOp::Mul => "mul",
                    crate::BinaryOp::Div => "div",
                    crate::BinaryOp::Equal => "equal",
                    crate::BinaryOp::GreaterThan => "greater_than",
                    crate::BinaryOp::LessThan => "less_than",
                }
                .to_string();
                let left = debug_operand(left, body);
                let right = debug_operand(right, body);

                format!("{function_name}({left}, {right})")
            }
            crate::Value::UnaryOp { op, expr } => {
                let function_name = match op {
                    crate::UnaryOp::Neg => "negative",
                    crate::UnaryOp::Not => "not",
                }
                .to_string();
                let expr = debug_operand(expr, body);

                format!("{function_name}({expr})")
            }
        }
    }

    fn debug_constant(constant: &crate::Constant) -> String {
        let const_value = match constant {
            crate::Constant::Integer(integer) => integer.to_string(),
            crate::Constant::Boolean(boolean) => boolean.to_string(),
            crate::Constant::String(string) => format!("\"{string}\""),
            crate::Constant::Unit => "()".to_string(),
        };
        format!("const {const_value}")
    }

    fn debug_operand(operand: &crate::Operand, body: &crate::Body) -> String {
        match operand {
            crate::Operand::Place(place) => debug_place(place, body),
            crate::Operand::Constant(constant) => debug_constant(constant),
        }
    }

    fn debug_termination(
        salsa_db: &dyn hir::Db,
        termination: &crate::Termination,
        body: &crate::Body,
        mir_result: &crate::LowerResult,
    ) -> String {
        match termination {
            crate::Termination::Return(return_local_idx) => {
                let return_local = &body.locals[*return_local_idx];
                format!("return _{}", return_local.idx)
            }
            crate::Termination::Goto(to_bb_idx) => {
                let to_bb = &body.blocks[*to_bb_idx];
                format!("goto -> {}", debug_bb_name(to_bb))
            }
            crate::Termination::Switch {
                condition,
                then_bb,
                else_bb,
            } => {
                let condition = debug_place(condition, body);
                let then_bb_name = debug_bb_name_by_idx(*then_bb, body);
                let else_bb_name = debug_bb_name_by_idx(*else_bb, body);
                format!("switch({condition}) -> [true: {then_bb_name}, false: {else_bb_name}]")
            }
            crate::Termination::Call {
                function,
                args,
                destination,
                target,
            } => {
                let function = mir_result.body_by_function[function];
                let function_name = mir_result.bodies[function].name;
                let function_name = function_name.text(salsa_db);
                let args = debug_args(args, body);
                let dest = debug_place(destination, body);
                let target_bb_name = debug_bb_name_by_idx(*target, body);

                format!("{dest} = {function_name}({args}) -> [return: {target_bb_name}]")
            }
        }
    }

    fn debug_args(args: &[crate::Operand], body: &crate::Body) -> String {
        args.iter()
            .map(|arg| debug_operand(arg, body))
            .collect::<Vec<String>>()
            .join(", ")
    }

    fn debug_ty(ty: &ResolvedType) -> String {
        match ty {
            ResolvedType::Unknown => "unknown",
            ResolvedType::Integer => "int",
            ResolvedType::String => "string",
            ResolvedType::Char => "char",
            ResolvedType::Bool => "bool",
            ResolvedType::Unit => "()",
            ResolvedType::Never => "!",
            ResolvedType::Function(_) => todo!(),
        }
        .to_string()
    }

    fn debug_bb_name_by_idx(
        basic_block_idx: crate::Idx<crate::BasicBlock>,
        body: &crate::Body,
    ) -> String {
        let basic_block = &body.blocks[basic_block_idx];
        debug_bb_name(basic_block)
    }

    fn debug_bb_name(basic_block: &crate::BasicBlock) -> String {
        basic_block.name()
    }

    #[test]
    fn test_add_number() {
        check_in_root_file(
            r#"
                fn main() -> int {
                    10 + 20
                }
            "#,
            expect![[r#"
                fn main() -> int {
                    let _0: int
                    let _1: int

                    entry: {
                        _1 = add(const 10, const 20)
                        _0 = _1
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
            "#]],
        );

        check_in_root_file(
            r#"
                fn main() -> int {
                    let a = 100;
                    let b = 10 + 20;

                    10 + 20 + a + b
                }
            "#,
            expect![[r#"
                fn main() -> int {
                    let _0: int
                    let _1: int
                    let _2: int
                    let _3: int
                    let _4: int
                    let _5: int
                    let _6: int

                    entry: {
                        _1 = const 100
                        _3 = add(const 10, const 20)
                        _2 = _3
                        _4 = add(const 10, const 20)
                        _5 = add(_4, _1)
                        _6 = add(_5, _3)
                        _0 = _6
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_sub_number() {
        check_in_root_file(
            r#"
                fn main() -> int {
                    10 - 20
                }
            "#,
            expect![[r#"
                fn main() -> int {
                    let _0: int
                    let _1: int

                    entry: {
                        _1 = sub(const 10, const 20)
                        _0 = _1
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_mul_number() {
        check_in_root_file(
            r#"
                fn main() -> int {
                    10 * 20
                }
            "#,
            expect![[r#"
                fn main() -> int {
                    let _0: int
                    let _1: int

                    entry: {
                        _1 = mul(const 10, const 20)
                        _0 = _1
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_div_number() {
        check_in_root_file(
            r#"
                fn main() -> int {
                    10 / 20
                }
            "#,
            expect![[r#"
                fn main() -> int {
                    let _0: int
                    let _1: int

                    entry: {
                        _1 = div(const 10, const 20)
                        _0 = _1
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_equal_number() {
        check_in_root_file(
            r#"
                fn main() -> bool {
                    10 == 20
                }
            "#,
            expect![[r#"
                fn main() -> bool {
                    let _0: bool
                    let _1: bool

                    entry: {
                        _1 = equal(const 10, const 20)
                        _0 = _1
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_equal_bool() {
        check_in_root_file(
            r#"
                fn main() -> bool {
                    let a = true;
                    let b = true;
                    a == b
                }
            "#,
            expect![[r#"
                fn main() -> bool {
                    let _0: bool
                    let _1: bool
                    let _2: bool
                    let _3: bool

                    entry: {
                        _1 = const true
                        _2 = const true
                        _3 = equal(_1, _2)
                        _0 = _3
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_ord_number() {
        check_in_root_file(
            r#"
                fn main() -> bool {
                    let a = 1;
                    let b = 2;
                    a < b;
                    a > b;
                    b < a;
                    b > a;
                }
            "#,
            expect![[r#"
                fn main() -> bool {
                    let _0: bool
                    let _1: int
                    let _2: int
                    let _3: bool
                    let _4: bool
                    let _5: bool
                    let _6: bool

                    entry: {
                        _1 = const 1
                        _2 = const 2
                        _3 = less_than(_1, _2)
                        _4 = greater_than(_1, _2)
                        _5 = less_than(_2, _1)
                        _6 = greater_than(_2, _1)
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_negative_number() {
        check_in_root_file(
            r#"
                fn main() -> bool {
                    let a = -10;
                    let b = 20;
                    a == -b
                }
            "#,
            expect![[r#"
                fn main() -> bool {
                    let _0: bool
                    let _1: int
                    let _2: int
                    let _3: int
                    let _4: int
                    let _5: bool

                    entry: {
                        _2 = negative(const 10)
                        _1 = _2
                        _3 = const 20
                        _4 = negative(_3)
                        _5 = equal(_2, _4)
                        _0 = _5
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_not_bool() {
        check_in_root_file(
            r#"
                fn main() -> bool {
                    let a = !true;
                    let b = false;
                    a == !b
                }
            "#,
            expect![[r#"
                fn main() -> bool {
                    let _0: bool
                    let _1: bool
                    let _2: bool
                    let _3: bool
                    let _4: bool
                    let _5: bool

                    entry: {
                        _2 = not(const true)
                        _1 = _2
                        _3 = const false
                        _4 = not(_3)
                        _5 = equal(_2, _4)
                        _0 = _5
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_main_return_int() {
        check_in_root_file(
            r#"
                fn main() -> int {
                    10
                }
            "#,
            expect![[r#"
                fn main() -> int {
                    let _0: int

                    entry: {
                        _0 = const 10
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_main_return_unit() {
        check_in_root_file(
            r#"
                fn main() {
                }
            "#,
            expect![[r#"
                fn main() -> () {
                    let _0: ()

                    entry: {
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
            "#]],
        );

        check_in_root_file(
            r#"
                fn main() {
                    10;
                }
            "#,
            expect![[r#"
                fn main() -> () {
                    let _0: ()

                    entry: {
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_main_return_bool() {
        check_in_root_file(
            r#"
                fn main() -> bool {
                    true
                }
            "#,
            expect![[r#"
                fn main() -> bool {
                    let _0: bool

                    entry: {
                        _0 = const true
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_main_return_string() {
        check_in_root_file(
            r#"
                fn main() -> string {
                    "aaa"
                }
            "#,
            expect![[r#"
                fn main() -> string {
                    let _0: string

                    entry: {
                        _0 = const "aaa"
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_let() {
        check_in_root_file(
            r#"
                fn main() -> int {
                    let x = 10;
                    x
                }
            "#,
            expect![[r#"
                fn main() -> int {
                    let _0: int
                    let _1: int

                    entry: {
                        _1 = const 10
                        _0 = _1
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_block_resolves_to_tail() {
        check_in_root_file(
            r#"
                fn main() -> int {
                    {
                        let x = 10;
                        x
                    }
                }
            "#,
            expect![[r#"
                fn main() -> int {
                    let _0: int
                    let _1: int

                    entry: {
                        _1 = const 10
                        _0 = _1
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_block_without_tail_resolves_to_unit() {
        check_in_root_file(
            r#"
                fn main() {
                    {
                        let x = 10;
                    }
                }
            "#,
            expect![[r#"
                fn main() -> () {
                    let _0: ()
                    let _1: int

                    entry: {
                        _1 = const 10
                        _0 = const ()
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_return() {
        check_in_root_file(
            r#"
                fn main() -> bool {
                    return true;
                }
            "#,
            expect![[r#"
                fn main() -> bool {
                    let _0: bool

                    entry: {
                        _0 = const true
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
            "#]],
        );

        check_in_root_file(
            r#"
                fn main() -> bool {
                    return true
                }
            "#,
            expect![[r#"
                fn main() -> bool {
                    let _0: bool

                    entry: {
                        _0 = const true
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
            "#]],
        );

        check_in_root_file(
            r#"
                fn main() -> int {
                    return 10;
                    20
                }
            "#,
            expect![[r#"
                fn main() -> int {
                    let _0: int

                    entry: {
                        _0 = const 10
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_return_in_block() {
        // return by statement in block
        check_in_root_file(
            r#"
                fn main() -> bool {
                    let c = {
                        let a = true;
                        return a;
                        let b = false;
                        b
                    };
                    c
                }
            "#,
            expect![[r#"
                fn main() -> bool {
                    let _0: bool
                    let _1: bool
                    let _2: bool

                    entry: {
                        _2 = const true
                        _0 = _2
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
            "#]],
        );

        // return by tail in block
        check_in_root_file(
            r#"
                fn main() -> int {
                    let c = {
                        return 10;
                    };
                    c
                }
            "#,
            expect![[r#"
                fn main() -> int {
                    let _0: int
                    let _1: ()

                    entry: {
                        _0 = const 10
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_return_in_switch() {
        check_in_root_file(
            r#"
                fn main() -> int {
                    if true {
                        return 10;
                    } else {
                        return 20;
                    }
                }
            "#,
            expect![[r#"
                fn main() -> int {
                    let _0: int
                    let _1: bool

                    entry: {
                        _1 = const true
                        switch(_1) -> [true: then0, false: else0]
                    }

                    exit: {
                        return _0
                    }

                    then0: {
                        _0 = const 10
                        goto -> exit
                    }

                    else0: {
                        _0 = const 20
                        goto -> exit
                    }
                }
            "#]],
        );
    }

    #[test]
    fn return_value_when_false_in_switch() {
        check_in_root_file(
            r#"
                fn main() -> int {
                    if true {
                        10
                    } else {
                        return 20;
                    }
                }
            "#,
            expect![[r#"
                fn main() -> int {
                    let _0: int
                    let _1: bool
                    let _2: int

                    entry: {
                        _1 = const true
                        switch(_1) -> [true: then0, false: else0]
                    }

                    exit: {
                        return _0
                    }

                    then0: {
                        _2 = const 10
                        goto -> bb0
                    }

                    else0: {
                        _0 = const 20
                        goto -> exit
                    }

                    bb0: {
                        _0 = _2
                        goto -> exit
                    }
                }
            "#]],
        );
    }

    #[test]
    fn return_value_when_true_in_switch() {
        check_in_root_file(
            r#"
                fn main() -> int {
                    if true {
                        return 10;
                    } else {
                        20
                    }
                }
            "#,
            expect![[r#"
                fn main() -> int {
                    let _0: int
                    let _1: bool
                    let _2: ()

                    entry: {
                        _1 = const true
                        switch(_1) -> [true: then0, false: else0]
                    }

                    exit: {
                        return _0
                    }

                    then0: {
                        _0 = const 10
                        goto -> exit
                    }

                    else0: {
                        _2 = const 20
                        goto -> bb0
                    }

                    bb0: {
                        _0 = _2
                        goto -> exit
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_switch() {
        check_in_root_file(
            r#"
                fn main() -> int {
                    if true {
                        10
                    } else {
                        20
                    }
                }
            "#,
            expect![[r#"
                fn main() -> int {
                    let _0: int
                    let _1: bool
                    let _2: int

                    entry: {
                        _1 = const true
                        switch(_1) -> [true: then0, false: else0]
                    }

                    exit: {
                        return _0
                    }

                    then0: {
                        _2 = const 10
                        goto -> bb0
                    }

                    else0: {
                        _2 = const 20
                        goto -> bb0
                    }

                    bb0: {
                        _0 = _2
                        goto -> exit
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_switch_conditional_early_return() {
        check_in_root_file(
            r#"
                fn main() -> int {
                    let a = 10;
                    if true {
                        return 20;
                    }

                    a
                }
            "#,
            expect![[r#"
                fn main() -> int {
                    let _0: int
                    let _1: int
                    let _2: bool

                    entry: {
                        _1 = const 10
                        _2 = const true
                        switch(_2) -> [true: then0, false: else0]
                    }

                    exit: {
                        return _0
                    }

                    then0: {
                        _0 = const 20
                        goto -> exit
                    }

                    else0: {
                        goto -> bb0
                    }

                    bb0: {
                        _0 = _1
                        goto -> exit
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_switch_only_then_branch() {
        check_in_root_file(
            r#"
                fn main() -> int {
                    let a = 10;
                    if true {
                        20
                    }

                    a
                }
            "#,
            expect![[r#"
                fn main() -> int {
                    let _0: int
                    let _1: int
                    let _2: bool
                    let _3: int

                    entry: {
                        _1 = const 10
                        _2 = const true
                        switch(_2) -> [true: then0, false: else0]
                    }

                    exit: {
                        return _0
                    }

                    then0: {
                        _3 = const 20
                        goto -> bb0
                    }

                    else0: {
                        _3 = const ()
                        goto -> bb0
                    }

                    bb0: {
                        _0 = _1
                        goto -> exit
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_statements_in_switch() {
        check_in_root_file(
            r#"
                fn main() -> int {
                    if true {
                        let b = 20;
                        b
                    } else {
                        let c = 20;
                        c
                    }
                }
            "#,
            expect![[r#"
                fn main() -> int {
                    let _0: int
                    let _1: bool
                    let _2: int
                    let _3: int
                    let _4: int

                    entry: {
                        _1 = const true
                        switch(_1) -> [true: then0, false: else0]
                    }

                    exit: {
                        return _0
                    }

                    then0: {
                        _2 = const 20
                        _3 = _2
                        goto -> bb0
                    }

                    else0: {
                        _4 = const 20
                        _3 = _4
                        goto -> bb0
                    }

                    bb0: {
                        _0 = _3
                        goto -> exit
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_call() {
        check_in_root_file(
            r#"
                fn aaa() -> int {
                    10
                }

                fn main() -> int {
                    aaa()
                }
            "#,
            expect![[r#"
                fn aaa() -> int {
                    let _0: int

                    entry: {
                        _0 = const 10
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
                fn main() -> int {
                    let _0: int
                    let _1: int

                    entry: {
                        _1 = aaa() -> [return: bb0]
                    }

                    exit: {
                        return _0
                    }

                    bb0: {
                        _0 = _1
                        goto -> exit
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_call_with_args() {
        check_in_root_file(
            r#"
                fn aaa(x: int, y: int) -> int {
                    x + y
                }

                fn main() -> int {
                    aaa(10, 20)
                }
            "#,
            expect![[r#"
                fn aaa(_1: int, _2: int) -> int {
                    let _0: int
                    let _3: int

                    entry: {
                        _3 = add(_1, _2)
                        _0 = _3
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
                fn main() -> int {
                    let _0: int
                    let _1: int

                    entry: {
                        _1 = aaa(const 10, const 20) -> [return: bb0]
                    }

                    exit: {
                        return _0
                    }

                    bb0: {
                        _0 = _1
                        goto -> exit
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_string_arg() {
        check_in_root_file(
            r#"
                fn aaa(x: string, y: string) -> string {
                    y
                }

                fn main() -> string {
                    aaa("aaa", "bbb")
                }
            "#,
            expect![[r#"
                fn aaa(_1: string, _2: string) -> string {
                    let _0: string

                    entry: {
                        _0 = _2
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
                fn main() -> string {
                    let _0: string
                    let _1: string

                    entry: {
                        _1 = aaa(const "aaa", const "bbb") -> [return: bb0]
                    }

                    exit: {
                        return _0
                    }

                    bb0: {
                        _0 = _1
                        goto -> exit
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_call_binding() {
        check_in_root_file(
            r#"
                fn aaa(x: int, y: int) -> int {
                    x + y
                }

                fn main() -> int {
                    let a = 10;
                    let b = 20;
                    let c = aaa(a, b);
                    c
                }
            "#,
            expect![[r#"
                fn aaa(_1: int, _2: int) -> int {
                    let _0: int
                    let _3: int

                    entry: {
                        _3 = add(_1, _2)
                        _0 = _3
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
                fn main() -> int {
                    let _0: int
                    let _1: int
                    let _2: int
                    let _3: int
                    let _4: int

                    entry: {
                        _1 = const 10
                        _2 = const 20
                        _4 = aaa(_1, _2) -> [return: bb0]
                    }

                    exit: {
                        return _0
                    }

                    bb0: {
                        _3 = _4
                        _0 = _3
                        goto -> exit
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_modules() {
        check_in_root_file(
            r#"
                fn main() {
                    return;
                }
                mod module_aaa {
                    mod module_bbb {
                        fn function_aaa() -> int {
                            mod module_ccc {
                                fn function_bbb() -> int {
                                    10
                                }
                            }

                            20
                        }
                    }

                    fn function_ccc() -> int {
                        30
                    }
                }
            "#,
            expect![[r#"
                fn main() -> () {
                    let _0: ()

                    entry: {
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
                fn module_aaa::module_bbb::function_aaa() -> int {
                    let _0: int

                    entry: {
                        _0 = const 20
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
                fn module_aaa::module_bbb::function_aaa::module_ccc::function_bbb() -> int {
                    let _0: int

                    entry: {
                        _0 = const 10
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
                fn module_aaa::function_ccc() -> int {
                    let _0: int

                    entry: {
                        _0 = const 30
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
            "#]],
        );
    }
}
