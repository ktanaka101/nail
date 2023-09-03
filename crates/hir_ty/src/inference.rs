use std::collections::HashMap;

use la_arena::{Arena, Idx};

pub fn infer_pods(db: &dyn hir::HirMasterDatabase, pods: &hir::Pods) -> InferenceResult {
    // TODO: 全てのPodを走査する
    let pod = &pods.pods[0];

    // 依存関係を気にしなくていいようにシグネチャを先に解決しておく
    let mut signatures = Arena::<Signature>::new();
    let mut signature_by_function = HashMap::<hir::Function, Idx<Signature>>::new();

    let functions = pod.all_functions(db);
    for (hir_file, function) in functions.clone() {
        let mut params = vec![];
        for param in function.params(db) {
            let ty = infer_ty(&param.data(hir_file.db(db)).ty);
            params.push(ty);
        }

        let signature = Signature {
            params,
            return_type: infer_ty(&function.return_type(db)),
        };
        let signature_idx = signatures.alloc(signature);
        signature_by_function.insert(function, signature_idx);
    }

    // 各関数内を型推論する
    let mut inference_by_body = HashMap::<hir::Function, InferenceBodyResult>::new();
    for (hir_file, function) in functions {
        let type_inferencer = TypeInferencer::new(db, hir_file, &pods.resolution_map, function);
        let inference_result = type_inferencer.infer();
        inference_by_body.insert(function, inference_result);
    }

    InferenceResult {
        inference_by_body,
        signatures,
        signature_by_function,
    }
}

fn infer_ty(ty: &hir::Type) -> ResolvedType {
    match ty {
        hir::Type::Unknown => ResolvedType::Unknown,
        hir::Type::Integer => ResolvedType::Integer,
        hir::Type::String => ResolvedType::String,
        hir::Type::Char => ResolvedType::Char,
        hir::Type::Boolean => ResolvedType::Bool,
        hir::Type::Unit => ResolvedType::Unit,
    }
}

/// 型推論の結果
///
/// 関数の引数と戻り値は必ず確定しているため、それより広い範囲で型推論を行う必要はありません。
/// そのため、関数単位に結果を持ちます。
#[derive(Debug)]
pub struct InferenceBodyResult {
    /// 関数内の式に対応する型
    pub type_by_expr: HashMap<hir::ExprId, ResolvedType>,
    /// 型推論中に発生したエラー
    pub errors: Vec<InferenceError>,
}

/// 型推論の結果
#[derive(Debug)]
pub struct InferenceResult {
    /// 関数に対応する型推論結果
    pub inference_by_body: HashMap<hir::Function, InferenceBodyResult>,
    /// 関数シグネチャ一覧
    pub signatures: Arena<Signature>,
    /// 関数に対応するシグネチャ
    pub signature_by_function: HashMap<hir::Function, Idx<Signature>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct InferenceContext {
    type_by_expr: HashMap<hir::ExprId, ResolvedType>,
    type_by_param: HashMap<hir::Param, ResolvedType>,
    signatures: Arena<Signature>,
    signature_by_function: HashMap<hir::Function, Idx<Signature>>,
    errors: Vec<InferenceError>,
}

impl InferenceContext {
    fn new() -> Self {
        Self {
            type_by_expr: HashMap::new(),
            type_by_param: HashMap::new(),
            signatures: Arena::new(),
            signature_by_function: HashMap::new(),
            errors: Vec::new(),
        }
    }
}

/// 型推論中に発生したエラー
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InferenceError {}

/// 解決後の型
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ResolvedType {
    /// 未解決の型
    Unknown,
    /// 数値型
    Integer,
    /// 文字列型
    String,
    /// 文字型
    Char,
    /// 真偽値型
    Bool,
    /// 単一値型
    Unit,
    /// 値を取り得ないことを表す型
    ///
    /// 例えば、必ず`panic`を起こす関数の型は`Never`です。
    Never,
    /// 関数型
    #[allow(dead_code)]
    Function(Idx<Signature>),
}

/// 関数シグネチャ
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Signature {
    /// パラメータの型一覧
    ///
    /// パラメータの順番に対応しています。
    pub params: Vec<ResolvedType>,
    /// 戻り値の型
    pub return_type: ResolvedType,
}

/// 型推論器
struct TypeInferencer<'a> {
    db: &'a dyn hir::HirMasterDatabase,
    hir_file: hir::HirFile,
    resolution_map: &'a hir::ResolutionMap,
    ctx: InferenceContext,
    function: hir::Function,
}
impl<'a> TypeInferencer<'a> {
    fn new(
        db: &'a dyn hir::HirMasterDatabase,
        hir_file: hir::HirFile,
        symbol_table: &'a hir::ResolutionMap,
        function: hir::Function,
    ) -> Self {
        Self {
            db,
            hir_file,
            resolution_map: symbol_table,
            ctx: InferenceContext::new(),
            function,
        }
    }

    fn infer(mut self) -> InferenceBodyResult {
        let body_ast_id = self.function.ast(self.db).body().unwrap();
        let body = self
            .hir_file
            .db(self.db)
            .function_body_by_ast_block(body_ast_id)
            .unwrap();
        match body {
            hir::Expr::Block(block) => self.infer_block(block),
            _ => unreachable!(),
        };

        InferenceBodyResult {
            type_by_expr: self.ctx.type_by_expr,
            errors: self.ctx.errors,
        }
    }

    fn infer_stmts(&mut self, stmts: &[hir::Stmt]) {
        for stmt in stmts {
            match stmt {
                hir::Stmt::ExprStmt { expr, .. } => {
                    let ty = self.infer_expr_id(*expr);
                    self.ctx.type_by_expr.insert(*expr, ty);
                }
                hir::Stmt::VariableDef { value, .. } => {
                    let ty = self.infer_expr_id(*value);
                    self.ctx.type_by_expr.insert(*value, ty);
                }
                hir::Stmt::Item { .. } => (),
            }
        }
    }

    fn infer_expr(&mut self, expr: &hir::Expr) -> ResolvedType {
        match expr {
            hir::Expr::Symbol(symbol) => match symbol {
                hir::Symbol::Local { expr, .. } => self.infer_expr_id(*expr),
                hir::Symbol::Param { param, .. } => {
                    infer_ty(&param.data(self.hir_file.db(self.db)).ty)
                }
                // TODO: supports function, name resolution
                hir::Symbol::Missing { path: _ } => ResolvedType::Unknown,
            },
            hir::Expr::Literal(literal) => match literal {
                hir::Literal::Integer(_) => ResolvedType::Integer,
                hir::Literal::String(_) => ResolvedType::String,
                hir::Literal::Char(_) => ResolvedType::Char,
                hir::Literal::Bool(_) => ResolvedType::Bool,
            },
            hir::Expr::Binary { op, lhs, rhs } => {
                // TODO: supports string equal
                let lhs_ty = self.infer_expr_id(*lhs);
                let rhs_ty = self.infer_expr_id(*rhs);

                match op {
                    ast::BinaryOp::Add(_)
                    | ast::BinaryOp::Sub(_)
                    | ast::BinaryOp::Mul(_)
                    | ast::BinaryOp::Div(_) => {
                        if rhs_ty == lhs_ty && (matches!(rhs_ty, ResolvedType::Integer)) {
                            return rhs_ty;
                        }
                    }
                    ast::BinaryOp::Equal(_) => {
                        if rhs_ty == lhs_ty
                            && (matches!(rhs_ty, ResolvedType::Integer | ResolvedType::Bool))
                        {
                            return ResolvedType::Bool;
                        }
                    }
                    ast::BinaryOp::GreaterThan(_) | ast::BinaryOp::LessThan(_) => {
                        if rhs_ty == lhs_ty && (matches!(rhs_ty, ResolvedType::Integer)) {
                            return ResolvedType::Bool;
                        }
                    }
                }

                match (lhs_ty, rhs_ty) {
                    (ty, ResolvedType::Unknown) => {
                        self.ctx.type_by_expr.insert(*rhs, ty);
                        ty
                    }
                    (ResolvedType::Unknown, ty) => {
                        self.ctx.type_by_expr.insert(*lhs, ty);
                        ty
                    }
                    (_, _) => ResolvedType::Unknown,
                }
            }
            hir::Expr::Unary { op, expr } => {
                let expr_ty = self.infer_expr_id(*expr);
                match op {
                    ast::UnaryOp::Neg(_) => {
                        if expr_ty == ResolvedType::Integer {
                            return ResolvedType::Integer;
                        }
                    }
                    ast::UnaryOp::Not(_) => {
                        if expr_ty == ResolvedType::Bool {
                            return ResolvedType::Bool;
                        }
                    }
                }

                ResolvedType::Unknown
            }
            hir::Expr::Call { callee, args } => match callee {
                hir::Symbol::Missing { path } => {
                    let Some(resolution_status) = self.resolution_map.item_by_symbol(path) else { return ResolvedType::Unknown; };

                    let resolved_item = match resolution_status {
                        hir::ResolutionStatus::Resolved { path: _, item } => item,
                        hir::ResolutionStatus::Unresolved | hir::ResolutionStatus::Error => {
                            return ResolvedType::Unknown;
                        }
                    };

                    match resolved_item {
                        hir::Item::Function(function) => {
                            for (i, arg) in args.iter().enumerate() {
                                let param = function.params(self.db)[i];

                                let arg_ty = self.infer_expr_id(*arg);
                                let param_ty = infer_ty(&param.data(self.hir_file.db(self.db)).ty);

                                if arg_ty == param_ty {
                                    continue;
                                }

                                match (arg_ty, param_ty) {
                                    (ResolvedType::Unknown, ResolvedType::Unknown) => (),
                                    (ResolvedType::Unknown, ty) => {
                                        self.ctx.type_by_expr.insert(*arg, ty);
                                    }
                                    (_, _) => (),
                                }
                            }

                            infer_ty(&function.return_type(self.db))
                        }
                        hir::Item::Module(_) => unimplemented!(),
                        hir::Item::UseItem(_) => unimplemented!(),
                    }
                }
                hir::Symbol::Local { .. } | hir::Symbol::Param { .. } => unimplemented!(),
            },
            hir::Expr::Block(block) => self.infer_block(block),
            hir::Expr::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.infer_expr_id(*condition);
                let then_branch_ty = self.infer_expr_id(*then_branch);
                let else_branch_ty = if let Some(else_branch) = else_branch {
                    self.infer_expr_id(*else_branch)
                } else {
                    ResolvedType::Unit
                };

                match (then_branch_ty, else_branch_ty) {
                    (ResolvedType::Unknown, ResolvedType::Unknown) => ResolvedType::Unknown,
                    (ResolvedType::Unknown, ty) => {
                        self.ctx.type_by_expr.insert(*then_branch, ty);
                        ty
                    }
                    (ty, ResolvedType::Unknown) => {
                        self.ctx.type_by_expr.insert(else_branch.unwrap(), ty);
                        ty
                    }
                    (ty_a, ty_b) if ty_a == ty_b => ty_a,
                    (_, _) => ResolvedType::Unknown,
                }
            }
            hir::Expr::Return { value } => {
                if let Some(value) = value {
                    self.infer_expr_id(*value);
                }
                ResolvedType::Never
            }
            hir::Expr::Missing => ResolvedType::Unknown,
        }
    }

    fn infer_block(&mut self, block: &hir::Block) -> ResolvedType {
        self.infer_stmts(&block.stmts);
        if let Some(tail) = block.tail {
            self.infer_expr_id(tail)
        } else {
            ResolvedType::Unit
        }
    }

    fn infer_expr_id(&mut self, expr_id: hir::ExprId) -> ResolvedType {
        if let Some(ty) = self.lookup_type(expr_id) {
            return ty;
        }

        let ty = self.infer_expr(expr_id.lookup(self.hir_file.db(self.db)));
        self.ctx.type_by_expr.insert(expr_id, ty);

        ty
    }

    fn lookup_type(&self, expr_id: hir::ExprId) -> Option<ResolvedType> {
        self.ctx.type_by_expr.get(&expr_id).copied()
    }
}
