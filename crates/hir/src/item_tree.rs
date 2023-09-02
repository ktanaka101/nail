mod item;

use std::collections::HashMap;

pub use item::{Function, Item, Module, ModuleKind, Param, Type, UseItem};

use crate::{NailFile, Name, Path};

/// `ItemTree`は、ファイル内のすべてのアイテムをツリー構造で保持するデータ構造です。
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ItemTree {
    /// ファイル
    pub file: NailFile,
    /// ItemTree内の関数一覧
    functions: Vec<Function>,
    /// ItemTree内のモジュール一覧
    modules: Vec<Module>,
    /// ItemTree内の使用宣言一覧
    use_items: Vec<UseItem>,

    /// ブロック(AST)に対応する関数
    function_by_block: HashMap<ast::BlockExpr, Function>,
    /// 関数に対応するブロック(AST)
    block_by_function: HashMap<Function, ast::BlockExpr>,

    /// モジュール(AST)に対応するモジュールID
    module_by_ast_module: HashMap<ast::Module, Module>,

    /// 使用宣言(AST)に対応する使用宣言ID
    use_item_by_ast_use: HashMap<ast::Use, UseItem>,
}
impl ItemTree {
    /// ItemTree内の関数一覧を返す
    pub fn functions(&self) -> &[Function] {
        &self.functions
    }

    /// ItemTree内の関数一覧を返す
    pub fn modules(&self) -> &[Module] {
        &self.modules
    }

    /// ブロック(AST)に対応する関数を取得します。
    /// 存在しない場合は`None`を返します。
    pub fn function_by_ast_block(&self, ast_block: &ast::BlockExpr) -> Option<Function> {
        self.function_by_block.get(ast_block).copied()
    }

    /// 関数に対応するブロック(AST)IDを取得します。
    /// 存在しない場合は`None`を返します。
    pub fn block_id_by_function(&self, function: &Function) -> Option<ast::BlockExpr> {
        Some(self.block_by_function.get(function)?.clone())
    }

    /// モジュール(AST)に対応するモジュールを取得します。
    /// 存在しない場合は`None`を返します。
    pub fn module_by_ast_module(&self, ast_module: ast::Module) -> Option<Module> {
        self.module_by_ast_module.get(&ast_module).copied()
    }

    /// 使用宣言(AST)に対応する使用宣言IDを取得します。
    /// 存在しない場合は`None`を返します。
    pub fn use_item_by_ast_use(&self, use_ast_id: ast::Use) -> Option<UseItem> {
        self.use_item_by_ast_use.get(&use_ast_id).copied()
    }
}

/// アイテムツリー構築用コンテキスト
pub(crate) struct ItemTreeBuilderContext {
    file: NailFile,

    functions: Vec<Function>,
    modules: Vec<Module>,
    use_items: Vec<UseItem>,

    function_by_block: HashMap<ast::BlockExpr, Function>,
    block_by_function: HashMap<Function, ast::BlockExpr>,

    module_by_ast_module: HashMap<ast::Module, Module>,

    use_item_by_ast_use: HashMap<ast::Use, UseItem>,
}
impl ItemTreeBuilderContext {
    /// コンテキストを作成します。
    pub(crate) fn new(file: NailFile) -> Self {
        Self {
            file,
            functions: vec![],
            modules: vec![],
            use_items: vec![],
            function_by_block: HashMap::new(),
            block_by_function: HashMap::new(),
            module_by_ast_module: HashMap::new(),
            use_item_by_ast_use: HashMap::new(),
        }
    }

    /// アイテムツリーを構築します。
    pub(crate) fn build(mut self, salsa_db: &dyn crate::Db, ast: &ast::SourceFile) -> ItemTree {
        for item in ast.items() {
            self.build_item(salsa_db, item);
        }

        ItemTree {
            file: self.file,
            functions: self.functions,
            modules: self.modules,
            use_items: self.use_items,
            function_by_block: self.function_by_block,
            block_by_function: self.block_by_function,
            module_by_ast_module: self.module_by_ast_module,
            use_item_by_ast_use: self.use_item_by_ast_use,
        }
    }

    /// アイテムを構築します。
    /// アイテムが構築できなかった場合は`None`を返します。
    fn build_item(&mut self, salsa_db: &dyn crate::Db, item: ast::Item) -> Option<Item> {
        match item {
            ast::Item::FunctionDef(def) => {
                let ast_block = def.body()?;
                let params = def
                    .params()?
                    .params()
                    .enumerate()
                    .map(|(pos, param)| {
                        let name = param
                            .name()
                            .map(|name| Name::new(salsa_db, name.name().to_string()));
                        let ty = self.lower_ty(param.ty());
                        Param::new(salsa_db, name, ty, pos)
                    })
                    .collect::<Vec<_>>();
                let param_by_name = params
                    .iter()
                    .filter_map(|param| param.name(salsa_db).map(|name| (name, *param)))
                    .collect::<HashMap<_, _>>();
                let return_type = if let Some(return_type) = def.return_type() {
                    self.lower_ty(return_type.ty())
                } else {
                    Type::Unit
                };

                let name = def
                    .name()
                    .map(|name| Name::new(salsa_db, name.name().to_string()));

                let function =
                    Function::new(salsa_db, name, params, param_by_name, return_type, def);
                self.functions.push(function);

                self.build_block(salsa_db, ast_block.clone());
                self.function_by_block.insert(ast_block.clone(), function);
                self.block_by_function.insert(function, ast_block);

                Some(Item::Function(function))
            }
            ast::Item::Module(ast_module) => {
                let module = self.build_module(salsa_db, ast_module)?;
                Some(Item::Module(module))
            }
            ast::Item::Use(ast_use) => {
                let use_item = self.build_use(salsa_db, ast_use)?;
                Some(Item::UseItem(use_item))
            }
        }
    }

    /// ステートメントを構築します。
    /// ステートメントが構築できなかった場合は`None`を返します。
    fn build_stmt(&mut self, salsa_db: &dyn crate::Db, stmt: ast::Stmt) -> Option<()> {
        match stmt {
            ast::Stmt::ExprStmt(expr_stmt) => self.build_expr(salsa_db, expr_stmt.expr()?)?,
            ast::Stmt::VariableDef(def) => self.build_expr(salsa_db, def.value()?)?,
            ast::Stmt::Item(item) => {
                self.build_item(salsa_db, item)?;
            }
        }

        Some(())
    }

    /// 型を構築します。
    /// 型を構築できなかった場合は`Type::Unknown`を返します。
    fn lower_ty(&mut self, ty: Option<ast::Type>) -> Type {
        let ident = match ty.and_then(|ty| ty.ty()) {
            Some(ident) => ident,
            None => return Type::Unknown,
        };

        let type_name = ident.name();
        match type_name {
            "int" => Type::Integer,
            "string" => Type::String,
            "char" => Type::Char,
            "bool" => Type::Boolean,
            _ => Type::Unknown,
        }
    }

    /// 式を構築します。
    /// 式自体はアイテムではないため、値を返すわけではありません。
    /// 式の中のアイテムを探すためにトラバースしています。
    /// 式の中のアイテムを構築できなかった場合は`None`を返します。
    /// 式の中にアイテムがあるのは、例えばブロック自体やIf式です。
    /// ```nail
    /// if true {
    ///     fn a() -> int { // Item
    ///         10
    ///     }
    /// }
    /// ```
    fn build_expr(&mut self, salsa_db: &dyn crate::Db, expr: ast::Expr) -> Option<()> {
        match expr {
            ast::Expr::BinaryExpr(binary) => {
                self.build_expr(salsa_db, binary.lhs()?)?;
                self.build_expr(salsa_db, binary.rhs()?)?;
            }
            ast::Expr::ParenExpr(paren) => {
                self.build_expr(salsa_db, paren.expr()?)?;
            }
            ast::Expr::UnaryExpr(unary) => {
                self.build_expr(salsa_db, unary.expr()?)?;
            }
            ast::Expr::BlockExpr(block) => {
                self.build_block(salsa_db, block);
            }
            ast::Expr::IfExpr(if_expr) => {
                self.build_expr(salsa_db, if_expr.condition()?)?;
                self.build_block(salsa_db, if_expr.then_branch()?);
                self.build_block(salsa_db, if_expr.else_branch()?);
            }
            _ => (),
        };

        Some(())
    }

    /// ブロックを構築します。
    fn build_block(&mut self, salsa_db: &dyn crate::Db, ast_block: ast::BlockExpr) {
        for stmt in ast_block.stmts() {
            self.build_stmt(salsa_db, stmt);
        }
    }

    /// モジュールを構築します。
    /// モジュールを構築できなかった場合は`None`を返します。
    fn build_module(
        &mut self,
        salsa_db: &dyn crate::Db,
        ast_module: ast::Module,
    ) -> Option<Module> {
        if let Some(name) = ast_module.name() {
            let module_name = Name::new(salsa_db, name.name().to_string());

            let module_kind = if let Some(item_list) = ast_module.items() {
                let mut items = vec![];
                for item in item_list.items() {
                    if let Some(item) = self.build_item(salsa_db, item) {
                        items.push(item);
                    }
                }
                ModuleKind::Inline { items }
            } else {
                ModuleKind::Outline
            };
            let module = Module::new(salsa_db, module_name, module_kind);

            self.modules.push(module);
            self.module_by_ast_module.insert(ast_module, module);

            Some(module)
        } else {
            None
        }
    }

    /// アイテムを構築します。
    /// アイテムを構築できなかった場合は`None`を返します。
    fn build_use(&mut self, salsa_db: &dyn crate::Db, ast_use: ast::Use) -> Option<UseItem> {
        let use_path = ast_use.path()?.segments().collect::<Vec<_>>();
        match use_path.as_slice() {
            [] => unreachable!("use path should not be empty"),
            [path @ .., name] => {
                let name = Name::new(salsa_db, name.name().unwrap().name().to_string());
                let segments = path
                    .iter()
                    .map(|segment| Name::new(salsa_db, segment.name().unwrap().name().to_string()))
                    .collect::<Vec<_>>();
                let path = Path { segments };
                let use_item = UseItem::new(salsa_db, name, path);

                self.use_items.push(use_item);
                self.use_item_by_ast_use.insert(ast_use, use_item);

                Some(use_item)
            }
        }
    }
}
