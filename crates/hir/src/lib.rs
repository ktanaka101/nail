//! HIRを構築するためのモジュールです。
//!
//! HIRは、ASTをより解析しやすい形に変換したものです。
//! 具体的には、以下を行います。
//! - セマンティクス解析
//! - 名前解決
//!   ローカル変数、関数、モジュールなどの名前を解決します。
//!   名前解決のために、スコープ管理やアイテムツリーの構築も行います。
//!
//! 以下はHIR時点では行いません。
//! - 型推論
//!   Typed-HIRで行います。
//! - 型チェック
//!   Typed-HIRで行います。
//! - 脱糖
//!   MIRで行います。
//!
//! HIRの構築は、以下の手順で行います。
//! 1. ルートファイルをパースする
//! 2. ルートファイルで出現したアウトラインモジュールのファイルをパースする
//! 3. モジュールツリーを構築する
//!   モジュールツリーは、`aaa::bbb::ccc`のようなパスを名前解決するためのものです。
//! 4. 使用宣言/関数/モジュールの名前解決を行う

// #[salsa::tracked]で生成される関数にドキュメントコメントが作成されないため警告が出てしまうため許可します。
// #![warn(missing_docs)]

mod body;
mod db;
mod input;
mod item;
mod name_resolver;
mod testing;

use std::{collections::HashMap, marker::PhantomData};

use ast::AstNode;
pub use body::{BodyLower, ExprId, FunctionBodyId, HirFileDatabase};
pub use db::{HirMasterDatabase, Jar};
pub use input::{FixtureDatabase, NailFile, SourceDatabase, SourceDatabaseTrait};
pub use item::{Function, Item, Module, ModuleKind, Param, Type, UseItem};
use name_resolver::resolve_symbols;
pub use name_resolver::{ModuleScopeOrigin, ResolutionMap, ResolutionStatus};
pub use testing::TestingDatabase;

/// ビルド対象全体を表します。
#[derive(Debug)]
pub struct Pods {
    /// ビルド対象のルートPod
    pub root_pod: Pod,

    /// 名前解決の結果
    pub resolution_map: ResolutionMap,
}

/// PodはNailにおけるパッケージの単位です。
#[derive(Debug)]
pub struct Pod {
    /// Pod名
    pub name: Name,

    /// ルートファイルID
    pub root_nail_file: NailFile,

    /// ルートファイルのHIR構築結果
    pub root_hir_file: HirFile,

    /// ルートファイルのソースコードとHIRのマッピング
    pub root_source_map: HirFileSourceMap,

    /// ファイル別のHIR構築結果
    ///
    /// ルートファイルは含まれません。
    hir_file_by_nail_file: HashMap<NailFile, HirFile>,

    /// ファイル別のソースコードとHIRのマッピング
    ///
    /// ルートファイルも含まれます。
    pub source_map_by_nail_file: HashMap<NailFile, HirFileSourceMap>,

    /// 関数別ののHIR構築結果
    ///
    /// ルートファイルのHIR構築結果
    hir_file_by_function: HashMap<Function, HirFile>,

    /// モジュール別のHIR構築結果
    ///
    /// ルートファイルのHIR構築結果
    hir_file_by_module: HashMap<Module, HirFile>,

    /// ファイルの登録順
    ///
    /// ルートファイルは含まれません。
    registration_order: Vec<NailFile>,
}
impl Pod {
    /// 指定したファイルのHIR構築結果を返します。
    ///
    /// ルートファイルのHIR構築結果は`root_lower_result`で参照してください。
    /// この関数はルートファイルを指定されても`None`を返します。
    pub fn get_hir_file_by_file(&self, file: NailFile) -> Option<&HirFile> {
        self.hir_file_by_nail_file.get(&file)
    }

    /// ファイルの登録順の昇順でHIR構築結果を返します。
    ///
    /// ルートファイルは含まれません。
    pub fn get_hir_files_order_registration_asc(&self) -> Vec<(NailFile, &HirFile)> {
        let mut lower_results = vec![];
        for file in &self.registration_order {
            lower_results.push((*file, self.hir_file_by_nail_file.get(file).unwrap()));
        }

        lower_results
    }

    /// 指定した関数のHIR構築結果を返します。
    pub fn get_hir_file_by_function(&self, function: Function) -> Option<&HirFile> {
        self.hir_file_by_function.get(&function)
    }

    /// モジュールからHIR構築結果を返します。
    pub fn get_hir_file_by_module(&self, module: &Module) -> Option<HirFile> {
        self.hir_file_by_module.get(module).copied()
    }

    /// Pod内の関数を全て返します。
    pub fn all_functions(&self, db: &dyn HirMasterDatabase) -> Vec<(HirFile, Function)> {
        let mut functions = vec![];

        functions.append(
            &mut self
                .root_hir_file
                .functions(db)
                .iter()
                .map(|function| (self.root_hir_file, *function))
                .collect::<Vec<_>>(),
        );

        for nail_file in &self.registration_order {
            let hir_file = self.get_hir_file_by_file(*nail_file).unwrap();
            functions.append(
                &mut hir_file
                    .functions(db)
                    .iter()
                    .map(|function| (*hir_file, *function))
                    .collect::<Vec<_>>(),
            );
        }

        functions
    }

    /// Nailファイルを元にソースマップを返します。
    pub fn source_map_by_function(
        &self,
        db: &dyn HirMasterDatabase,
        function: Function,
    ) -> Option<&HirFileSourceMap> {
        let hir_file = self.hir_file_by_function.get(&function)?;
        self.source_map_by_nail_file.get(&hir_file.file(db))
    }
}

/// ルートファイルをパースし、Pod全体を構築します。
pub fn parse_pods(db: &dyn HirMasterDatabase, source_db: &mut dyn SourceDatabaseTrait) -> Pods {
    let pod = PodBuilder::new(db, source_db).build();
    let symbol_table = resolve_symbols(db, &pod);

    Pods {
        root_pod: pod,
        resolution_map: symbol_table,
    }
}

struct PodBuilder<'a> {
    db: &'a dyn HirMasterDatabase,

    source_db: &'a mut dyn SourceDatabaseTrait,

    /// ファイル別のHIR構築結果
    ///
    /// ルートファイルは含まれません。
    hir_file_by_nail_file: HashMap<NailFile, HirFile>,

    /// ファイル別のソースコードとHIRのマッピング
    ///
    /// ルートファイルも含まれます。
    source_map_by_nail_file: HashMap<NailFile, HirFileSourceMap>,

    /// 関数別ののHIR構築結果
    ///
    /// ルートファイルのHIR構築結果
    hir_file_by_function: HashMap<Function, HirFile>,

    /// モジュール別のHIR構築結果
    ///
    /// ルートファイルのHIR構築結果
    hir_file_by_module: HashMap<Module, HirFile>,

    /// ファイルの登録順
    ///
    /// ルートファイルは含まれません。
    registration_order: Vec<NailFile>,
}
impl<'a> PodBuilder<'a> {
    fn new(
        db: &'a dyn HirMasterDatabase,
        source_db: &'a mut dyn SourceDatabaseTrait,
    ) -> PodBuilder<'a> {
        PodBuilder {
            db,
            source_db,
            hir_file_by_nail_file: HashMap::new(),
            source_map_by_nail_file: HashMap::new(),
            hir_file_by_function: HashMap::new(),
            hir_file_by_module: HashMap::new(),
            registration_order: vec![],
        }
    }

    /// ルートファイル、サブファイルをパースし、Podを構築します。
    fn build(mut self) -> Pod {
        let root_nail_file = self.source_db.source_root();
        let root_file_path = root_nail_file.file_path(self.db);

        let ast_source = parse_to_ast(self.db, root_nail_file);
        let (root_hir_file, root_source_map) = build_hir_file(self.db, ast_source);
        for function in root_hir_file.functions(self.db) {
            self.hir_file_by_function.insert(*function, root_hir_file);
        }
        self.source_map_by_nail_file
            .insert(root_nail_file, root_source_map);

        let root_dir = root_file_path.parent().unwrap();
        for sub_module in root_hir_file.modules(self.db) {
            if matches!(sub_module.kind(self.db), ModuleKind::Inline { .. }) {
                continue;
            }

            self.parse_module(self.db, *sub_module, root_dir);
        }

        Pod {
            name: Name::new(self.db, "t_pod".to_string()),
            root_nail_file,
            root_hir_file,
            root_source_map,

            hir_file_by_nail_file: self.hir_file_by_nail_file,
            hir_file_by_function: self.hir_file_by_function,
            hir_file_by_module: self.hir_file_by_module,
            source_map_by_nail_file: self.source_map_by_nail_file,
            registration_order: self.registration_order,
        }
    }

    /// モジュールのパースを行います。
    ///
    /// モジュールのパースは、再帰的に行われます。
    /// 例えば、`aaa::bbb::ccc`というモジュールがあった場合、`aaa`、`bbb`、`ccc`の順でパースが行われます。
    ///
    /// `dir`には、パース対象モジュールのディレクトリを指定します。
    /// そのディレクトリの`module`の名前がパースされます。
    /// `dir`に`aaa`を指定した場合、`aaa.nail`がパースされます。
    /// そのファイル内のアウトラインモジュールがあれば、`aaa/`配下のファイルをパースしていきます。
    /// 例えば、アウトラインモジュール名が`bbb`の場合、`aaa/bbb.nail`がパースされます。
    fn parse_module(&mut self, db: &dyn HirMasterDatabase, module: Module, dir: &std::path::Path) {
        let module_name = module.name(db).text(db);
        let nail_file_path = dir.join(format!("{module_name}.nail"));
        let nail_file = self.source_db.get_file(&nail_file_path).expect("todo");

        let ast_source = parse_to_ast(db, nail_file);
        let (hir_file, source_map) = build_hir_file(db, ast_source);

        for function in hir_file.functions(db) {
            self.hir_file_by_function.insert(*function, hir_file);
        }
        self.hir_file_by_module.insert(module, hir_file);
        self.registration_order.push(hir_file.file(db));
        self.hir_file_by_nail_file
            .insert(hir_file.file(db), hir_file);
        self.source_map_by_nail_file
            .insert(hir_file.file(db), source_map);

        let sub_dir = dir.join(module_name);
        for sub_module in hir_file.modules(db) {
            if matches!(sub_module.kind(db), ModuleKind::Inline { .. }) {
                continue;
            }

            self.parse_module(db, *sub_module, &sub_dir);
        }
    }
}

/// HIR構築時のエラー
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LowerError {
    /// エントリーポイントが見つからない場合
    UndefinedEntryPoint,
}

/// ファイル単位のソースコードとHIRのマッピング
#[salsa::tracked]
pub struct HirFileSourceMap {
    /// ファイル
    ///
    /// 1ファイルにつき1つの[HirFileSourceMap]が生成されます。
    pub file: NailFile,

    /// 式とソースコードのマッピング
    pub source_by_expr: HashMap<ExprId, ExprSource>,

    /// 式とソースコードのマッピング
    pub source_by_function: HashMap<Function, FunctionSource>,
}

/// ASTノードのIDです。
///
/// 1ファイル内でユニークです。
/// IDからASTを参照し、ファイル内における位置を取得することができます。
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AstPtr<T: AstNode> {
    /// ASTノード
    pub node: ast::SyntaxNodePtr,
    _ty: PhantomData<T>,
}
/// 式のAST位置です。
pub type ExprSource = InFile<AstPtr<ast::Expr>>;
/// 関数定義のAST位置です。
pub type FunctionSource = InFile<ast::FunctionDef>;

/// 型引数をファイル内で一意として表現します。
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InFile<T> {
    pub file: NailFile,
    pub value: T,
}

/// ファイル単位のHIR構築結果
#[salsa::tracked]
pub struct HirFile {
    /// ファイル
    ///
    /// 1ファイルにつき1つの[HirFile]が生成されます。
    pub file: NailFile,

    /// ボディ構築時に共有されるコンテキスト
    ///
    /// 1ファイル内のコンテキストです。
    #[return_ref]
    pub db: HirFileDatabase,
    /// トップレベルのアイテム一覧
    #[return_ref]
    pub top_level_items: Vec<Item>,
    /// エントリーポイントの関数
    pub entry_point: Option<Function>,
    /// エラー一覧
    #[return_ref]
    pub errors: Vec<LowerError>,
}
impl HirFile {
    /// 関数IDから関数ボディを取得します。
    pub fn function_body_by_function<'a>(
        &self,
        db: &'a dyn HirMasterDatabase,
        function: Function,
    ) -> Option<&'a Expr> {
        self.db(db)
            .function_body_by_ast_block(function.ast(db).body()?)
    }

    /// ファイル内の関数一覧を返します。
    pub fn functions<'a>(&self, db: &'a dyn HirMasterDatabase) -> &'a [Function] {
        self.db(db).functions()
    }

    /// ファイル内のモジュール一覧を返します。
    pub fn modules<'a>(&self, db: &'a dyn HirMasterDatabase) -> &'a [Module] {
        self.db(db).modules()
    }
}

#[salsa::tracked]
pub struct AstSourceFile {
    /** Nailファイル */
    pub file: NailFile,

    /** AST */
    #[return_ref]
    pub source: ast::SourceFile,
}

/// ファイルを元にASTを構築します。
#[salsa::tracked]
pub fn parse_to_ast(db: &dyn HirMasterDatabase, nail_file: NailFile) -> AstSourceFile {
    let ast = parser::parse(nail_file.contents(db));
    let ast_source_file = ast::SourceFile::cast(ast.syntax()).unwrap();
    AstSourceFile::new(db, nail_file, ast_source_file)
}

/// ASTを元に[HirFile]を構築します。
#[salsa::tracked]
fn build_hir_file(
    db: &dyn HirMasterDatabase,
    ast_source: AstSourceFile,
) -> (HirFile, HirFileSourceMap) {
    let file = ast_source.file(db);
    let source_file = ast_source.source(db);

    let mut hir_file_db = HirFileDatabase::new();

    let mut root_file_body = BodyLower::new(file, HashMap::new(), &mut hir_file_db);
    let top_level_items = source_file
        .items()
        .filter_map(|item| root_file_body.lower_toplevel(db, item))
        .collect::<Vec<_>>();

    let (errors, entry_point) = if ast_source.file(db).root(db) {
        let mut errors = vec![];
        let entry_point = get_entry_point(db, &top_level_items);
        if entry_point.is_none() {
            errors.push(LowerError::UndefinedEntryPoint);
        }
        (errors, entry_point)
    } else {
        (vec![], None)
    };

    let source_by_expr = root_file_body.source_by_expr;
    let source_by_function = root_file_body.source_by_function;
    let hir_file = HirFile::new(db, file, hir_file_db, top_level_items, entry_point, errors);
    let source_map = HirFileSourceMap::new(db, file, source_by_expr, source_by_function);

    (hir_file, source_map)
}

/// トップレベルのアイテムからエントリポイントを取得します。
///
/// エントリポイントが見つからない場合は`None`を返します。
/// エントリポイントが複数存在する場合は、最初に見つかったものを返します。
fn get_entry_point(db: &dyn HirMasterDatabase, top_level_items: &[Item]) -> Option<Function> {
    for item in top_level_items {
        match item {
            Item::Function(function) => {
                if function.name(db).text(db) == "main" {
                    return Some(*function);
                }
            }
            Item::Module(_) => (),
            Item::UseItem(_) => (),
        }
    }

    None
}

/// HIR中に現れるシンボルを表します
/// メモリ効率のため、シンボルは文字列のインデックスとして表現されます
/// 元の文字列は[Interner]によって管理されます
#[salsa::interned]
pub struct Name {
    #[return_ref]
    pub text: String,
}

/// ステートメントです。
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stmt {
    /// 変数定義を表します。
    ///
    /// 例: `let <name> = <value>;`
    VariableDef {
        /// 変数名
        name: Name,
        /// 初期値
        value: ExprId,
    },
    /// 式を表します。
    ///
    /// 例: `<expr>`
    /// 例: `<expr>;`
    ExprStmt {
        /// 式
        expr: ExprId,
        /// セミコロンがあるかどうか
        has_semicolon: bool,
    },
    /// アイテムを表します。
    Item {
        /// アイテム
        item: Item,
    },
}

/// リテラル
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Literal {
    /// 整数リテラルです。
    Integer(u64),
    /// 文字列リテラルです。
    String(String),
    /// 文字リテラルです。
    Char(char),
    /// 真偽値リテラルです。
    Bool(bool),
}

/// 式
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    /// ローカル変数や関数の参照名です。
    ///
    /// 例: `aaa`
    Symbol(Symbol),
    /// 二項演算子です。
    ///
    /// 例: `<lhs> <op> <rhs>`
    Binary {
        /// 演算子
        op: ast::BinaryOp,
        /// 左辺
        lhs: ExprId,
        /// 右辺
        rhs: ExprId,
    },
    /// リテラルです。
    ///
    /// 例:
    /// - `123`
    /// - `true`
    /// - `'a'`
    Literal(Literal),
    /// 単項演算子です。
    ///
    /// 例: `<op> <expr>`
    Unary {
        /// 演算子
        op: ast::UnaryOp,
        /// 式
        expr: ExprId,
    },
    /// 関数呼び出しです。
    ///
    /// 例: `<callee>(<args>)`
    Call {
        /// 呼び出し対象
        callee: Symbol,
        /// 引数
        args: Vec<ExprId>,
    },
    /// ブロックです。
    ///
    /// 例: `{ <stmts> }`
    Block(Block),
    /// if式です。
    ///
    /// 例: `if <condition> { <then_branch> } else { <else_branch> }`
    If {
        /// 条件式
        condition: ExprId,
        /// then節
        then_branch: ExprId,
        /// else節
        else_branch: Option<ExprId>,
    },
    /// 関数を途中で中断し、指定した値を戻り値として返します。
    ///
    /// 例:
    /// - `return <value>;`
    /// - `return;`
    Return {
        /// 戻り値
        value: Option<ExprId>,
    },
    /// ループブロックを表します。
    Loop {
        /// ループの本体
        block: ExprId,
    },
    /// 解釈できない不明な式です。
    Missing,
}

/// パスを表します
///
/// 例: `aaa::bbb`
#[salsa::interned]
pub struct Path {
    #[return_ref]
    pub segments: Vec<Name>,
}

/// コード中に現れるシンボルを表します
///
/// 関数パラメータとローカル変数は先に名前解決を行い、アイテムなどは後で行います。
/// モジュールスコープを構築するには、別Pod, 別ファイルの解析後でないといけないためです。
/// TODO: ParamとLocalの解決タイミングもアイテムの解決時に行うようにする
///
/// ```nail
/// let a = 10;
/// a // Symbol(Path { segments: ["a"] })
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Symbol {
    /// 関数パラメータ
    Param {
        /// パラメータ名
        name: Name,
        /// パラメータ
        param: Param,
    },
    /// ローカル変数
    Local {
        /// 変数名
        name: Name,
        /// 式
        expr: ExprId,
    },
    /// 解決できないシンボル
    ///
    /// 名前解決フェーズで名前解決を試みます。
    Missing {
        /// パス
        path: NameSolutionPath,
    },
}

/// 名前解決対象のパスを表します。
#[salsa::tracked]
pub struct NameSolutionPath {
    pub path: Path,
}

/// ブロック
///
/// ```nail
/// {
///     let a = 10;
/// } // Block { stmts: [Stmt::VariableDef { name: "a", value: ExprId(0) }], tail: None }
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Block {
    /// ブロック内のステートメント一覧
    pub stmts: Vec<Stmt>,
    /// ブロックの最後の式
    ///
    /// 式がない場合はNoneが入ります。
    /// 例えば、`{ let a = 10; }`のようなブロックは最後のステートメントが`;`なので、式を持たないため、Noneが入ります。
    pub tail: Option<ExprId>,
    /// ブロックAST
    pub ast: ast::BlockExpr,
}
impl Block {
    /// ブロックの最後の式を返します
    /// ブロックの最後の式がない場合はNoneが返ります。
    ///
    /// ```nail
    /// {
    ///    let a = 10;
    ///    a + 1
    /// } // Some(ExprId(1))
    /// ```
    ///
    /// ```nail
    /// {
    ///   let a = 10;
    /// } // None
    /// ```
    pub fn tail<'a>(&self, ctx: &'a HirFileDatabase) -> Option<&'a Expr> {
        if let Some(tail) = self.tail {
            Some(tail.lookup(ctx))
        } else {
            None
        }
    }
}
