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

use std::collections::HashMap;

use ast::AstNode;
pub use body::{BodyLower, ExprId, FunctionBodyId, SharedBodyLowerContext};
pub use db::{Db, Jar};
pub use input::{FixtureDatabase, NailFile, SourceDatabase, SourceDatabaseTrait};
pub use item::{Function, Item, Module, ModuleKind, Param, Type, UseItem};
use name_resolver::resolve_symbols;
pub use name_resolver::{ResolutionStatus, SymbolTable};
pub use testing::TestingDatabase;

/// ビルド対象全体を表します。
#[derive(Debug)]
pub struct Pods {
    pub pods: Vec<Pod>,

    pub symbol_table: SymbolTable,
}

/// PodはNailにおけるパッケージの単位です。
#[derive(Debug)]
pub struct Pod {
    /// Pod名
    pub name: Name,

    /// ルートファイルID
    pub root_file: NailFile,

    /// ルートファイルのHIR構築結果
    pub root_lower_result: LowerResult,

    /// ファイル別のHIR構築結果
    ///
    /// ルートファイルは含まれません。
    lower_result_by_file: HashMap<NailFile, LowerResult>,

    /// モジュール別のHIR構築結果
    ///
    /// ルートファイルのHIR構築結果
    lower_result_by_module: HashMap<Module, LowerResult>,

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
    pub fn get_lower_result_by_file(&self, file: NailFile) -> Option<&LowerResult> {
        self.lower_result_by_file.get(&file)
    }

    /// ファイルの登録順の昇順でHIR構築結果を返します。
    pub fn lower_results_order_registration_asc(&self) -> Vec<(NailFile, &LowerResult)> {
        let mut lower_results = vec![];
        for file in &self.registration_order {
            lower_results.push((*file, self.lower_result_by_file.get(file).unwrap()));
        }

        lower_results
    }

    /// モジュールからHIR構築結果を返します。
    pub fn lower_result_by_module(&self, module: &Module) -> Option<LowerResult> {
        self.lower_result_by_module.get(module).copied()
    }
}

/// ルートファイルをパースし、Pod全体を構築します。
pub fn parse_pods(salsa_db: &dyn Db, path: &str, source_db: &mut dyn SourceDatabaseTrait) -> Pods {
    let pod = parse_pod(salsa_db, path, source_db);
    let symbol_table = resolve_symbols(salsa_db, &pod);

    Pods {
        pods: vec![pod],
        symbol_table,
    }
}

/// ルートファイル、サブファイルをパースし、Podを構築します。
fn parse_pod(salsa_db: &dyn Db, path: &str, source_db: &mut dyn SourceDatabaseTrait) -> Pod {
    let mut lower_result_by_file = HashMap::new();
    let mut lower_result_by_module = HashMap::new();
    let mut registration_order = vec![];

    let root_file_path = std::path::PathBuf::from(path);
    let nail_file = source_db.source_root();

    let ast_source = parse_to_ast(salsa_db, nail_file);
    let root_result = build_hir(salsa_db, ast_source);

    for sub_module in root_result.modules(salsa_db) {
        if matches!(sub_module.kind(salsa_db), ModuleKind::Inline { .. }) {
            continue;
        }

        let sub_module_name = sub_module.name(salsa_db).text(salsa_db);
        let sub_module_lower_result =
            parse_sub_module(salsa_db, sub_module_name, &root_file_path, source_db);
        lower_result_by_module.insert(*sub_module, sub_module_lower_result);

        registration_order.push(sub_module_lower_result.file(salsa_db));
        lower_result_by_file.insert(
            sub_module_lower_result.file(salsa_db),
            sub_module_lower_result,
        );
    }

    Pod {
        name: Name::new(salsa_db, "dummy_pod_name".to_string()),
        root_file: source_db.source_root(),
        root_lower_result: root_result,
        lower_result_by_file,
        lower_result_by_module,
        registration_order,
    }
}

/// サブモジュールをパースします
/// `root_file_path`はルートファイルのファイルパスまで込みのパスを指定します。(`/main.nail`)
fn parse_sub_module(
    salsa_db: &dyn Db,
    sub_module_name: &str,
    root_file_path: &std::path::Path,
    source_db: &mut dyn SourceDatabaseTrait,
) -> LowerResult {
    // todo: サブモジュールのサブモジュールの場合はaaa/bbb.nailのようにする必要がある
    let file_path = root_file_path.with_file_name(format!("{sub_module_name}.nail"));
    let nail_file = source_db.register_file_with_read(salsa_db, file_path);

    let ast_source = parse_to_ast(salsa_db, nail_file);
    build_hir(salsa_db, ast_source)
}

/// HIR構築時のエラー
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LowerError {
    /// エントリーポイントが見つからない場合
    UndefinedEntryPoint,
}

/// HIR構築結果
#[salsa::tracked]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LowerResult {
    /// ファイル
    ///
    /// 1ファイルにつき1つの[LowerResult]が生成されます。
    pub file: NailFile,

    /// ボディ構築時に共有されるコンテキスト
    ///
    /// 1ファイル内のコンテキストです。
    #[return_ref]
    pub shared_ctx: SharedBodyLowerContext,
    /// トップレベルのアイテム一覧
    #[return_ref]
    pub top_level_items: Vec<Item>,
    /// エントリーポイントの関数
    pub entry_point: Option<Function>,
    /// エラー一覧
    #[return_ref]
    pub errors: Vec<LowerError>,
}
impl LowerResult {
    /// 関数IDから関数ボディを取得します。
    pub fn function_body_by_function<'a>(
        &self,
        db: &'a dyn Db,
        function: Function,
    ) -> Option<&'a Expr> {
        self.shared_ctx(db)
            .function_body_by_ast_block(function.ast(db).body()?)
    }

    /// ファイル内の関数一覧を返します。
    pub fn functions<'a>(&self, db: &'a dyn Db) -> &'a [Function] {
        self.shared_ctx(db).functions()
    }

    /// ファイル内のモジュール一覧を返します。
    pub fn modules<'a>(&self, db: &'a dyn Db) -> &'a [Module] {
        self.shared_ctx(db).modules()
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
pub fn parse_to_ast(db: &dyn crate::Db, nail_file: NailFile) -> AstSourceFile {
    let ast = parser::parse(nail_file.contents(db));
    let ast_source_file = ast::SourceFile::cast(ast.syntax()).unwrap();
    AstSourceFile::new(db, nail_file, ast_source_file)
}

/// ASTを元に[ItemTree]を構築します。
#[salsa::tracked]
pub fn build_hir(salsa_db: &dyn crate::Db, ast_source: AstSourceFile) -> crate::LowerResult {
    let file = ast_source.file(salsa_db);
    let source_file = ast_source.source(salsa_db);

    let mut shared_ctx = SharedBodyLowerContext::new();

    let mut top_level_ctx = BodyLower::new(file, HashMap::new());
    let top_level_items = source_file
        .items()
        .filter_map(|item| top_level_ctx.lower_toplevel(salsa_db, item, &mut shared_ctx))
        .collect::<Vec<_>>();

    let (errors, entry_point) = if ast_source.file(salsa_db).root(salsa_db) {
        let mut errors = vec![];
        let entry_point = get_entry_point(salsa_db, &top_level_items);
        if entry_point.is_none() {
            errors.push(LowerError::UndefinedEntryPoint);
        }
        (errors, entry_point)
    } else {
        (vec![], None)
    };

    LowerResult::new(
        salsa_db,
        file,
        shared_ctx,
        top_level_items,
        entry_point,
        errors,
    )
}

/// トップレベルのアイテムからエントリポイントを取得します。
///
/// エントリポイントが見つからない場合は`None`を返します。
/// エントリポイントが複数存在する場合は、最初に見つかったものを返します。
fn get_entry_point(salsa_db: &dyn Db, top_level_items: &[Item]) -> Option<Function> {
    for item in top_level_items {
        match item {
            Item::Function(function) => {
                if let Some(name) = function.name(salsa_db) {
                    if name.text(salsa_db) == "main" {
                        return Some(*function);
                    }
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
    /// 解釈できない不明な式です。
    Missing,
}

/// パスを表します
///
/// 例: `aaa::bbb`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Path {
    segments: Vec<Name>,
}
impl Path {
    /// 区切り文字で分割したパスの一覧を返します
    pub fn segments(&self) -> &[Name] {
        self.segments.as_slice()
    }
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
    Missing {
        /// パス
        path: Path,
    },
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
    pub fn tail<'a>(&self, ctx: &'a SharedBodyLowerContext) -> Option<&'a Expr> {
        if let Some(tail) = self.tail {
            Some(tail.lookup(ctx))
        } else {
            None
        }
    }
}
