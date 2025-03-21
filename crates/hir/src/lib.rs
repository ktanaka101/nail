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
//!    モジュールツリーは、`aaa::bbb::ccc`のようなパスを名前解決するためのものです。
//! 4. 使用宣言/関数/モジュールの名前解決を行う

// #[salsa::tracked]で生成される関数にドキュメントコメントが作成されないため警告が出てしまうため許可します。
#![allow(missing_docs)]

mod body;
mod db;
mod input;
mod item;
mod name_resolver;
pub mod testing;

use std::collections::HashMap;

use ast::{AstNode, AstPtr, HasName};
pub use body::{BindingId, BodyLower, ExprId, FunctionBodyId, HirFileDatabase};
pub use db::{HirMasterDatabase, Jar};
pub use input::{FixtureDatabase, NailFile, SourceDatabase, SourceDatabaseTrait};
pub use item::{
    Function, Item, Module, ModuleKind, Param, ParamData, RecordField, Struct, StructKind, Type,
    UseItem,
};
use name_resolver::resolve_symbols;
pub use name_resolver::{ModuleScopeOrigin, ResolutionMap, ResolutionStatus};
use syntax::SyntaxNode;

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

    /// Pod内の構造体を全て返します。
    pub fn all_structs(&self, db: &dyn HirMasterDatabase) -> Vec<(HirFile, Struct)> {
        let mut structs = vec![];

        structs.append(
            &mut self
                .root_hir_file
                .structs(db)
                .iter()
                .map(|struct_| (self.root_hir_file, *struct_))
                .collect::<Vec<_>>(),
        );

        for nail_file in &self.registration_order {
            let hir_file = self.get_hir_file_by_file(*nail_file).unwrap();
            structs.append(
                &mut hir_file
                    .structs(db)
                    .iter()
                    .map(|struct_| (*hir_file, *struct_))
                    .collect::<Vec<_>>(),
            );
        }

        structs
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
pub fn parse_pods(db: &dyn HirMasterDatabase, source_db: &dyn SourceDatabaseTrait) -> Pods {
    let pod = PodBuilder::new(db, source_db).build();
    let symbol_table = resolve_symbols(db, &pod);

    Pods {
        root_pod: pod,
        resolution_map: symbol_table,
    }
}

struct PodBuilder<'a> {
    db: &'a dyn HirMasterDatabase,

    source_db: &'a dyn SourceDatabaseTrait,

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
        source_db: &'a dyn SourceDatabaseTrait,
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

        let root_nail_green_node = build_green_node(self.db, root_nail_file);
        let (root_hir_file, root_source_map) = build_hir_file(self.db, root_nail_green_node);
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

        let nail_green_node = build_green_node(self.db, nail_file);
        let (hir_file, source_map) = build_hir_file(db, nail_green_node);

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

    /// ファイルのGreenノード
    pub green_node: NailGreenNode,

    /// 式とソースコードのマッピング
    pub source_by_expr: HashMap<ExprId, ExprSource>,

    /// 式とソースコードのマッピング
    pub source_by_function: HashMap<Function, FunctionSource>,

    /// 型とソースコードのマッピング
    pub source_by_type: HashMap<Type, TypeSource>,
}

/// パーサのエラー情報
#[salsa::accumulator]
pub struct ParserDiagnostics(parser::ParserError);

/// あるファイルのGreenノードを保持します。
#[salsa::tracked]
pub struct NailGreenNode {
    /// ファイルを表します。
    /// コンテンツが変わればGreenNodeも変わるため、idとしています。
    #[id]
    pub nail_file: NailFile,

    /// Greenノード
    #[return_ref]
    pub green_node: ast::GreenNode,
}

/// ASTを元に[HirFile]を構築します。
#[salsa::tracked]
pub fn build_green_node(db: &dyn HirMasterDatabase, nail_file: NailFile) -> NailGreenNode {
    let parse_result = parser::parse(nail_file.contents(db));
    for parse_error in parse_result.errors {
        ParserDiagnostics::push(db, parse_error);
    }

    NailGreenNode::new(db, nail_file, parse_result.green_node)
}

/// 式のAST位置です。
pub type ExprSource = InFile<AstPtr<ast::Expr>>;
/// 関数定義のAST位置です。
pub type FunctionSource = InFile<AstPtr<ast::FunctionDef>>;
/// パス型のAST位置です。
pub type TypeSource = InFile<AstPtr<ast::PathType>>;

/// 型引数をファイル内で一意として表現します。
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InFile<T> {
    /// ファイル情報
    pub file: NailFile,
    /// ファイル内として保持する値
    pub value: T,
}

/// ファイル単位のHIR構築結果
#[salsa::tracked]
pub struct HirFile {
    /// ファイル
    ///
    /// 1ファイルにつき1つの[HirFile]が生成されます
    pub file: NailFile,
    /// ファイルのGreenノード
    pub green_node: NailGreenNode,

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
    /// ファイル内の関数一覧を返します。
    pub fn functions<'a>(&self, db: &'a dyn HirMasterDatabase) -> &'a [Function] {
        self.db(db).functions()
    }

    /// ファイル内の構造体一覧を返します。
    pub fn structs<'a>(&self, db: &'a dyn HirMasterDatabase) -> &'a [Struct] {
        self.db(db).structs()
    }

    /// ファイル内のモジュール一覧を返します。
    pub fn modules<'a>(&self, db: &'a dyn HirMasterDatabase) -> &'a [Module] {
        self.db(db).modules()
    }
}

/// ASTを元に[HirFile]を構築します。
#[salsa::tracked]
fn build_hir_file(
    db: &dyn HirMasterDatabase,
    nail_green_node: NailGreenNode,
) -> (HirFile, HirFileSourceMap) {
    let mut hir_file_db = HirFileDatabase::new(nail_green_node);

    let mut root_file_body = BodyLower::new(db, nail_green_node, HashMap::new(), &mut hir_file_db);

    let syntax_node = SyntaxNode::new_root(nail_green_node.green_node(db).clone());
    let ast_source_file = ast::SourceFile::cast(syntax_node).unwrap();

    let top_level_items = ast_source_file
        .items()
        .filter_map(|item| root_file_body.lower_toplevel(db, item))
        .collect::<Vec<_>>();

    let nail_file = nail_green_node.nail_file(db);
    let (errors, entry_point) = if nail_file.root(db) {
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
    let source_by_type = root_file_body.source_by_type;
    let hir_file = HirFile::new(
        db,
        nail_file,
        nail_green_node,
        hir_file_db,
        top_level_items,
        entry_point,
        errors,
    );
    let source_map = HirFileSourceMap::new(
        db,
        nail_file,
        nail_green_node,
        source_by_expr,
        source_by_function,
        source_by_type,
    );

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
            Item::Struct(_) => (),
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
impl Name {
    #[inline]
    pub(crate) fn new_from_ident(db: &dyn HirMasterDatabase, ident: ast::Ident) -> Self {
        Name::new(db, ident.name().to_string())
    }

    #[inline]
    pub(crate) fn new_from_has_name<T: ast::HasName>(
        db: &dyn HirMasterDatabase,
        has_name: &T,
    ) -> Option<Self> {
        Some(Name::new_from_ident(db, has_name.name()?))
    }
}

/// ステートメントです。
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stmt {
    /// 変数定義を表します。
    ///
    /// 例: `let [mut] <name> = <value>;`
    Let {
        /// 変数名
        name: Name,
        /// バインディング情報
        binding: BindingId,
        /// 初期値
        value: ExprId,
    },
    /// 式を表します。
    ///
    /// 例: `<expr>`
    /// 例: `<expr>;`
    Expr {
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

/// 二項演算子
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinaryOp {
    /// `+`
    Add,
    /// `-`
    Sub,
    /// `*`
    Mul,
    /// `/`
    Div,
    /// `==`
    Equal,
    /// `!=`
    NotEq,
    /// `>`
    GreaterThan,
    /// `<`
    LessThan,
    /// `>=`
    GtEq,
    /// `<=`
    LtEq,
    /// `=`
    Assign,
}
impl std::fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryOp::Add => write!(f, "+"),
            BinaryOp::Sub => write!(f, "-"),
            BinaryOp::Mul => write!(f, "*"),
            BinaryOp::Div => write!(f, "/"),
            BinaryOp::Equal => write!(f, "=="),
            BinaryOp::NotEq => write!(f, "!="),
            BinaryOp::GreaterThan => write!(f, ">"),
            BinaryOp::LessThan => write!(f, "<"),
            BinaryOp::GtEq => write!(f, ">="),
            BinaryOp::LtEq => write!(f, "<="),
            BinaryOp::Assign => write!(f, "="),
        }
    }
}

/// 単項演算子
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnaryOp {
    /// `-`
    Neg,
    /// `!`
    Not,
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
        op: BinaryOp,
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
        op: UnaryOp,
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
    /// ループを中断し、次の反復処理に進みます。
    Continue,
    /// ループを終了します。
    Break {
        /// Breakの値
        value: Option<ExprId>,
    },
    Record {
        /// レコード式のシンボル
        ///
        /// ex. `aaa::bbb::Point { x: i32, y: i32 }`
        ///      ^^^^^^^^^^^^^^^
        symbol: Symbol,
        /// フィールド
        fields: Vec<RecordFieldExpr>,
    },
    Field {
        /// フィールド式のベースです。
        ///
        /// ex. `foo.bar`
        base: ExprId,
        /// フィールド名
        name: Name,
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
impl Path {
    /// パスを含んだASTノードからHIR表現のパスを生成します
    pub(crate) fn new_from_path<T: ast::HasPath>(
        db: &dyn HirMasterDatabase,
        has_path: &T,
    ) -> Option<Self> {
        let path = has_path.path()?;
        let segments = path
            .segments()
            .map(|segment| Some(Name::new_from_ident(db, segment.name()?)))
            .collect::<Option<Vec<Name>>>()?;
        Some(Path::new(db, segments))
    }

    /// Nameを含んだASTNodeリストからパスを生成します
    pub(crate) fn new_from_has_names<T: ast::HasName>(
        db: &dyn HirMasterDatabase,
        has_names: &[T],
    ) -> Option<Self> {
        let segments = has_names
            .iter()
            .map(|has_name| Name::new_from_has_name(db, has_name))
            .collect::<Option<Vec<Name>>>()?;
        Some(Path::new(db, segments))
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
        /// TODO: use binding
        param: Param,
    },
    /// ローカル変数
    Local {
        /// 変数名
        name: Name,
        /// バインディング情報
        binding: BindingId,
    },
    /// 解決できない式シンボル
    ///
    /// 名前解決フェーズで名前解決を試みます。
    MissingExpr {
        /// パス
        path: NameSolutionPath,
    },
    /// 解決できない型シンボル
    ///
    /// 型解決フェーズで名前解決を試みます。
    MissingType {
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
/// } // Block { stmts: [Stmt::Let { name: "a", value: ExprId(0) }], tail: None }
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

/// An expr binding in HIR
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Binding {
    pub name: Name,
    pub mutable: bool,
    pub expr: ExprId,
}

/// レコード式のフィールドを表す
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RecordFieldExpr {
    /// 名前
    pub name: Name,
    /// 値
    pub value: ExprId,
}
