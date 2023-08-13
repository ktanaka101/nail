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

#![warn(missing_docs)]

mod body;
mod db;
mod input;
mod item_tree;
mod string_interner;

use std::{collections::HashMap, marker::PhantomData};

use ast::{Ast, AstNode};
pub use body::{BodyLower, ExprId, FunctionBodyId, SharedBodyLowerContext};
pub use db::{Database, FunctionId, ItemScopeId, ModuleId, ParamId, UseItemId};
pub use input::{
    FileId, FilelessSourceDatabase, FixtureDatabase, SourceDatabase, SourceDatabaseTrait,
};
use item_tree::ItemTreeBuilderContext;
pub use item_tree::{Function, ItemDefId, ItemTree, Module, Param, Type, UseItem};
use la_arena::Idx;
use string_interner::{Interner, Key};
use syntax::SyntaxNodePtr;

/// HIR構築時のエラー
#[derive(Debug)]
pub enum LowerError {
    /// エントリーポイントが見つからない場合
    UndefinedEntryPoint,
}

/// HIR構築結果
#[derive(Debug)]
pub struct LowerResult {
    /// ファイルID
    pub file_id: FileId,
    /// ボディ構築時に共有されるコンテキスト
    ///
    /// 1ファイル内のコンテキストです。
    pub shared_ctx: SharedBodyLowerContext,
    /// トップレベルのアイテム一覧
    pub top_level_items: Vec<ItemDefId>,
    /// エントリーポイントの関数
    pub entry_point: Option<FunctionId>,
    /// データベース
    pub db: Database,
    /// アイテムツリー
    pub item_tree: ItemTree,
    /// シンボルインターナ
    pub interner: Interner,
    /// エラー一覧
    pub errors: Vec<LowerError>,
}
impl LowerResult {
    /// 関数IDから関数ボディを取得します。
    pub fn function_body_by_function(&self, function: FunctionId) -> Option<&Expr> {
        let body_block = self.item_tree.block_id_by_function(&function)?;
        self.shared_ctx.function_body_by_block(body_block)
    }
}

/// 指定したファイルパスをルートファイルとして、ファイルの読み込みからHIRの構築までを行います。
pub fn parse_root(path: &str, source_db: &mut dyn SourceDatabaseTrait) -> LowerResult {
    let file_id = source_db.register_file_with_read(path);

    let ast = parser::parse(source_db.content(file_id).unwrap());
    let ast = ast::SourceFile::cast(ast.syntax()).unwrap();

    lower(file_id, ast)
}

/// ASTからHIRの構築を行います。
pub fn lower(file_id: FileId, ast: ast::SourceFile) -> LowerResult {
    let mut interner = Interner::new();
    let mut db = Database::new();
    let item_tree_builder = ItemTreeBuilderContext::new(file_id, &mut interner);
    let item_tree = item_tree_builder.build(file_id, &ast, &mut db);

    let mut shared_ctx = SharedBodyLowerContext::new();

    let mut top_level_ctx = BodyLower::new(file_id, HashMap::new());
    let top_level_items = ast
        .items()
        .filter_map(|item| {
            top_level_ctx.lower_toplevel(item, &mut shared_ctx, &db, &item_tree, &mut interner)
        })
        .collect::<Vec<_>>();

    let mut errors = vec![];
    let entry_point = get_entry_point(&top_level_items, &db, &interner);
    if entry_point.is_none() {
        errors.push(LowerError::UndefinedEntryPoint);
    }

    LowerResult {
        file_id,
        shared_ctx,
        top_level_items,
        entry_point,
        db,
        item_tree,
        interner,
        errors,
    }
}

/// トップレベルのアイテムからエントリポイントを取得します。
///
/// エントリポイントが見つからない場合は`None`を返します。
/// エントリポイントが複数存在する場合は、最初に見つかったものを返します。
fn get_entry_point(
    top_level_items: &[ItemDefId],
    db: &Database,
    interner: &Interner,
) -> Option<FunctionId> {
    for item in top_level_items {
        match item {
            ItemDefId::Function(function_id) => {
                let function = function_id.lookup(db);
                if let Some(name) = function.name {
                    if interner.lookup(name.key()) == "main" {
                        return Some(*function_id);
                    }
                }
            }
            ItemDefId::Module(_) => (),
            ItemDefId::UseItem(_) => (),
        }
    }

    None
}

/// ファイル内のASTノードを一意に表します
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct AstId<T: Ast>(InFile<AstPtr<T>>);

/// 単一のASTノード上の特定のノードを一意に識別するための値です。
/// ジェネリクスにより異なるノード種類を型レベルで区別します。
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct AstPtr<T: Ast> {
    raw: AstIdx,
    _ty: PhantomData<fn() -> T>,
}

/// 単一のASTノード上で一意に識別するための値です。
pub type AstIdx = Idx<SyntaxNodePtr>;

/// ファイル内の任意の値を保持します。
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct InFile<T> {
    /// ファイルID
    pub file_id: FileId,
    /// 値
    ///
    /// ファイルIDとの組み合わせで一意に識別されるものを使用することを推奨します。
    pub value: T,
}

/// HIR中に現れるシンボルを表します
/// メモリ効率のため、シンボルは文字列のインデックスとして表現されます
/// 元の文字列は[Interner]によって管理されます
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Name(Key);
impl Name {
    /// 内部表現であるキーを取得します
    ///
    /// このキーは[Interner]によって管理されている文字列の参照用の値です。
    pub fn key(&self) -> Key {
        self.0
    }

    /// キーから[Name]を取得します
    fn from_key(key: Key) -> Self {
        Self(key)
    }
}

/// ステートメントです。
#[derive(Debug, PartialEq, Eq)]
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
        item: ItemDefId,
    },
}

/// リテラル
#[derive(Debug, PartialEq, Eq)]
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
#[derive(Debug, PartialEq, Eq)]
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
    /// - `abc`
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
#[derive(Debug, Clone, PartialEq, Eq)]
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
/// コード中のを解決した結果として、Param, Localなどに変換されます
///
/// ```nail
/// let a = 10;
/// a // Symbol::Local { name: "a", expr: ExprId(0) }
/// ```
#[derive(Debug, PartialEq, Eq)]
pub enum Symbol {
    /// 関数パラメータ
    Param {
        /// パラメータ名
        name: Name,
        /// パラメータ
        param: ParamId,
    },
    /// ローカル変数
    Local {
        /// 変数名
        name: Name,
        /// 式
        expr: ExprId,
    },
    /// 関数
    Function {
        /// 関数名
        path: Path,
        /// 関数
        function: FunctionId,
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
#[derive(Debug, PartialEq, Eq)]
pub struct Block {
    /// ブロック内のステートメント一覧
    pub stmts: Vec<Stmt>,
    /// ブロックの最後の式
    ///
    /// 式がない場合はNoneが入ります。
    /// 例えば、`{ let a = 10; }`のようなブロックは最後のステートメントが`;`なので、式を持たないため、Noneが入ります。
    pub tail: Option<ExprId>,
    /// ブロックAST
    pub ast: AstId<ast::BlockExpr>,
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
