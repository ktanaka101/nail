#![warn(missing_docs)]

mod body;
mod db;
mod input;
mod item_tree;
pub mod string_interner;

use std::{collections::HashMap, marker::PhantomData};

use ast::{Ast, AstNode};
pub use body::{BodyLower, ExprId, FunctionBodyId, SharedBodyLowerContext};
pub use db::{Database, FunctionId, ItemScopeId, ModuleId, ParamId, UseItemId};
pub use input::{
    FileId, FilelessSourceDatabase, FixtureDatabase, SourceDatabase, SourceDatabaseTrait,
};
use item_tree::ItemTreeBuilderContext;
pub use item_tree::{Function, ItemDefId, ItemTree, Param, Type};
use la_arena::Idx;
use string_interner::{Interner, Key};
use syntax::SyntaxNodePtr;

#[derive(Debug)]
pub enum LowerError {
    UndefinedEntryPoint,
}

#[derive(Debug)]
pub struct LowerResult {
    pub file_id: FileId,
    pub shared_ctx: SharedBodyLowerContext,
    pub top_level_items: Vec<ItemDefId>,
    pub entry_point: Option<FunctionId>,
    pub db: Database,
    pub item_tree: ItemTree,
    pub interner: Interner,
    pub errors: Vec<LowerError>,
}
impl LowerResult {
    pub fn function_body_by_function(&self, function: FunctionId) -> Option<&Expr> {
        let body_block = self.item_tree.block_id_by_function(&function)?;
        self.shared_ctx.function_body_by_block(body_block)
    }
}

pub fn parse_root(path: &str, source_db: &mut dyn SourceDatabaseTrait) -> LowerResult {
    let file_id = source_db.register_file_with_read(path);

    let ast = parser::parse(source_db.content(file_id).unwrap());
    let ast = ast::SourceFile::cast(ast.syntax()).unwrap();

    lower(file_id, ast)
}

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
    pub file_id: FileId,
    pub value: T,
}

/// HIR中に現れるシンボルを表します
/// メモリ効率のため、シンボルは文字列のインデックスとして表現されます
/// 元の文字列は`Interner`によって管理されます
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Name(Key);
impl Name {
    pub fn key(&self) -> Key {
        self.0
    }

    fn from_key(key: Key) -> Self {
        Self(key)
    }
}

/// ステートメントです。
#[derive(Debug, PartialEq, Eq)]
pub enum Stmt {
    /// 変数定義を表します。
    /// `let <name> = <value>;`
    VariableDef { name: Name, value: ExprId },
    /// 式を表します。
    /// `<expr>`
    /// `<expr>;`
    ExprStmt { expr: ExprId, has_semicolon: bool },
    /// アイテムを表します。
    Item { item: ItemDefId },
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
    /// `aaa`
    Symbol(Symbol),
    /// 二項演算子です。
    /// `<lhs> <op> <rhs>`
    Binary {
        op: ast::BinaryOp,
        lhs: ExprId,
        rhs: ExprId,
    },
    /// リテラルです。
    /// `123`
    /// `abc`
    /// `true`
    /// `'a'`
    Literal(Literal),
    /// 単項演算子です。
    /// `<op> <expr>`
    Unary { op: ast::UnaryOp, expr: ExprId },
    /// 関数呼び出しです。
    /// `<callee>(<args>)`
    Call { callee: Symbol, args: Vec<ExprId> },
    /// ブロックです。
    /// `{ <stmts> }`
    Block(Block),
    /// if式です。
    /// `if <condition> { <then_branch> } else { <else_branch> }`
    If {
        condition: ExprId,
        then_branch: ExprId,
        else_branch: Option<ExprId>,
    },
    /// 関数を途中で中断し、指定した値を戻り値として返します。
    /// `return <value>;`
    /// `return;`
    Return { value: Option<ExprId> },
    /// 解釈できない不明な式です。
    Missing,
}

/// パスを表します
/// `aaa::bbb`
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
/// ```nail
/// let a = 10;
/// a // Symbol::Local { name: "a", expr: ExprId(0) }
/// ```
#[derive(Debug, PartialEq, Eq)]
pub enum Symbol {
    /// 関数パラメータ
    Param { name: Name, param: ParamId },
    /// ローカル変数
    Local { name: Name, expr: ExprId },
    /// 関数
    Function { path: Path, function: FunctionId },
    /// 解決できない
    Missing { path: Path },
}

/// ブロック
/// ```nail
/// {
///     let a = 10;
/// } // Block { stmts: [Stmt::VariableDef { name: "a", value: ExprId(0) }], tail: None }
/// ```
#[derive(Debug, PartialEq, Eq)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub tail: Option<ExprId>,
    pub ast: AstId<ast::BlockExpr>,
}
impl Block {
    /// ブロックの最後の式を返します
    /// ブロックの最後の式がない場合はNoneが返ります。
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
