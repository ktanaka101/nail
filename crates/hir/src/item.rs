use std::collections::HashMap;

use la_arena::Idx;

use crate::{Expr, HirFile, HirFileDatabase, HirMasterDatabase, Name, Path};

/// 関数のパラメータのIDを表す
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Param(pub(crate) Idx<ParamData>);
impl Param {
    /// パラメータを作成します。
    pub(crate) fn new(
        hir_file_ctx: &mut HirFileDatabase,
        name: Option<Name>,
        ty: Type,
        pos: usize,
    ) -> Self {
        hir_file_ctx.alloc_param(ParamData { name, ty, pos })
    }

    /// パラメータのデータを取得します。
    pub fn data(self, hir_file_ctx: &HirFileDatabase) -> &ParamData {
        hir_file_ctx.param_data(self)
    }
}

/// 関数のパラメータを表す
/// 例: `fn f(x: int, y: int) -> int { x + y }` であれば `x: int` と `y: int` のそれぞれがパラメータ
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParamData {
    /// パラメータ名
    /// 例: `fn f(x: int)` であれば `x`
    pub name: Option<Name>,
    /// パラメータの型
    /// 例: `fn f(x: int)` であれば `int`
    pub ty: Type,
    /// パラメータの位置(0-indexed)
    /// 例: `fn f(x: int, y: int)` であれば `x` は `0` で `y` は `1`
    pub pos: usize,
}

/// 型を表す
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    /// 数値型
    ///
    /// 将来的には `u8`, `u16`, `u32`, `u64`, `u128`, `usize`, `i8`, `i16`, `i32`, `i64`, `i128`, `isize` に分かれる
    /// 例: `10`
    Integer,
    /// 文字列型
    ///
    /// 例: `"hello"`
    String,
    /// 文字型
    ///
    /// 例: `'a'`
    Char,
    /// 真偽値型
    ///
    /// 例: `true`
    Boolean,
    /// 値を持たない型
    ///
    /// 他言語等では、`void`のような概念です。
    /// 例: `()`, `fn f() -> () { () }`
    Unit,
    /// 型が不明なことを表す
    ///
    /// この型を直接指定することはできませんが、型推論の結果として得られることがあります。
    Unknown,
}

/// 関数定義を表す
///
/// 例: `fn f(x: int, y: int) -> int { x + y }`
#[salsa::tracked]
pub struct Function {
    /// 関数名
    pub name: Name,
    /// 関数のパラメータ
    #[return_ref]
    pub params: Vec<Param>,
    /// 関数のパラメータ名をキーとしたパラメータIDのマップ
    #[return_ref]
    pub param_by_name: HashMap<Name, Param>,
    /// 関数の戻り値の型
    ///
    /// 例: `fn f(x: int, y: int) -> int { x + y }` であれば `int`
    pub return_type: Type,
}

impl Function {
    /// 関数定義のASTを返します。
    pub fn ast_def(
        self,
        db: &dyn HirMasterDatabase,
        hir_file: HirFile,
    ) -> Option<ast::FunctionDef> {
        hir_file.db(db).function_ast_by_function(self)
    }

    /// 関数ボディを返します。
    pub fn body(self, db: &dyn HirMasterDatabase, hir_file: HirFile) -> Option<&Expr> {
        hir_file.db(db).function_body_by_function(self)
    }

    /// 関数ボディのASTを返します。
    pub fn ast_body(self, db: &dyn HirMasterDatabase, hir_file: HirFile) -> Option<ast::BlockExpr> {
        hir_file
            .db(db)
            .function_ast_by_function(self)
            .and_then(|f| f.body())
    }
}

/// モジュール定義を表す
#[salsa::tracked]
pub struct Module {
    /// モジュール名
    pub name: Name,

    /// モジュール種別
    #[return_ref]
    pub kind: ModuleKind,
}

/// モジュール種別
///
/// モジュール定義には以下の2種類があります。
/// - 別ファイルにモジュールがあることを表す`mod outline;`
/// - 1ファイル中にインラインで記述可能な`mod inline { /** モジュール内 */ }`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ModuleKind {
    /// インラインモジュール
    ///
    /// ```nail
    /// //- mail.nail
    /// mod aaa;
    ///
    /// //- aaa.nail
    /// // mod aaa
    /// mod bbb;
    ///
    /// //- aaa/bbb.nail
    /// // mod bbb
    /// ```
    Inline {
        /// アイテム一覧
        items: Vec<Item>,
    },

    /// アウトラインモジュール
    ///
    /// ```nail
    /// mod aaa {
    ///     mod bbb {
    ///     }
    ///
    ///     fn aaa {
    ///     }
    /// }
    /// ```
    Outline,
}

/// アイテムの使用宣言を表す
///
/// 例: `use std::io::println;`
#[salsa::interned]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UseItem {
    /// 使用宣言の名前
    ///
    /// 例: `use std::io::println;` であれば `println`
    pub name: Name,
    /// 使用宣言対象のパス
    ///
    /// 例: `use std::io::println;` であれば `std::io`
    #[return_ref]
    pub path: Path,
}

/// ファイルルート及びモジュール内のアイテムを表す
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Item {
    /// 関数定義
    Function(Function),
    /// モジュール定義
    Module(Module),
    /// アイテム使用宣言
    UseItem(UseItem),
}
