use std::collections::HashMap;

use crate::{
    db::{FunctionId, ModuleId, UseItemId},
    AstId, Name, ParamId, Path,
};

/// 関数のパラメータを表す
/// 例: `fn f(x: int, y: int) -> int { x + y }` であれば `x: int` と `y: int` のそれぞれがパラメータ
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Param {
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
#[derive(Debug, Clone, PartialEq, Eq)]
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
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Function {
    /// 関数のモジュールパス
    pub path: Path,
    /// 関数名
    ///
    /// 言語仕様上は`None`になることはありませんが、
    /// エラー耐性のために、関数名が存在しない場合は`None`となります。
    pub name: Option<Name>,
    /// 関数のパラメータ
    pub params: Vec<ParamId>,
    /// 関数のパラメータ名をキーとしたパラメータIDのマップ
    pub param_by_name: HashMap<Name, ParamId>,
    /// 関数の戻り値の型
    ///
    /// 例: `fn f(x: int, y: int) -> int { x + y }` であれば `int`
    pub return_type: Type,
    /// 関数のAST上のID
    pub ast: AstId<ast::FunctionDef>,
}

/// モジュール定義を表す
#[derive(Debug, PartialEq, Eq)]
pub struct Module {
    /// モジュール名
    pub name: Name,
    /// モジュール内のアイテム一覧
    pub items: Vec<ItemDefId>,
}

/// アイテムの使用宣言を表す
///
/// 例: `use std::io::println;`
#[derive(Debug, PartialEq, Eq)]
pub struct UseItem {
    /// 使用宣言の名前
    ///
    /// 例: `use std::io::println;` であれば `println`
    pub name: Name,
    /// 使用宣言対象のパス
    ///
    /// 例: `use std::io::println;` であれば `std::io`
    pub path: Path,
}

/// ファイルルート及びモジュール内のアイテムを表す
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ItemDefId {
    /// 関数定義
    Function(FunctionId),
    /// モジュール定義
    Module(ModuleId),
    /// アイテム使用宣言
    UseItem(UseItemId),
}
