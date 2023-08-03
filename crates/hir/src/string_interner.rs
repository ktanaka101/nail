/// コード中に現れる文字列を効率よく管理するための構造体
/// 例えば、`"foo"`という文字列がコード中に複数回現れる場合、
/// それらの文字列は同じメモリ領域に保持され、使用者側はその文字列にアクセスするための`Key`を取り回します。
#[derive(Debug, Default)]
pub struct Interner(lasso::Rodeo);
impl Interner {
    /// 新しい`Interner`を作成します。
    pub fn new() -> Self {
        Self(lasso::Rodeo::new())
    }

    /// 文字列を割り当て、その`Key`を返します。
    pub fn intern(&mut self, string: &str) -> Key {
        Key(self.0.get_or_intern(string))
    }

    /// 文字列を元に割り当て隅であれば`Key`を返します。
    /// 存在しなければ`None`を返します。
    pub fn get_key(&self, string: &str) -> Option<Key> {
        self.0.get(string).map(Key)
    }

    /// `Key`を元に文字列を取得します。
    pub fn lookup(&self, key: Key) -> &str {
        self.0.resolve(&key.0)
    }
}

/// `Interner`によって割り当てられた文字列にアクセスするためのキーを表します。
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Key(lasso::Spur);
