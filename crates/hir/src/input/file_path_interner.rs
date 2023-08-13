/// ファイルパスを効率的に扱うための構造体
///
/// ファイルパスを文字列ではなく整数で扱うことで、メモリ使用量を削減します。
/// 同じファイルパスが複数回出現した場合、同じ値を返すようになっています。
pub struct FilePathInterner(lasso::Rodeo);
impl FilePathInterner {
    #[must_use]
    pub(crate) fn new() -> Self {
        Self(lasso::Rodeo::new())
    }

    pub(crate) fn intern(&mut self, path: &str) -> FilePath {
        FilePath(self.0.get_or_intern(path))
    }

    #[must_use]
    pub(crate) fn get(&self, path: &str) -> Option<FilePath> {
        self.0.get(path).map(FilePath)
    }

    #[must_use]
    pub fn _lookup(&self, path: FilePath) -> &str {
        self.0.resolve(&path.0)
    }
}

/// ファイルパス
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FilePath(lasso::Spur);
impl FilePath {
    #[must_use]
    pub fn _lookup(self, interner: &FilePathInterner) -> &str {
        interner._lookup(self)
    }
}
