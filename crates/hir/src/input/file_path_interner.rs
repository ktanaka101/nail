pub struct FilePathInterner(lasso::Rodeo);
impl FilePathInterner {
    pub(crate) fn new() -> Self {
        Self(lasso::Rodeo::new())
    }

    pub(crate) fn intern(&mut self, path: &str) -> FilePath {
        FilePath(self.0.get_or_intern(path))
    }

    #[allow(dead_code)]
    pub(crate) fn get(&self, path: &str) -> Option<FilePath> {
        self.0.get(path).map(FilePath)
    }

    #[allow(dead_code)]
    pub fn lookup(&self, path: FilePath) -> &str {
        self.0.resolve(&path.0)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FilePath(lasso::Spur);
impl FilePath {
    #[allow(dead_code)]
    pub fn lookup(self, interner: &FilePathInterner) -> &str {
        interner.lookup(self)
    }
}
