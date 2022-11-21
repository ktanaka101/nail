#[derive(Debug, Default)]
pub struct Interner(lasso::Rodeo);
impl Interner {
    pub fn new() -> Self {
        Self(lasso::Rodeo::new())
    }

    pub fn intern(&mut self, string: &str) -> Key {
        Key(self.0.get_or_intern(string))
    }

    pub fn lookup(&self, key: Key) -> &str {
        self.0.resolve(&key.0)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Key(lasso::Spur);
