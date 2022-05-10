use core::fmt;
use std::borrow::Borrow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::hash;
use std::rc::Rc;

pub trait Symbol: Clone + fmt::Debug + Eq + PartialEq + hash::Hash {}

pub trait Value {}

#[derive(Debug, Clone)]
pub struct SymbolTable<'ctx, K: Symbol, V: Value> {
    pub outer: Option<Rc<RefCell<SymbolTable<'ctx, K, V>>>>,
    store: HashMap<&'ctx K, &'ctx V>,
}

impl<'ctx, K: Symbol, V: Value> SymbolTable<'ctx, K, V> {
    pub fn new() -> Self {
        Self {
            outer: None,
            store: HashMap::new(),
        }
    }

    pub fn new_enclosed(outer: Rc<RefCell<Self>>) -> SymbolTable<'ctx, K, V> {
        Self {
            outer: Some(outer),
            store: HashMap::new(),
        }
    }

    pub fn define(&mut self, symbol: &'ctx K, value: &'ctx V) {
        self.store.insert(symbol, value);
    }

    pub fn resolve(&self, symbol: &K) -> Option<&'ctx V> {
        let result = self.store.get(symbol).copied();
        if result.is_some() {
            return result;
        }

        if let Some(outer) = self.outer.borrow() {
            let outer = Rc::clone(outer);
            let borrow = outer.borrow_mut();
            borrow.resolve(symbol)
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    impl Symbol for String {}
    impl Value for i64 {}

    #[test]
    fn test_define_and_resolve() {
        let mut global_table = SymbolTable::new();

        let symbol_a = "a".to_string();
        let value_a: i64 = 0;
        global_table.define(&symbol_a, &value_a);
        assert_eq!(global_table.resolve(&symbol_a), Some(&value_a));

        let value_b: i64 = 1;
        let symbol_b = "b".to_string();

        assert_eq!(global_table.resolve(&symbol_a), Some(&value_a));
        global_table.define(&symbol_b, &value_b);
        assert_eq!(global_table.resolve(&symbol_b), Some(&value_b));
    }

    #[test]
    fn test_resolve_nested_local() {
        let mut global_table = SymbolTable::new();

        let symbol_a = "a".to_string();
        let value_a: i64 = 0;

        global_table.define(&symbol_a, &value_a);
        assert_eq!(global_table.resolve(&symbol_a), Some(&value_a));

        let global_table = Rc::new(RefCell::new(global_table));
        let mut local_table = SymbolTable::new_enclosed(Rc::clone(&global_table));

        let symbol_b = "b".to_string();
        let value_b: i64 = 1;

        local_table.define(&symbol_b, &value_b);

        assert_eq!(local_table.resolve(&symbol_a), Some(&value_a));
        assert_eq!(local_table.resolve(&symbol_b), Some(&value_b));

        assert_eq!(
            Rc::clone(&global_table).borrow_mut().resolve(&symbol_b),
            None
        );
    }

    #[test]
    fn test_shadowing() {
        let mut global_table = SymbolTable::new();

        let symbol_a = "a".to_string();
        let value_a: i64 = 0;

        global_table.define(&symbol_a, &value_a);
        assert_eq!(global_table.resolve(&symbol_a), Some(&value_a));

        let global_table = Rc::new(RefCell::new(global_table));
        let mut local_table = SymbolTable::new_enclosed(Rc::clone(&global_table));

        let value_b: i64 = 1;

        local_table.define(&symbol_a, &value_b);

        assert_eq!(local_table.resolve(&symbol_a), Some(&value_b));

        assert_eq!(
            Rc::clone(&global_table).borrow_mut().resolve(&symbol_a),
            Some(&value_a)
        );
    }
}
