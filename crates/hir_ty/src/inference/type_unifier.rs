use std::collections::HashMap;

use super::{environment::InferenceError, types::Monotype};

#[derive(Default, Debug)]
pub struct TypeUnifier {
    pub(crate) nodes: HashMap<Monotype, Node>,
    pub(crate) errors: Vec<super::environment::InferenceError>,
}

impl TypeUnifier {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn find(&mut self, ty: &Monotype) -> Monotype {
        let node = self.nodes.get(ty);
        if let Some(node) = node {
            node.topmost_parent().value
        } else {
            self.nodes.insert(ty.clone(), Node::new(ty.clone()));
            ty.clone()
        }
    }

    pub fn unify(&mut self, a: &Monotype, b: &Monotype) {
        let a_rep = self.find(a);
        let b_rep = self.find(b);

        if a_rep == b_rep {
            return;
        }

        match (&a_rep, &b_rep) {
            (
                Monotype::Function {
                    args: a_args,
                    return_type: a_return_type,
                },
                Monotype::Function {
                    args: b_args,
                    return_type: b_return_type,
                },
            ) => {
                if a_args.len() != b_args.len() {
                    self.errors.push(InferenceError::TypeMismatch {
                        expected: a_rep,
                        actual: b_rep,
                    });
                    return;
                }

                for (a_arg, b_arg) in a_args.iter().zip(b_args.iter()) {
                    self.unify(a_arg, b_arg);
                }

                self.unify(a_return_type, b_return_type);
            }
            (Monotype::Variable(_), b_rep) => self.unify_var(&a_rep, b_rep),
            (a_rep, Monotype::Variable(_)) => self.unify_var(&b_rep, a_rep),
            (_, _) => {
                self.errors.push(InferenceError::TypeMismatch {
                    expected: a_rep,
                    actual: b_rep,
                });
            }
        }
    }

    fn unify_var(&mut self, type_var: &Monotype, term: &Monotype) {
        assert!(matches!(type_var, Monotype::Variable(_)));

        let value = Some(Box::new(self.nodes.get(term).unwrap().clone()));
        let node = self.nodes.get_mut(type_var);
        node.unwrap().parent = value;
    }
}

#[derive(Debug, Clone)]
pub struct Node {
    value: Monotype,
    parent: Option<Box<Node>>,
}

impl Node {
    fn new(ty: Monotype) -> Self {
        Node {
            value: ty,
            parent: None,
        }
    }

    fn topmost_parent(&self) -> Self {
        if let Some(node) = &self.parent {
            node.topmost_parent()
        } else {
            self.clone()
        }
    }
}
