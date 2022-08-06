use crate::ast::Fun;

#[derive(Clone, Debug, Default)]
pub struct Funs {
    items: Vec<Option<Fun>>,
}

impl Funs {
    pub fn push(&mut self) -> FunRef {
        let fun_ref = self.items.len();
        self.items.push(None);
        FunRef(fun_ref)
    }

    pub fn init(&mut self, fun_ref: FunRef, fun: Fun) {
        if self
            .items
            .get_mut(fun_ref.0)
            .expect("Expected function reference to be in bounds")
            .replace(fun)
            .is_some()
        {
            panic!("Expected referenced function to uninitialized");
        }
    }

    pub fn get(&self, fun_ref: FunRef) -> &Fun {
        self.items
            .get(fun_ref.0)
            .expect("Expected function reference to be in bounds")
            .as_ref()
            .expect("Expected value to be initialized")
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct FunRef(pub usize);
