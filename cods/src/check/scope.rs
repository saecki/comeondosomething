use std::collections::HashMap;
use std::rc::Rc;

use crate::{ast, Ast, BuiltinConst, BuiltinFun, Context, DataType, Ident, IdentSpan};

impl Context {
    pub fn resolve_fun(&self, id: &IdentSpan) -> crate::Result<Rc<ast::Fun>> {
        let name = self.idents.name(id.ident);
        match BuiltinFun::from(name) {
            Some(_) => todo!(),
            None => (),
        }

        match self.scopes.fun(id.ident) {
            Some(f) => Ok(f),
            None => {
                let name = self.idents.name(id.ident);
                Err(crate::Error::UndefinedFun(name.to_owned(), id.span))
            }
        }
    }

    pub fn def_fun(&mut self, fun: Rc<ast::Fun>) -> crate::Result<()> {
        let s = self.scopes.current_mut();
        let id = fun.ident;
        if let Some(f) = s.fun(id.ident) {
            let i_s = f.ident.span;
            let name = self.idents.name(id.ident);
            return Err(crate::Error::RedefinedFun(name.to_owned(), i_s, id.span));
        }

        s.funs.insert(id.ident, fun);

        Ok(())
    }

    pub fn resolve_var(&self, id: &IdentSpan) -> crate::Result<DataType> {
        let name = self.idents.name(id.ident);
        if let Some(b) = BuiltinConst::from(name) {
            return Ok(b.data_type());
        }

        let var = match self.scopes.var(id.ident) {
            Some(v) => v,
            None => return Err(crate::Error::UndefinedVar(name.to_owned(), id.span)),
        };

        if !var.assigned {
            let name = self.idents.name(id.ident);
            return Err(crate::Error::UninitializedVar(
                name.into(),
                var.ident.span,
                id.span,
            ));
        }

        Ok(var.data_type)
    }

    pub fn def_var(&mut self, var: ast::Var) {
        let s = self.scopes.current_mut();
        s.def_var(var);
    }

    pub fn set_var(&mut self, id: &IdentSpan, val: &Ast) -> crate::Result<()> {
        match self.scopes.var_mut(id.ident) {
            Some(v) => {
                if val.data_type != v.data_type {
                    todo!("error")
                }

                if !v.mutable && v.assigned {
                    let name = self.idents.name(id.ident);
                    return Err(crate::Error::ImmutableAssign(
                        name.into(),
                        id.span,
                        val.span,
                    ));
                }

                v.assigned = true;

                Ok(())
            }
            None => {
                let name = self.idents.name(id.ident);
                Err(crate::Error::UndefinedVar(name.to_owned(), id.span))
            }
        }
    }
}

// TODO: separate function and var scope
#[derive(Debug)]
pub struct Scopes {
    scopes: Vec<Scope>,
    len: usize,
}

impl Default for Scopes {
    fn default() -> Self {
        Self {
            scopes: vec![Scope::default()],
            len: 1,
        }
    }
}

impl Scopes {
    pub fn current(&self) -> &Scope {
        self.scopes
            .get(self.len - 1)
            .expect("Expected at least the global scope")
    }

    pub fn current_mut(&mut self) -> &mut Scope {
        self.scopes
            .get_mut(self.len - 1)
            .expect("Expected at least the global scope")
    }

    pub fn iter(&self) -> impl Iterator<Item = &Scope> {
        self.scopes[0..(self.len)].iter()
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut Scope> {
        self.scopes[0..(self.len)].iter_mut()
    }

    pub fn rev(&self) -> impl Iterator<Item = &Scope> {
        self.scopes[0..(self.len)].iter().rev()
    }

    pub fn rev_mut(&mut self) -> impl Iterator<Item = &mut Scope> {
        self.scopes[0..(self.len)].iter_mut().rev()
    }

    pub fn push(&mut self) {
        if self.len >= self.scopes.len() {
            self.scopes.push(Scope::default())
        }
        self.len += 1;
    }

    pub fn pop(&mut self) {
        self.current_mut().clear();
        self.len -= 1;
    }

    pub fn clear(&mut self) {
        for s in self.iter_mut() {
            s.clear();
        }
        self.len = 0;
    }

    fn fun(&self, id: Ident) -> Option<Rc<ast::Fun>> {
        for s in self.rev() {
            if let Some(f) = s.fun(id) {
                return Some(f);
            }
        }
        None
    }

    fn var(&self, id: Ident) -> Option<&ast::Var> {
        for s in self.rev() {
            if let Some(v) = s.var(id) {
                return Some(v);
            }
        }
        None
    }

    fn var_mut(&mut self, id: Ident) -> Option<&mut ast::Var> {
        for s in self.rev_mut() {
            if let Some(v) = s.var_mut(id) {
                return Some(v);
            }
        }
        None
    }
}

#[derive(Debug, Default)]
pub struct Scope {
    funs: HashMap<Ident, Rc<ast::Fun>>,
    vars: HashMap<Ident, ast::Var>,
}

impl Scope {
    pub fn clear(&mut self) {
        self.vars.clear();
        self.funs.clear();
    }

    pub fn fun(&self, id: Ident) -> Option<Rc<ast::Fun>> {
        self.funs.get(&id).map(Rc::clone)
    }

    pub fn vars(&self) -> impl Iterator<Item = &ast::Var> {
        self.vars.values()
    }

    pub fn funs(&self) -> impl Iterator<Item = &Rc<ast::Fun>> {
        self.funs.values()
    }

    pub fn var(&self, id: Ident) -> Option<&ast::Var> {
        self.vars.get(&id)
    }

    pub fn var_mut(&mut self, id: Ident) -> Option<&mut ast::Var> {
        self.vars.get_mut(&id)
    }

    pub fn def_var(&mut self, var: ast::Var) {
        self.vars.insert(var.ident.ident, var);
    }
}
