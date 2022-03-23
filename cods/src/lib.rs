pub use error::*;
pub use eval::*;
pub use group::*;
pub use lex::*;
pub use parse::*;
pub use types::*;
pub use util::*;

mod error;
mod eval;
mod group;
mod lex;
mod parse;
mod types;
mod util;

#[derive(Debug, Default)]
pub struct Context {
    pub scope: Scope,
    pub errors: Vec<crate::Error>,
    pub warnings: Vec<crate::Warning>,
}

impl Context {
    pub fn clear_errors(&mut self) {
        self.errors.clear();
        self.warnings.clear();
    }

    pub fn parse_and_eval(&mut self, input: &str) -> crate::Result<Option<Val>> {
        let asts = self.parse_str(input)?;
        if !self.errors.is_empty() {
            return Err(self.errors.remove(0));
        }

        let val = self.eval_all(&asts)?;
        Ok(val)
    }

    pub fn parse_str(&mut self, input: &str) -> crate::Result<Vec<Ast>> {
        let tokens = self.lex(input.as_ref())?;
        let items = self.group(tokens)?;
        let asts = self.parse(items)?;
        Ok(asts)
    }
}

#[derive(Debug, Default)]
pub struct Scope {
    pub vars: Vec<Var>,
}

impl Scope {
    pub fn clear(&mut self) {
        self.vars.clear();
    }

    pub fn var(&self, id: VarId) -> &Var {
        &self.vars[id.0]
    }

    pub fn set_var(&mut self, id: VarId, val: Option<Val>) {
        // TODO const vars
        self.vars[id.0].value = val;
    }

    pub fn declare_var(&mut self, name: &str) -> VarId {
        for (id, v) in self.vars.iter().enumerate() {
            if v.name == name {
                return VarId(id);
            }
        }

        let id = self.vars.len();
        self.vars.push(Var::new(name.to_owned(), None));
        VarId(id)
    }

    pub fn add_var(&mut self, name: &str, val: Option<Val>) -> VarId {
        let id = self.declare_var(name);
        self.set_var(id, val);
        id
    }
}

pub fn eval(input: &str) -> crate::Result<Option<Val>> {
    let mut ctx = Context::default();
    ctx.parse_and_eval(input)
}
