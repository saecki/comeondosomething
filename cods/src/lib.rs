pub use error::*;
pub use eval::*;
pub use group::*;
pub use parse::*;
pub use lex::*;

mod error;
mod eval;
mod group;
mod parse;
mod lex;

#[derive(Debug, Default)]
pub struct Context {
    pub vars: Vec<Var>,
    pub errors: Vec<crate::Error>,
    pub warnings: Vec<crate::Warning>,
}

impl Context {
    pub fn clear(&mut self) {
        self.clear_vars();
        self.clear_errors();
    }

    pub fn clear_vars(&mut self) {
        self.vars.clear();
    }

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

pub fn eval(input: &str) -> crate::Result<Option<Val>> {
    let mut ctx = Context::default();
    ctx.parse_and_eval(input)
}
