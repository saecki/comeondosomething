pub use error::*;
pub use eval::*;
pub use group::*;
pub use ident::*;
pub use lex::*;
pub use parse::*;
pub use stdio::*;
pub use types::*;
pub use util::*;

mod error;
mod eval;
mod group;
mod ident;
mod lex;
mod parse;
mod stdio;
mod types;
mod util;

#[derive(Debug, Default)]
pub struct Context {
    pub idents: Idents,
    pub scopes: Scopes,
    pub stdio: Stdio,
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

pub fn eval(input: &str) -> crate::Result<Option<Val>> {
    let mut ctx = Context::default();
    ctx.parse_and_eval(input)
}
