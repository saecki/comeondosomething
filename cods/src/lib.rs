pub use check::*;
pub use error::*;
pub use eval::*;
pub use group::*;
pub use ident::*;
pub use lex::*;
pub use parse::*;
pub use span::*;
pub use stdio::*;
pub use util::*;

mod check;
mod error;
mod eval;
mod group;
mod ident;
mod lex;
mod parse;
mod span;
mod stdio;
mod util;

#[derive(Debug, Default)]
pub struct Context {
    pub idents: Idents,
    pub stdio: Stdio,
    pub errors: Vec<crate::Error>,
    pub warnings: Vec<crate::Warning>,
}

impl Context {
    pub fn clear(&mut self) {
        self.idents.clear();
        self.errors.clear();
        self.warnings.clear();
    }

    pub fn clear_errors(&mut self) {
        self.errors.clear();
        self.warnings.clear();
    }

    pub fn parse_and_eval(&mut self, input: &str) -> crate::Result<Val> {
        let asts = self.parse_str(input)?;
        if !self.errors.is_empty() {
            return Err(self.errors.remove(0));
        }

        let val = eval::eval(&asts)?;
        Ok(val)
    }

    pub fn parse_str(&mut self, input: &str) -> crate::Result<Asts> {
        let tokens = self.lex(input.as_ref())?;
        let items = self.group(tokens)?;
        let csts = self.parse(items)?;
        let asts = self.check(csts)?;
        Ok(asts)
    }
}

pub fn eval_str(input: &str) -> crate::Result<Val> {
    let mut ctx = Context::default();
    ctx.parse_and_eval(input)
}
