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

#[derive(Debug)]
pub struct Context {
    pub idents: Vec<String>,
    pub scopes: Vec<Scope>,
    pub errors: Vec<crate::Error>,
    pub warnings: Vec<crate::Warning>,
}

impl Default for Context {
    fn default() -> Self {
        Self {
            scopes: vec![Scope::default()],
            idents: Vec::new(),
            errors: Vec::new(),
            warnings: Vec::new(),
        }
    }
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

    pub fn push_ident(&mut self, name: &str) -> Ident {
        for (id, n) in self.idents.iter().enumerate() {
            if n == name {
                return Ident(id);
            }
        }

        let id = self.idents.len();
        self.idents.push(name.to_owned());
        Ident(id)
    }

    pub fn ident_name(&self, id: Ident) -> &str {
        &self.idents[id.0]
    }
}

pub fn eval(input: &str) -> crate::Result<Option<Val>> {
    let mut ctx = Context::default();
    ctx.parse_and_eval(input)
}
