pub use error::*;
pub use eval::*;
pub use ext::*;
pub use group::*;
pub use parse::*;
pub use token::*;

mod error;
mod eval;
mod ext;
mod group;
mod parse;
mod token;

#[derive(Debug, Default)]
pub struct Context {
    pub providers: Vec<Box<dyn Provider>>,
    pub vars: Vec<Var>,
    pub errors: Vec<crate::Error>,
    pub warnings: Vec<crate::Warning>,
}

impl Context {
    pub fn new(providers: Vec<Box<dyn Provider>>) -> Self {
        Self {
            providers,
            vars: Vec::new(),
            errors: Vec::new(),
            warnings: Vec::new(),
        }
    }

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

    pub fn parse_and_eval(&mut self, input: &str) -> crate::Result<Option<Data>> {
        let exprs = self.parse_str(input)?;
        if !self.errors.is_empty() {
            return Err(self.errors.remove(0));
        }

        let val = self.eval_all(&exprs)?;
        Ok(val)
    }

    pub fn parse_str(&mut self, input: &str) -> crate::Result<Vec<Expr>> {
        let tokens = self.tokenize(input.as_ref())?;
        let items = self.group(&tokens)?;
        let exprs = self.parse(&items)?;
        Ok(exprs)
    }
}

pub fn eval(input: &str) -> crate::Result<Option<Data>> {
    let mut ctx = Context::default();
    ctx.parse_and_eval(input)
}
