use std::fmt;

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
}

impl fmt::Display for PlainVal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PlainVal::Int(n) => write!(f, "{}", n),
            PlainVal::Float(n) => {
                if n.is_infinite() {
                    if n.is_sign_positive() {
                        write!(f, "infinity")
                    } else {
                        write!(f, "-infinity")
                    }
                } else if n.is_nan() {
                    write!(f, "undefined")
                } else {
                    write!(f, "{}", n)
                }
            }
            PlainVal::TAU => write!(f, "τ"),
            PlainVal::PI => write!(f, "π"),
            PlainVal::E => write!(f, "e"),
        }
    }
}

pub fn eval(string: &str) -> crate::Result<Option<PlainVal>> {
    let mut ctx = Context::default();
    ctx.parse_and_eval(string)
}

impl Context {
    pub fn parse_and_eval(&mut self, string: &str) -> crate::Result<Option<PlainVal>> {
        let calc = self.parse_str(string)?;
        if !self.errors.is_empty() {
            return Err(self.errors.remove(0));
        }

        let val = self.eval_all(&calc)?;
        Ok(val)
    }

    pub fn parse_str(&mut self, string: &str) -> crate::Result<Vec<Calc>> {
        let tokens = self.tokenize(string.as_ref())?;
        let items = self.group(&tokens)?;
        let calc = self.parse(&items)?;
        Ok(calc)
    }
}
