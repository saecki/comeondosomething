use std::fmt;

pub use calc::*;
pub use display::*;
pub use dummy::*;
pub use error::*;
pub use ext::*;
pub use group::*;
pub use parse::*;
pub use style::*;
pub use token::*;

pub mod calc;
mod display;
mod dummy;
mod error;
mod ext;
mod group;
mod parse;
mod style;
mod token;

#[derive(Clone, Debug, PartialEq)]
pub struct Context<T: Ext, P: Provider<T>> {
    pub provider: P,
    pub vars: Vec<Var<T>>,
    pub errors: Vec<crate::Error<T>>,
    pub warnings: Vec<crate::Warning>,
}

impl Default for Context<ExtDummy, DummyProvider> {
    fn default() -> Self {
        Self::new(DummyProvider)
    }
}
impl<T: Ext, P: Provider<T>> Context<T, P> {
    pub fn new(provider: P) -> Self {
        Self {
            provider,
            vars: Vec::new(),
            errors: Vec::new(),
            warnings: Vec::new(),
        }
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

pub fn calc(string: &str) -> crate::Result<PlainVal, ExtDummy> {
    let mut ctx = Context::new(DummyProvider);
    ctx.calc(string)
}

impl<T: Ext, P: Provider<T>> Context<T, P> {
    pub fn calc(&mut self, string: &str) -> crate::Result<PlainVal, T> {
        let calc = self.parse_str(string)?;
        if !self.errors.is_empty() {
            return Err(self.errors.remove(0));
        }

        let val = self.eval(&calc)?;
        Ok(val)
    }

    pub fn parse_str(&mut self, string: &str) -> crate::Result<Calc<T>, T> {
        let tokens = self.tokenize(string.as_ref())?;
        let items = self.group(&tokens)?;
        let calc = self.parse(&items)?;
        Ok(calc)
    }
}
