use std::{fmt, result};

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

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Context<T: Ext> {
    pub errors: Vec<crate::Error<T>>,
    pub warnings: Vec<crate::Warning>,
}

impl<T: Ext> Context<T> {
    pub fn new() -> Self {
        Self {
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

impl<T: Ext> fmt::Display for Val<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Val::Plain(p) => write!(f, "{}", p),
            Val::Ext(e) => write!(f, "{}", e),
        }
    }
}

pub fn calc(string: &str) -> (result::Result<PlainVal, ()>, Context<ExtDummy>) {
    let (calc, ctx) = calc_with(&DummyProvider, string);
    let calc = calc.map(|v| match v {
        Val::Plain(p) => p,
        Val::Ext(_) => unreachable!(),
    });
    (calc, ctx)
}

pub fn calc_with<T: Ext>(
    provider: &impl Provider<T>,
    string: &str,
) -> (result::Result<Val<T>, ()>, Context<T>) {
    let (calc, mut ctx) = parse(string);

    let calc = match calc {
        Ok(c) if ctx.errors.is_empty() => c,
        _ => return (Err(()), ctx),
    };

    match calc.eval(provider) {
        Err(e) => {
            ctx.errors.push(e);
            (Err(()), ctx)
        }
        Ok(v) => (Ok(v), ctx),
    }
}

pub fn parse<T: Ext>(string: &str) -> (result::Result<Calc<T>, ()>, Context<T>) {
    let mut ctx = Context::new();

    let tokens = match ctx.tokenize(string.as_ref()) {
        Err(e) => {
            ctx.errors.push(e);
            return (Err(()), ctx);
        }
        Ok(t) => t,
    };

    let items = match ctx.group(&tokens) {
        Err(e) => {
            ctx.errors.push(e);
            return (Err(()), ctx);
        }
        Ok(i) => i,
    };

    let calc = match ctx.parse(&items) {
        Err(e) => {
            ctx.errors.push(e);
            return (Err(()), ctx);
        }
        Ok(c) => c,
    };

    (Ok(calc), ctx)
}
