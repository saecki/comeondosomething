use std::f64::consts;
use std::str::FromStr;
use std::{fmt, result};

pub use calc::*;
pub use display::*;
pub use dummy::*;
pub use error::*;
pub use group::*;
pub use parse::*;
pub use style::*;
pub use token::*;

pub mod calc;
mod display;
mod dummy;
mod error;
mod group;
mod parse;
mod style;
mod token;

pub trait Provider<T: Var> {
    fn var_to_f64(&self, var: T) -> f64;

    fn val_to_f64(&self, val: Val<T>) -> f64 {
        match val {
            Val::Int(i) => i as f64,
            Val::Float(f) => f,
            Val::Var(v) => self.var_to_f64(v),
            Val::TAU => consts::TAU,
            Val::PI => consts::PI,
            Val::E => consts::E,
        }
    }
}

pub trait Var: Copy + fmt::Debug + fmt::Display + FromStr {}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Context<T: Var> {
    pub errors: Vec<Error<T>>,
    pub warnings: Vec<Warning>,
}

impl<T: Var> Context<T> {
    pub fn new() -> Self {
        Self {
            errors: Vec::new(),
            warnings: Vec::new(),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum PlainVal {
    Int(i128),
    Float(f64),
    TAU,
    PI,
    E,
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

impl PlainVal {
    pub fn to_f64(&self) -> f64 {
        match self {
            Self::Int(i) => *i as f64,
            Self::Float(f) => *f,
            Self::TAU => consts::TAU,
            Self::PI => consts::PI,
            Self::E => consts::E,
        }
    }
}

impl<T: Var> fmt::Display for Val<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Val::Int(n) => write!(f, "{}", n),
            Val::Float(n) => {
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
            Val::Var(v) => write!(f, "{}", v),
            Val::TAU => write!(f, "τ"),
            Val::PI => write!(f, "π"),
            Val::E => write!(f, "e"),
        }
    }
}

pub fn calc(string: &str) -> (result::Result<PlainVal, ()>, Context<DummyVar>) {
    let (calc, ctx) = calc_with(&DummyProvider, string);
    let calc = calc.map(|v| match v {
        Val::Int(i) => PlainVal::Int(i),
        Val::Float(f) => PlainVal::Float(f),
        Val::TAU => PlainVal::TAU,
        Val::PI => PlainVal::PI,
        Val::E => PlainVal::E,
        Val::Var(_) => unreachable!(),
    });
    (calc, ctx)
}

pub fn calc_with<T: Var>(
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

pub fn parse<T: Var>(string: &str) -> (result::Result<Calc<T>, ()>, Context<T>) {
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
