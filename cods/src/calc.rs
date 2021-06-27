use std::convert::TryFrom;
use std::f64::consts;
use std::fmt;

use crate::{span, Num, Range};

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Val {
    Int(i128),
    Float(f64),
    TAU,
    PI,
    E,
}

impl fmt::Display for Val {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Val::Int(n) => write!(f, "{}", n),
            Val::Float(n) => write!(f, "{}", n),
            Val::TAU => write!(f, "τ"),
            Val::PI => write!(f, "π"),
            Val::E => write!(f, "e"),
        }
    }
}

impl Val {
    pub const fn to_f64(self) -> f64 {
        match self {
            Self::Int(i) => i as f64,
            Self::Float(f) => f,
            Self::TAU => consts::TAU,
            Self::PI => consts::PI,
            Self::E => consts::E,
        }
    }

    pub fn maybe_int(self) -> Self {
        match self {
            Self::Float(f) => {
                let i = f as i128;
                #[allow(clippy::float_cmp)]
                if i as f64 == f {
                    Self::Int(i)
                } else {
                    Self::Float(f)
                }
            }
            v => v,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Calc {
    Error(Range),
    Num(Num),
    Add(Box<Calc>, Box<Calc>),
    Sub(Box<Calc>, Box<Calc>),
    Mul(Box<Calc>, Box<Calc>),
    Div(Box<Calc>, Box<Calc>),
    Pow(Box<Calc>, Box<Calc>, Range),
    Ln(Box<Calc>, Range),
    Log(Box<Calc>, Box<Calc>, Range),
    Sqrt(Box<Calc>, Range),
    Sin(Box<Calc>, Range),
    Cos(Box<Calc>, Range),
    Tan(Box<Calc>, Range),
    Degree(Box<Calc>, Range),
    Factorial(Box<Calc>, Range),
}

impl Calc {
    pub fn calc(&self) -> crate::Result<Val> {
        Ok(self._calc()?.val)
    }

    // TODO use checked variants of arithmetic operations
    fn _calc(&self) -> crate::Result<Num> {
        match self {
            Self::Error(r) => Err(crate::Error::Parsing(*r)),
            Self::Num(n) => Ok(*n),
            Self::Add(a, b) => add(a._calc()?, b._calc()?),
            Self::Sub(a, b) => sub(a._calc()?, b._calc()?),
            Self::Mul(a, b) => mul(a._calc()?, b._calc()?),
            Self::Div(a, b) => div(a._calc()?, b._calc()?),
            Self::Pow(a, b, r) => pow(a._calc()?, b._calc()?, *r),
            Self::Ln(a, r) => ln(a._calc()?, *r),
            Self::Log(a, b, r) => log(a._calc()?, b._calc()?, *r),
            Self::Sqrt(a, r) => a._calc()?.sqrt(*r),
            Self::Sin(a, r) => a._calc()?.sin(*r),
            Self::Cos(a, r) => a._calc()?.cos(*r),
            Self::Tan(a, r) => a._calc()?.tan(*r),
            Self::Degree(a, r) => a._calc()?.degree(*r), // TODO add rad modifier and require a typed angle value as input for trigeometrical functions
            Self::Factorial(a, r) => a._calc()?.factorial(*r),
        }
        .map(|mut n| {
            n.val = n.val.maybe_int();
            n
        })
    }
}

pub fn add(n1: Num, n2: Num) -> crate::Result<Num> {
    let val = match (n1.val, n2.val) {
        (Val::Int(a), Val::Int(b)) => Val::Int(a + b),
        (a, b) => Val::Float(a.to_f64() + b.to_f64()),
    };
    let range = span(n1.range, n2.range);
    Ok(Num { val, range })
}

pub fn sub(n1: Num, n2: Num) -> crate::Result<Num> {
    let val = match (n1.val, n2.val) {
        (Val::Int(a), Val::Int(b)) => Val::Int(a - b),
        (a, b) => Val::Float(a.to_f64() - b.to_f64()),
    };
    let range = span(n1.range, n2.range);
    Ok(Num { val, range })
}

pub fn mul(n1: Num, n2: Num) -> crate::Result<Num> {
    let val = match (n1.val, n2.val) {
        (Val::Int(a), Val::Int(b)) => Val::Int(a * b),
        (a, b) => Val::Float(a.to_f64() * b.to_f64()),
    };
    let range = span(n1.range, n2.range);
    Ok(Num { val, range })
}

pub fn div(n1: Num, n2: Num) -> crate::Result<Num> {
    let val = match (n1.val, n2.val) {
        (Val::Int(a), Val::Int(b)) => {
            if b == 0 {
                return Err(crate::Error::DivideByZero(n1, n2));
            } else if a % b == 0 {
                Val::Int(a / b)
            } else {
                Val::Float(a as f64 / b as f64)
            }
        }
        (a, b) => {
            let divisor = b.to_f64();
            if divisor == 0.0 {
                return Err(crate::Error::DivideByZero(n1, n2));
            } else {
                Val::Float(a.to_f64() / divisor)
            }
        }
    };
    let range = span(n1.range, n2.range);
    Ok(Num { val, range })
}

pub fn pow(n1: Num, n2: Num, range: Range) -> crate::Result<Num> {
    let val = match (n1.val, n2.val) {
        (Val::Int(a), Val::Int(b)) => {
            if let Ok(exp) = u32::try_from(b) {
                Val::Int(a.pow(exp))
            } else if let Ok(exp) = i32::try_from(b) {
                Val::Float((a as f64).powi(exp))
            } else {
                Val::Float((a as f64).powf(b as f64))
            }
        }
        (Val::Float(a), Val::Int(b)) => {
            if let Ok(exp) = i32::try_from(b) {
                Val::Float((a as f64).powi(exp))
            } else {
                Val::Float((a as f64).powf(b as f64))
            }
        }
        (a, b) => Val::Float(a.to_f64().powf(b.to_f64())),
    };
    Ok(Num { val, range })
}

pub fn ln(n: Num, range: Range) -> crate::Result<Num> {
    let val = Val::Float(n.val.to_f64().ln());
    Ok(Num { val, range })
}

pub fn log(base: Num, n: Num, range: Range) -> crate::Result<Num> {
    let val = Val::Float(n.val.to_f64().log(base.val.to_f64()));
    Ok(Num { val, range })
}

impl Num {
    pub fn sqrt(self, range: Range) -> crate::Result<Self> {
        let val = Val::Float(self.val.to_f64().sqrt());
        Ok(Num { val, range })
    }

    pub fn sin(self, range: Range) -> crate::Result<Self> {
        let val = Val::Float(self.val.to_f64().sin());
        Ok(Num { val, range })
    }

    pub fn cos(self, range: Range) -> crate::Result<Self> {
        let val = Val::Float(self.val.to_f64().cos());
        Ok(Num { val, range })
    }

    pub fn tan(self, range: Range) -> crate::Result<Self> {
        let val = Val::Float(self.val.to_f64().tan());
        Ok(Num { val, range })
    }

    pub fn degree(self, range: Range) -> crate::Result<Self> {
        let val = Val::Float(self.val.to_f64().to_radians());
        Ok(Num { val, range })
    }

    pub fn factorial(self, range: Range) -> crate::Result<Self> {
        let val = match self.val {
            Val::Int(i) => {
                if i < 0 {
                    return Err(crate::Error::NegativeFactorial(self.range));
                } else {
                    Val::Int((1..=i).reduce(|a, b| a * b).unwrap_or(1))
                }
            }
            v => {
                let f = v.to_f64();
                if f < 0.0 {
                    return Err(crate::Error::NegativeFactorial(self.range));
                } else {
                    todo!("Factorial of fractions is not yet implemented")
                }
            }
        };
        Ok(Num { val, range })
    }
}
