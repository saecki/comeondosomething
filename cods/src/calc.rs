use std::convert::TryFrom;
use std::f64::consts;
use std::fmt;

use crate::{Num, Range, Val};

impl fmt::Display for Val {
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
    Neg(Box<Calc>, Range),
    Add(Box<Calc>, Box<Calc>),
    Sub(Box<Calc>, Box<Calc>),
    Mul(Box<Calc>, Box<Calc>),
    Div(Box<Calc>, Box<Calc>),
    Pow(Box<Calc>, Box<Calc>, Range),
    Ln(Box<Calc>, Range),
    Log(Box<Calc>, Box<Calc>, Range),
    Sqrt(Box<Calc>, Range),
    Ncr(Box<Calc>, Box<Calc>, Range),
    Sin(Box<Calc>, Range),
    Cos(Box<Calc>, Range),
    Tan(Box<Calc>, Range),
    Asin(Box<Calc>, Range),
    Acos(Box<Calc>, Range),
    Atan(Box<Calc>, Range),
    Degree(Box<Calc>, Range),
    Factorial(Box<Calc>, Range),
}

impl Calc {
    pub fn calc(&self) -> crate::Result<Val> {
        Ok(self.calc_num()?.val)
    }

    // TODO use checked variants of arithmetic operations
    fn calc_num(&self) -> crate::Result<Num> {
        match self {
            Self::Error(r) => Err(crate::Error::Parsing(*r)),
            Self::Num(n) => Ok(*n),
            Self::Neg(a, r) => neg(a.calc_num()?, *r),
            Self::Add(a, b) => add(a.calc_num()?, b.calc_num()?),
            Self::Sub(a, b) => sub(a.calc_num()?, b.calc_num()?),
            Self::Mul(a, b) => mul(a.calc_num()?, b.calc_num()?),
            Self::Div(a, b) => div(a.calc_num()?, b.calc_num()?),
            Self::Pow(a, b, r) => pow(a.calc_num()?, b.calc_num()?, *r),
            Self::Ln(a, r) => ln(a.calc_num()?, *r),
            Self::Log(a, b, r) => log(a.calc_num()?, b.calc_num()?, *r),
            Self::Sqrt(a, r) => sqrt(a.calc_num()?, *r),
            Self::Ncr(a, b, r) => ncr(a.calc_num()?, b.calc_num()?, *r),
            Self::Sin(a, r) => sin(a.calc_num()?, *r),
            Self::Cos(a, r) => cos(a.calc_num()?, *r),
            Self::Tan(a, r) => tan(a.calc_num()?, *r),
            Self::Asin(a, r) => asin(a.calc_num()?, *r),
            Self::Acos(a, r) => acos(a.calc_num()?, *r),
            Self::Atan(a, r) => atan(a.calc_num()?, *r),
            Self::Degree(a, r) => degree(a.calc_num()?, *r), // TODO add rad modifier and require a typed angle value as input for trigeometrical functions
            Self::Factorial(a, r) => factorial(a.calc_num()?, *r),
        }
        .map(|mut n| {
            n.val = n.val.maybe_int();
            n
        })
    }
}

fn neg(n: Num, range: Range) -> crate::Result<Num> {
    let val = match n.val {
        Val::Int(i) => Val::Int(-i),
        v => Val::Float(-v.to_f64()),
    };
    Ok(Num { val, range })
}

fn add(n1: Num, n2: Num) -> crate::Result<Num> {
    let val = match (n1.val, n2.val) {
        (Val::Int(a), Val::Int(b)) => match a.checked_add(b) {
            Some(v) => Val::Int(v),
            None => return Err(crate::Error::AddOverflow(n1, n2)),
        },
        (a, b) => Val::Float(a.to_f64() + b.to_f64()),
    };
    let range = Range::span(n1.range, n2.range);
    Ok(Num { val, range })
}

fn sub(n1: Num, n2: Num) -> crate::Result<Num> {
    let val = match (n1.val, n2.val) {
        (Val::Int(a), Val::Int(b)) => match a.checked_sub(b) {
            Some(v) => Val::Int(v),
            None => return Err(crate::Error::SubOverflow(n1, n2)),
        },
        (a, b) => Val::Float(a.to_f64() - b.to_f64()),
    };
    let range = Range::span(n1.range, n2.range);
    Ok(Num { val, range })
}

fn mul(n1: Num, n2: Num) -> crate::Result<Num> {
    let val = match (n1.val, n2.val) {
        (Val::Int(a), Val::Int(b)) => match a.checked_mul(b) {
            Some(v) => Val::Int(v),
            None => return Err(crate::Error::MulOverflow(n1, n2)),
        },
        (a, b) => Val::Float(a.to_f64() * b.to_f64()),
    };
    let range = Range::span(n1.range, n2.range);
    Ok(Num { val, range })
}

fn div(n1: Num, n2: Num) -> crate::Result<Num> {
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
    let range = Range::span(n1.range, n2.range);
    Ok(Num { val, range })
}

fn pow(n1: Num, n2: Num, range: Range) -> crate::Result<Num> {
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

fn ln(n: Num, range: Range) -> crate::Result<Num> {
    let val = Val::Float(n.val.to_f64().ln());
    Ok(Num { val, range })
}

fn log(base: Num, n: Num, range: Range) -> crate::Result<Num> {
    let val = Val::Float(n.val.to_f64().log(base.val.to_f64()));
    Ok(Num { val, range })
}

fn sqrt(n: Num, range: Range) -> crate::Result<Num> {
    let val = Val::Float(n.val.to_f64().sqrt());
    Ok(Num { val, range })
}

fn ncr(a: Num, b: Num, range: Range) -> crate::Result<Num> {
    let divident = factorial(a, range)?;
    let divisor = mul(factorial(b, range)?, factorial(sub(a, b)?, range)?)?;
    let val = div(divident, divisor)?.val;
    Ok(Num { val, range })
}

fn sin(n: Num, range: Range) -> crate::Result<Num> {
    let val = Val::Float(n.val.to_f64().sin());
    Ok(Num { val, range })
}

fn cos(n: Num, range: Range) -> crate::Result<Num> {
    let val = Val::Float(n.val.to_f64().cos());
    Ok(Num { val, range })
}

fn tan(n: Num, range: Range) -> crate::Result<Num> {
    let val = Val::Float(n.val.to_f64().tan());
    Ok(Num { val, range })
}

fn asin(n: Num, range: Range) -> crate::Result<Num> {
    let val = Val::Float(n.val.to_f64().asin());
    Ok(Num { val, range })
}

fn acos(n: Num, range: Range) -> crate::Result<Num> {
    let val = Val::Float(n.val.to_f64().acos());
    Ok(Num { val, range })
}

fn atan(n: Num, range: Range) -> crate::Result<Num> {
    let val = Val::Float(n.val.to_f64().atan());
    Ok(Num { val, range })
}

fn degree(n: Num, range: Range) -> crate::Result<Num> {
    let val = Val::Float(n.val.to_f64().to_radians());
    Ok(Num { val, range })
}

fn factorial(n: Num, range: Range) -> crate::Result<Num> {
    let val = match n.val {
        Val::Int(i) => {
            if i < 0 {
                return Err(crate::Error::NegativeFactorial(n.range));
            } else {
                Val::Int((1..=i).product())
            }
        }
        v => {
            let f = v.to_f64();
            if f < 0.0 {
                return Err(crate::Error::NegativeFactorial(n.range));
            } else {
                todo!("Factorial of fractions is not yet implemented")
            }
        }
    };
    Ok(Num { val, range })
}
