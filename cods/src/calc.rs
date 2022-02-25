use std::convert::TryFrom;

use crate::{Num, Provider, Range, Val, Var};

impl<T: Var> Val<T> {
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
pub enum Calc<T: Var> {
    Error(Range),
    Num(Num<T>),
    Neg(Box<Calc<T>>, Range),
    Add(Box<Calc<T>>, Box<Calc<T>>),
    Sub(Box<Calc<T>>, Box<Calc<T>>),
    Mul(Box<Calc<T>>, Box<Calc<T>>),
    Div(Box<Calc<T>>, Box<Calc<T>>),
    Rem(Box<Calc<T>>, Box<Calc<T>>),
    Pow(Box<Calc<T>>, Box<Calc<T>>, Range),
    Ln(Box<Calc<T>>, Range),
    Log(Box<Calc<T>>, Box<Calc<T>>, Range),
    Sqrt(Box<Calc<T>>, Range),
    Ncr(Box<Calc<T>>, Box<Calc<T>>, Range),
    Sin(Box<Calc<T>>, Range),
    Cos(Box<Calc<T>>, Range),
    Tan(Box<Calc<T>>, Range),
    Asin(Box<Calc<T>>, Range),
    Acos(Box<Calc<T>>, Range),
    Atan(Box<Calc<T>>, Range),
    Gcd(Box<Calc<T>>, Box<Calc<T>>, Range),
    Degree(Box<Calc<T>>, Range),
    Factorial(Box<Calc<T>>, Range),
}

impl<T: Var> Calc<T> {
    pub fn eval(&self, provider: &impl Provider<T>) -> crate::Result<Val<T>, T> {
        Ok(self.eval_num(provider)?.val)
    }

    // TODO use checked variants of arithmetic operations
    fn eval_num(&self, p: &impl Provider<T>) -> crate::Result<Num<T>, T> {
        match self {
            Self::Error(r) => Err(crate::Error::Parsing(*r)),
            Self::Num(n) => Ok(*n),
            Self::Neg(a, r) => neg(p, a.eval_num(p)?, *r),
            Self::Add(a, b) => add(p, a.eval_num(p)?, b.eval_num(p)?),
            Self::Sub(a, b) => sub(p, a.eval_num(p)?, b.eval_num(p)?),
            Self::Mul(a, b) => mul(p, a.eval_num(p)?, b.eval_num(p)?),
            Self::Div(a, b) => div(p, a.eval_num(p)?, b.eval_num(p)?),
            Self::Rem(a, b) => rem(p, a.eval_num(p)?, b.eval_num(p)?),
            Self::Pow(a, b, r) => pow(p, a.eval_num(p)?, b.eval_num(p)?, *r),
            Self::Ln(a, r) => ln(p, a.eval_num(p)?, *r),
            Self::Log(a, b, r) => log(p, a.eval_num(p)?, b.eval_num(p)?, *r),
            Self::Sqrt(a, r) => sqrt(p, a.eval_num(p)?, *r),
            Self::Ncr(a, b, r) => ncr(p, a.eval_num(p)?, b.eval_num(p)?, *r),
            Self::Sin(a, r) => sin(p, a.eval_num(p)?, *r),
            Self::Cos(a, r) => cos(p, a.eval_num(p)?, *r),
            Self::Tan(a, r) => tan(p, a.eval_num(p)?, *r),
            Self::Asin(a, r) => asin(p, a.eval_num(p)?, *r),
            Self::Acos(a, r) => acos(p, a.eval_num(p)?, *r),
            Self::Atan(a, r) => atan(p, a.eval_num(p)?, *r),
            Self::Gcd(a, b, r) => gcd(p, a.eval_num(p)?, b.eval_num(p)?, *r),
            Self::Degree(a, r) => degree(p, a.eval_num(p)?, *r), // TODO add rad modifier and require a typed angle value as input for trigeometrical functions
            Self::Factorial(a, r) => factorial(p, a.eval_num(p)?, *r),
        }
        .map(|mut n| {
            n.val = n.val.maybe_int();
            n
        })
    }
}

fn neg<T: Var>(p: &impl Provider<T>, n: Num<T>, range: Range) -> crate::Result<Num<T>, T> {
    let val = match n.val {
        Val::Int(i) => Val::Int(-i),
        v => Val::Float(-p.val_to_f64(v)),
    };
    Ok(Num { val, range })
}

fn add<T: Var>(p: &impl Provider<T>, n1: Num<T>, n2: Num<T>) -> crate::Result<Num<T>, T> {
    let val = match (n1.val, n2.val) {
        (Val::Int(a), Val::Int(b)) => match a.checked_add(b) {
            Some(v) => Val::Int(v),
            None => return Err(crate::Error::AddOverflow(n1, n2)),
        },
        (a, b) => Val::Float(p.val_to_f64(a) + p.val_to_f64(b)),
    };
    let range = Range::span(n1.range, n2.range);
    Ok(Num { val, range })
}

fn sub<T: Var>(p: &impl Provider<T>, n1: Num<T>, n2: Num<T>) -> crate::Result<Num<T>, T> {
    let val = match (n1.val, n2.val) {
        (Val::Int(a), Val::Int(b)) => match a.checked_sub(b) {
            Some(v) => Val::Int(v),
            None => return Err(crate::Error::SubOverflow(n1, n2)),
        },
        (a, b) => Val::Float(p.val_to_f64(a) - p.val_to_f64(b)),
    };
    let range = Range::span(n1.range, n2.range);
    Ok(Num { val, range })
}

fn mul<T: Var>(p: &impl Provider<T>, n1: Num<T>, n2: Num<T>) -> crate::Result<Num<T>, T> {
    let val = match (n1.val, n2.val) {
        (Val::Int(a), Val::Int(b)) => match a.checked_mul(b) {
            Some(v) => Val::Int(v),
            None => return Err(crate::Error::MulOverflow(n1, n2)),
        },
        (a, b) => Val::Float(p.val_to_f64(a) * p.val_to_f64(b)),
    };
    let range = Range::span(n1.range, n2.range);
    Ok(Num { val, range })
}

fn div<T: Var>(p: &impl Provider<T>, n1: Num<T>, n2: Num<T>) -> crate::Result<Num<T>, T> {
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
            let divisor = p.val_to_f64(b);
            if divisor == 0.0 {
                return Err(crate::Error::DivideByZero(n1, n2));
            } else {
                Val::Float(p.val_to_f64(a) / divisor)
            }
        }
    };
    let range = Range::span(n1.range, n2.range);
    Ok(Num { val, range })
}

fn rem<T: Var>(_p: &impl Provider<T>, n1: Num<T>, n2: Num<T>) -> crate::Result<Num<T>, T> {
    let val = match (n1.val, n2.val) {
        (Val::Int(a), Val::Int(b)) => {
            if b == 0 {
                return Err(crate::Error::RemainderByZero(n1, n2));
            } else {
                let r = a % b;
                if (r > 0 && b < 0) || (r < 0 && b > 0) {
                    Val::Int(r + b)
                } else {
                    Val::Int(r)
                }
            }
        }
        _ => return Err(crate::Error::FractionRemainder(n1, n2)),
    };
    let range = Range::span(n1.range, n2.range);
    Ok(Num { val, range })
}

fn pow<T: Var>(
    p: &impl Provider<T>,
    n1: Num<T>,
    n2: Num<T>,
    range: Range,
) -> crate::Result<Num<T>, T> {
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
        (a, b) => Val::Float(p.val_to_f64(a).powf(p.val_to_f64(b))),
    };
    Ok(Num { val, range })
}

fn ln<T: Var>(p: &impl Provider<T>, n: Num<T>, range: Range) -> crate::Result<Num<T>, T> {
    let val = Val::Float(p.val_to_f64(n.val).ln());
    Ok(Num { val, range })
}

fn log<T: Var>(
    p: &impl Provider<T>,
    base: Num<T>,
    n: Num<T>,
    range: Range,
) -> crate::Result<Num<T>, T> {
    let val = Val::Float(p.val_to_f64(n.val).log(p.val_to_f64(base.val)));
    Ok(Num { val, range })
}

fn sqrt<T: Var>(p: &impl Provider<T>, n: Num<T>, range: Range) -> crate::Result<Num<T>, T> {
    let val = Val::Float(p.val_to_f64(n.val).sqrt());
    Ok(Num { val, range })
}

fn ncr<T: Var>(
    p: &impl Provider<T>,
    a: Num<T>,
    b: Num<T>,
    range: Range,
) -> crate::Result<Num<T>, T> {
    let divident = factorial(p, a, range)?;
    let divisor = mul(
        p,
        factorial(p, b, range)?,
        factorial(p, sub(p, a, b)?, range)?,
    )?;
    let val = div(p, divident, divisor)?.val;
    Ok(Num { val, range })
}

fn sin<T: Var>(p: &impl Provider<T>, n: Num<T>, range: Range) -> crate::Result<Num<T>, T> {
    let val = Val::Float(p.val_to_f64(n.val).sin());
    Ok(Num { val, range })
}

fn cos<T: Var>(p: &impl Provider<T>, n: Num<T>, range: Range) -> crate::Result<Num<T>, T> {
    let val = Val::Float(p.val_to_f64(n.val).cos());
    Ok(Num { val, range })
}

fn tan<T: Var>(p: &impl Provider<T>, n: Num<T>, range: Range) -> crate::Result<Num<T>, T> {
    let val = Val::Float(p.val_to_f64(n.val).tan());
    Ok(Num { val, range })
}

fn asin<T: Var>(p: &impl Provider<T>, n: Num<T>, range: Range) -> crate::Result<Num<T>, T> {
    let val = Val::Float(p.val_to_f64(n.val).asin());
    Ok(Num { val, range })
}

fn acos<T: Var>(p: &impl Provider<T>, n: Num<T>, range: Range) -> crate::Result<Num<T>, T> {
    let val = Val::Float(p.val_to_f64(n.val).acos());
    Ok(Num { val, range })
}

fn atan<T: Var>(p: &impl Provider<T>, n: Num<T>, range: Range) -> crate::Result<Num<T>, T> {
    let val = Val::Float(p.val_to_f64(n.val).atan());
    Ok(Num { val, range })
}

fn gcd<T: Var>(_p: &impl Provider<T>, n1: Num<T>, n2: Num<T>, range: Range) -> crate::Result<Num<T>, T> {
    fn euclid(m: i128, n: i128) -> (i128, i128) {
        if m == 0 {
            return (0, 1);
        } else if n == 0 {
            return (1, 0);
        } else if m > n {
            let (x, y) = euclid(n, m);
            (y, x)
        } else if n % m == 0 {
            (1, 0)
        } else {
            let (x1, y1) = euclid(n % m, m);
            let x = y1 - x1 * (n / m);
            let y = x1;
            (x, y)
        }
    }


    match (n1.val, n2.val) {
        (Val::Int(m), Val::Int(n)) => {
            let (x, y) = euclid(m, n);
            let val = Val::Int(x * m + y * n);
            Ok(Num { val, range })
        }
        _ => Err(crate::Error::FractionGcd(n1, n2)),
    }
}

fn degree<T: Var>(p: &impl Provider<T>, n: Num<T>, range: Range) -> crate::Result<Num<T>, T> {
    let val = Val::Float(p.val_to_f64(n.val).to_radians());
    Ok(Num { val, range })
}

fn factorial<T: Var>(_: &impl Provider<T>, n: Num<T>, range: Range) -> crate::Result<Num<T>, T> {
    let val = match n.val {
        Val::Int(i) => {
            if i < 0 {
                return Err(crate::Error::NegativeFactorial(n.range));
            } else {
                Val::Int((1..=i).product())
            }
        }
        _ => return Err(crate::Error::FractionFactorial(n.range)),
    };
    Ok(Num { val, range })
}
