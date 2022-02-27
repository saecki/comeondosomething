use std::cmp::Ordering;
use std::convert::TryFrom;
use std::f64::consts;

use crate::{Ext, Num, PlainVal, Provider, Range, Val};

impl<T: Ext> Val<T> {
    /// Convert values of type [`Self::Float`] that aren't fractions to [`Self::Int`];
    pub fn maybe_int(self) -> Self {
        match self {
            Self::Plain(PlainVal::Float(f)) => {
                let i = f as i128;
                #[allow(clippy::float_cmp)]
                if i as f64 == f {
                    Self::Plain(PlainVal::Int(i))
                } else {
                    Self::Plain(PlainVal::Float(f))
                }
            }
            v => v,
        }
    }
}

impl PlainVal {
    pub fn to_int(self) -> Option<i128> {
        match self {
            Self::Int(i) => Some(i),
            Self::Float(f) => {
                let i = f as i128;
                #[allow(clippy::float_cmp)]
                if i as f64 == f {
                    Some(i)
                } else {
                    None
                }
            }
            PlainVal::TAU => None,
            PlainVal::PI => None,
            PlainVal::E => None,
        }
    }

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

#[derive(Clone, Debug, PartialEq)]
pub enum Calc<T: Ext> {
    Error(Range),
    Num(Num<T>),
    Neg(Box<Calc<T>>, Range),
    Add(Box<Calc<T>>, Box<Calc<T>>),
    Sub(Box<Calc<T>>, Box<Calc<T>>),
    Mul(Box<Calc<T>>, Box<Calc<T>>),
    Div(Box<Calc<T>>, Box<Calc<T>>),
    IntDiv(Box<Calc<T>>, Box<Calc<T>>),
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
    Min(Vec<Calc<T>>, Range),
    Max(Vec<Calc<T>>, Range),
    Clamp(Box<Calc<T>>, Box<Calc<T>>, Box<Calc<T>>, Range),
    Degree(Box<Calc<T>>, Range),
    Factorial(Box<Calc<T>>, Range),
}

impl<T: Ext> Calc<T> {
    pub fn eval(&self, provider: &impl Provider<T>) -> crate::Result<Val<T>, T> {
        Ok(self.eval_num(provider)?.val)
    }

    fn eval_num(&self, p: &impl Provider<T>) -> crate::Result<Num<T>, T> {
        match self {
            Self::Error(r) => Err(crate::Error::Parsing(*r)),
            Self::Num(n) => Ok(*n),
            Self::Neg(a, r) => neg(p, a.eval_num(p)?, *r),
            Self::Add(a, b) => add(p, a.eval_num(p)?, b.eval_num(p)?),
            Self::Sub(a, b) => sub(p, a.eval_num(p)?, b.eval_num(p)?),
            Self::Mul(a, b) => mul(p, a.eval_num(p)?, b.eval_num(p)?),
            Self::Div(a, b) => div(p, a.eval_num(p)?, b.eval_num(p)?),
            Self::IntDiv(a, b) => int_div(p, a.eval_num(p)?, b.eval_num(p)?),
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
            Self::Min(args, r) => {
                let nums = eval_nums(p, args)?;
                min(p, nums, *r)
            }
            Self::Max(args, r) => {
                let nums = eval_nums(p, args)?;
                max(p, nums, *r)
            }
            Self::Clamp(val, min, max, r) => {
                clamp(p, val.eval_num(p)?, min.eval_num(p)?, max.eval_num(p)?, *r)
            }
            Self::Degree(a, r) => degree(p, a.eval_num(p)?, *r), // TODO add rad modifier and require a typed angle value as input for trigeometrical functions
            Self::Factorial(a, r) => factorial(p, a.eval_num(p)?, *r),
        }
        .map(|mut n| {
            n.val = n.val.maybe_int();
            n
        })
    }
}

fn eval_nums<T: Ext>(p: &impl Provider<T>, args: &[Calc<T>]) -> crate::Result<Vec<Num<T>>, T> {
    let mut nums = Vec::with_capacity(args.len());
    for a in args {
        nums.push(a.eval_num(p)?);
    }
    Ok(nums)
}

fn neg<T: Ext>(p: &impl Provider<T>, n: Num<T>, range: Range) -> crate::Result<Num<T>, T> {
    let val = match n.val {
        Val::Plain(PlainVal::Int(i)) => Val::int(-i),
        v => Val::float(-p.to_f64(v)),
    };
    Ok(Num { val, range })
}

fn add<T: Ext>(p: &impl Provider<T>, n1: Num<T>, n2: Num<T>) -> crate::Result<Num<T>, T> {
    let val = match (p.to_int(n1.val), p.to_int(n2.val)) {
        (Some(a), Some(b)) => match a.checked_add(b) {
            Some(v) => Val::int(v),
            None => return Err(crate::Error::AddOverflow(n1, n2)),
        },
        _ => Val::float(p.to_f64(n1.val) + p.to_f64(n2.val)),
    };
    let range = Range::span(n1.range, n2.range);
    Ok(Num { val, range })
}

fn sub<T: Ext>(p: &impl Provider<T>, n1: Num<T>, n2: Num<T>) -> crate::Result<Num<T>, T> {
    let val = match (p.to_int(n1.val), p.to_int(n2.val)) {
        (Some(a), Some(b)) => match a.checked_sub(b) {
            Some(v) => Val::int(v),
            None => return Err(crate::Error::SubOverflow(n1, n2)),
        },
        _ => Val::float(p.to_f64(n1.val) - p.to_f64(n2.val)),
    };
    let range = Range::span(n1.range, n2.range);
    Ok(Num { val, range })
}

fn mul<T: Ext>(p: &impl Provider<T>, n1: Num<T>, n2: Num<T>) -> crate::Result<Num<T>, T> {
    let val = match (p.to_int(n1.val), p.to_int(n2.val)) {
        (Some(a), Some(b)) => match a.checked_mul(b) {
            Some(v) => Val::int(v),
            None => return Err(crate::Error::MulOverflow(n1, n2)),
        },
        _ => Val::float(p.to_f64(n1.val) * p.to_f64(n2.val)),
    };
    let range = Range::span(n1.range, n2.range);
    Ok(Num { val, range })
}

fn div<T: Ext>(p: &impl Provider<T>, n1: Num<T>, n2: Num<T>) -> crate::Result<Num<T>, T> {
    let val = match (p.to_int(n1.val), p.to_int(n2.val)) {
        (Some(a), Some(b)) => {
            if b == 0 {
                return Err(crate::Error::DivideByZero(n1, n2));
            } else if a % b == 0 {
                Val::int(a / b)
            } else {
                Val::float(a as f64 / b as f64)
            }
        }
        _ => {
            let divisor = p.to_f64(n2.val);
            if divisor == 0.0 {
                return Err(crate::Error::DivideByZero(n1, n2));
            } else {
                Val::float(p.to_f64(n1.val) / divisor)
            }
        }
    };
    let range = Range::span(n1.range, n2.range);
    Ok(Num { val, range })
}

fn int_div<T: Ext>(p: &impl Provider<T>, n1: Num<T>, n2: Num<T>) -> crate::Result<Num<T>, T> {
    let val = match (p.to_int(n1.val), p.to_int(n2.val)) {
        (Some(a), Some(b)) => {
            if b == 0 {
                return Err(crate::Error::DivideByZero(n1, n2));
            } else {
                Val::int(a / b)
            }
        }
        _ => return Err(crate::Error::FractionEuclidDiv(n1, n2)),
    };
    let range = Range::span(n1.range, n2.range);
    Ok(Num { val, range })
}

fn rem<T: Ext>(p: &impl Provider<T>, n1: Num<T>, n2: Num<T>) -> crate::Result<Num<T>, T> {
    let val = match (p.to_int(n1.val), p.to_int(n2.val)) {
        (Some(a), Some(b)) => {
            if b == 0 {
                return Err(crate::Error::RemainderByZero(n1, n2));
            } else {
                let r = a % b;
                if (r > 0 && b < 0) || (r < 0 && b > 0) {
                    Val::int(r + b)
                } else {
                    Val::int(r)
                }
            }
        }
        _ => return Err(crate::Error::FractionRemainder(n1, n2)),
    };
    let range = Range::span(n1.range, n2.range);
    Ok(Num { val, range })
}

fn pow<T: Ext>(
    p: &impl Provider<T>,
    n1: Num<T>,
    n2: Num<T>,
    range: Range,
) -> crate::Result<Num<T>, T> {
    let val = match (p.to_int(n1.val), p.to_int(n2.val)) {
        (Some(a), Some(b)) => {
            if let Ok(exp) = u32::try_from(b) {
                Val::int(a.pow(exp))
            } else if let Ok(exp) = i32::try_from(b) {
                Val::float((a as f64).powi(exp))
            } else {
                Val::float((a as f64).powf(b as f64))
            }
        }
        (None, Some(b)) => {
            if let Ok(exp) = i32::try_from(b) {
                Val::float((p.to_f64(n1.val)).powi(exp))
            } else {
                Val::float((p.to_f64(n1.val)).powf(b as f64))
            }
        }
        _ => Val::float(p.to_f64(n1.val).powf(p.to_f64(n2.val))),
    };
    Ok(Num { val, range })
}

fn ln<T: Ext>(p: &impl Provider<T>, n: Num<T>, range: Range) -> crate::Result<Num<T>, T> {
    let val = Val::float(p.to_f64(n.val).ln());
    Ok(Num { val, range })
}

fn log<T: Ext>(
    p: &impl Provider<T>,
    base: Num<T>,
    n: Num<T>,
    range: Range,
) -> crate::Result<Num<T>, T> {
    let val = Val::float(p.to_f64(n.val).log(p.to_f64(base.val)));
    Ok(Num { val, range })
}

fn sqrt<T: Ext>(p: &impl Provider<T>, n: Num<T>, range: Range) -> crate::Result<Num<T>, T> {
    let val = Val::float(p.to_f64(n.val).sqrt());
    Ok(Num { val, range })
}

fn ncr<T: Ext>(
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

fn sin<T: Ext>(p: &impl Provider<T>, n: Num<T>, range: Range) -> crate::Result<Num<T>, T> {
    let val = Val::float(p.to_f64(n.val).sin());
    Ok(Num { val, range })
}

fn cos<T: Ext>(p: &impl Provider<T>, n: Num<T>, range: Range) -> crate::Result<Num<T>, T> {
    let val = Val::float(p.to_f64(n.val).cos());
    Ok(Num { val, range })
}

fn tan<T: Ext>(p: &impl Provider<T>, n: Num<T>, range: Range) -> crate::Result<Num<T>, T> {
    let val = Val::float(p.to_f64(n.val).tan());
    Ok(Num { val, range })
}

fn asin<T: Ext>(p: &impl Provider<T>, n: Num<T>, range: Range) -> crate::Result<Num<T>, T> {
    let val = Val::float(p.to_f64(n.val).asin());
    Ok(Num { val, range })
}

fn acos<T: Ext>(p: &impl Provider<T>, n: Num<T>, range: Range) -> crate::Result<Num<T>, T> {
    let val = Val::float(p.to_f64(n.val).acos());
    Ok(Num { val, range })
}

fn atan<T: Ext>(p: &impl Provider<T>, n: Num<T>, range: Range) -> crate::Result<Num<T>, T> {
    let val = Val::float(p.to_f64(n.val).atan());
    Ok(Num { val, range })
}

fn gcd<T: Ext>(
    p: &impl Provider<T>,
    n1: Num<T>,
    n2: Num<T>,
    range: Range,
) -> crate::Result<Num<T>, T> {
    fn euclid(m: i128, n: i128) -> (i128, i128) {
        if m == 0 {
            (0, 1)
        } else if n == 0 {
            (1, 0)
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

    match (p.to_int(n1.val), p.to_int(n2.val)) {
        (Some(m), Some(n)) => {
            let (x, y) = euclid(m, n);
            let val = Val::int(x * m + y * n);
            Ok(Num { val, range })
        }
        _ => Err(crate::Error::FractionGcd(n1, n2)),
    }
}

fn min<T: Ext>(p: &impl Provider<T>, args: Vec<Num<T>>, range: Range) -> crate::Result<Num<T>, T> {
    let min = args
        .iter()
        .min_by(|a, b| {
            if p.to_f64(a.val) > p.to_f64(b.val) {
                Ordering::Greater
            } else {
                Ordering::Less
            }
        })
        .expect("Iterator should at least contain 1 element");

    Ok(Num::new(min.val, range))
}

fn max<T: Ext>(p: &impl Provider<T>, args: Vec<Num<T>>, range: Range) -> crate::Result<Num<T>, T> {
    let max = args
        .iter()
        .max_by(|a, b| {
            if p.to_f64(a.val) < p.to_f64(b.val) {
                Ordering::Less
            } else {
                Ordering::Greater
            }
        })
        .expect("Iterator should at least contain 1 element");

    Ok(Num::new(max.val, range))
}

fn clamp<T: Ext>(
    p: &impl Provider<T>,
    num: Num<T>,
    min: Num<T>,
    max: Num<T>,
    range: Range,
) -> crate::Result<Num<T>, T> {
    let val = match (p.to_int(num.val), p.to_int(min.val), p.to_int(max.val)) {
        (Some(v), Some(lo), Some(hi)) => {
            if lo > hi {
                return Err(crate::Error::InvalidClampBounds(min, max));
            }
            Val::int(v.clamp(lo, hi))
        }
        _ => {
            let v = p.to_f64(num.val);
            let lo = p.to_f64(min.val);
            let hi = p.to_f64(max.val);
            if !(lo <= hi) { // floating point weirdness
                return Err(crate::Error::InvalidClampBounds(min, max));
            }
            Val::float(v.clamp(lo, hi))
        }
    };
    Ok(Num::new(val, range))
}

fn degree<T: Ext>(p: &impl Provider<T>, n: Num<T>, range: Range) -> crate::Result<Num<T>, T> {
    let val = Val::float(p.to_f64(n.val).to_radians());
    Ok(Num { val, range })
}

fn factorial<T: Ext>(p: &impl Provider<T>, n: Num<T>, range: Range) -> crate::Result<Num<T>, T> {
    let val = match p.to_int(n.val) {
        Some(i) => {
            if i < 0 {
                return Err(crate::Error::NegativeFactorial(n.range));
            } else {
                Val::int((1..=i).product())
            }
        }
        _ => return Err(crate::Error::FractionFactorial(n.range)),
    };
    Ok(Num { val, range })
}
