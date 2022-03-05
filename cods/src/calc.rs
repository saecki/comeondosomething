use std::convert::TryFrom;
use std::f64::consts;

use crate::{Context, Ext, Num, PlainVal, Provider, Range, Val};

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

pub enum ValResult {
    Resolved(PlainVal),
    Unresolved(String),
}

impl<T: Ext, P: Provider<T>> Context<T, P> {
    pub fn eval(&self, calc: &Calc<T>) -> crate::Result<PlainVal, T> {
        let num = self.eval_num(calc)?;
        self.plain_val(num)
    }

    pub fn plain_val(&self, num: Num<T>) -> crate::Result<PlainVal, T> {
        match self.resolve_val(num.val) {
            ValResult::Resolved(p) => Ok(p),
            ValResult::Unresolved(name) => Err(crate::Error::UndefinedVar(name, num.range)),
        }
    }

    pub fn to_f64(&self, num: Num<T>) -> crate::Result<f64, T> {
        Ok(self.plain_val(num)?.to_f64())
    }

    pub fn to_int(&self, num: Num<T>) -> crate::Result<Option<i128>, T> {
        Ok(self.plain_val(num)?.to_int())
    }

    pub fn resolve_val(&self, val: Val<T>) -> ValResult {
        match val {
            Val::Plain(p) => ValResult::Resolved(p),
            Val::Ext(e) => ValResult::Resolved(self.provider.plain_val(e)),
            Val::Var(id) => {
                let var = &self.vars[id];
                match var.value {
                    Some(v) => self.resolve_val(v), // TODO: check for infinite recursion: x = y; y = x
                    None => ValResult::Unresolved(var.name.clone()),
                }
            }
        }
    }

    fn eval_num(&self, calc: &Calc<T>) -> crate::Result<Num<T>, T> {
        match calc {
            Calc::Error(r) => Err(crate::Error::Parsing(*r)),
            Calc::Num(n) => Ok(*n),
            Calc::Neg(a, r) => self.neg(self.eval_num(a)?, *r),
            Calc::Add(a, b) => self.add(self.eval_num(a)?, self.eval_num(b)?),
            Calc::Sub(a, b) => self.sub(self.eval_num(a)?, self.eval_num(b)?),
            Calc::Mul(a, b) => self.mul(self.eval_num(a)?, self.eval_num(b)?),
            Calc::Div(a, b) => self.div(self.eval_num(a)?, self.eval_num(b)?),
            Calc::IntDiv(a, b) => self.int_div(self.eval_num(a)?, self.eval_num(b)?),
            Calc::Rem(a, b) => self.rem(self.eval_num(a)?, self.eval_num(b)?),
            Calc::Pow(a, b, r) => self.pow(self.eval_num(a)?, self.eval_num(b)?, *r),
            Calc::Ln(a, r) => self.ln(self.eval_num(a)?, *r),
            Calc::Log(a, b, r) => self.log(self.eval_num(a)?, self.eval_num(b)?, *r),
            Calc::Sqrt(a, r) => self.sqrt(self.eval_num(a)?, *r),
            Calc::Ncr(a, b, r) => self.ncr(self.eval_num(a)?, self.eval_num(b)?, *r),
            Calc::Sin(a, r) => self.sin(self.eval_num(a)?, *r),
            Calc::Cos(a, r) => self.cos(self.eval_num(a)?, *r),
            Calc::Tan(a, r) => self.tan(self.eval_num(a)?, *r),
            Calc::Asin(a, r) => self.asin(self.eval_num(a)?, *r),
            Calc::Acos(a, r) => self.acos(self.eval_num(a)?, *r),
            Calc::Atan(a, r) => self.atan(self.eval_num(a)?, *r),
            Calc::Gcd(a, b, r) => self.gcd(self.eval_num(a)?, self.eval_num(b)?, *r),
            Calc::Min(args, r) => {
                let nums = self.eval_nums(args)?;
                self.min(nums, *r)
            }
            Calc::Max(args, r) => {
                let nums = self.eval_nums(args)?;
                self.max(nums, *r)
            }
            Calc::Clamp(num, min, max, r) => self.clamp(
                self.eval_num(num)?,
                self.eval_num(min)?,
                self.eval_num(max)?,
                *r,
            ),
            Calc::Degree(a, r) => self.degree(self.eval_num(a)?, *r), // TODO add rad modifier and require a typed angle value as input for trigeometrical functions
            Calc::Factorial(a, r) => self.factorial(self.eval_num(a)?, *r),
        }
        .map(|mut n| {
            n.val = n.val.maybe_int();
            n
        })
    }

    fn eval_nums(&self, args: &[Calc<T>]) -> crate::Result<Vec<Num<T>>, T> {
        let mut nums = Vec::with_capacity(args.len());
        for a in args {
            nums.push(self.eval_num(a)?);
        }
        Ok(nums)
    }

    fn neg(&self, n: Num<T>, range: Range) -> crate::Result<Num<T>, T> {
        let val = match n.val {
            Val::Plain(PlainVal::Int(i)) => Val::int(-i),
            _ => Val::float(-self.to_f64(n)?),
        };
        Ok(Num { val, range })
    }

    fn add(&self, n1: Num<T>, n2: Num<T>) -> crate::Result<Num<T>, T> {
        let val = match (self.to_int(n1)?, self.to_int(n2)?) {
            (Some(a), Some(b)) => match a.checked_add(b) {
                Some(v) => Val::int(v),
                None => return Err(crate::Error::AddOverflow(n1, n2)),
            },
            _ => Val::float(self.to_f64(n1)? + self.to_f64(n2)?),
        };
        let range = Range::span(n1.range, n2.range);
        Ok(Num { val, range })
    }

    fn sub(&self, n1: Num<T>, n2: Num<T>) -> crate::Result<Num<T>, T> {
        let val = match (self.to_int(n1)?, self.to_int(n2)?) {
            (Some(a), Some(b)) => match a.checked_sub(b) {
                Some(v) => Val::int(v),
                None => return Err(crate::Error::SubOverflow(n1, n2)),
            },
            _ => Val::float(self.to_f64(n1)? - self.to_f64(n2)?),
        };
        let range = Range::span(n1.range, n2.range);
        Ok(Num { val, range })
    }

    fn mul(&self, n1: Num<T>, n2: Num<T>) -> crate::Result<Num<T>, T> {
        let val = match (self.to_int(n1)?, self.to_int(n2)?) {
            (Some(a), Some(b)) => match a.checked_mul(b) {
                Some(v) => Val::int(v),
                None => return Err(crate::Error::MulOverflow(n1, n2)),
            },
            _ => Val::float(self.to_f64(n1)? * self.to_f64(n2)?),
        };
        let range = Range::span(n1.range, n2.range);
        Ok(Num { val, range })
    }

    fn div(&self, n1: Num<T>, n2: Num<T>) -> crate::Result<Num<T>, T> {
        let val = match (self.to_int(n1)?, self.to_int(n2)?) {
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
                let divisor = self.to_f64(n2)?;
                if divisor == 0.0 {
                    return Err(crate::Error::DivideByZero(n1, n2));
                } else {
                    Val::float(self.to_f64(n1)? / divisor)
                }
            }
        };
        let range = Range::span(n1.range, n2.range);
        Ok(Num { val, range })
    }

    fn int_div(&self, n1: Num<T>, n2: Num<T>) -> crate::Result<Num<T>, T> {
        let val = match (self.to_int(n1)?, self.to_int(n2)?) {
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

    fn rem(&self, n1: Num<T>, n2: Num<T>) -> crate::Result<Num<T>, T> {
        let val = match (self.to_int(n1)?, self.to_int(n2)?) {
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

    fn pow(&self, n1: Num<T>, n2: Num<T>, range: Range) -> crate::Result<Num<T>, T> {
        let val = match (self.to_int(n1)?, self.to_int(n2)?) {
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
                    Val::float((self.to_f64(n1)?).powi(exp))
                } else {
                    Val::float((self.to_f64(n1)?).powf(b as f64))
                }
            }
            _ => Val::float(self.to_f64(n1)?.powf(self.to_f64(n2)?)),
        };
        Ok(Num { val, range })
    }

    fn ln(&self, n: Num<T>, range: Range) -> crate::Result<Num<T>, T> {
        let val = Val::float(self.to_f64(n)?.ln());
        Ok(Num { val, range })
    }

    fn log(&self, base: Num<T>, n: Num<T>, range: Range) -> crate::Result<Num<T>, T> {
        let val = Val::float(self.to_f64(n)?.log(self.to_f64(base)?));
        Ok(Num { val, range })
    }

    fn sqrt(&self, n: Num<T>, range: Range) -> crate::Result<Num<T>, T> {
        let val = Val::float(self.to_f64(n)?.sqrt());
        Ok(Num { val, range })
    }

    fn ncr(&self, n1: Num<T>, n2: Num<T>, range: Range) -> crate::Result<Num<T>, T> {
        let val = match (self.to_int(n1)?, self.to_int(n2)?) {
            (Some(n), Some(mut r)) => {
                if r < 0 {
                    return Err(crate::Error::NegativeNcr(n1, n2));
                }
                if n < r {
                    return Err(crate::Error::InvalidNcr(n1, n2));
                }

                // symmetrical: nCr(9, 2) == nCr(9, 7)
                if r > n - r {
                    r = n - r;
                }

                let mut val = 1;
                for i in 1..=r {
                    val *= n - r + i;
                    val /= i;
                }

                Val::int(val)
            }
            _ => return Err(crate::Error::FractionNcr(n1, n2)),
        };
        Ok(Num { val, range })
    }

    fn sin(&self, n: Num<T>, range: Range) -> crate::Result<Num<T>, T> {
        let val = Val::float(self.to_f64(n)?.sin());
        Ok(Num { val, range })
    }

    fn cos(&self, n: Num<T>, range: Range) -> crate::Result<Num<T>, T> {
        let val = Val::float(self.to_f64(n)?.cos());
        Ok(Num { val, range })
    }

    fn tan(&self, n: Num<T>, range: Range) -> crate::Result<Num<T>, T> {
        let val = Val::float(self.to_f64(n)?.tan());
        Ok(Num { val, range })
    }

    fn asin(&self, n: Num<T>, range: Range) -> crate::Result<Num<T>, T> {
        let val = Val::float(self.to_f64(n)?.asin());
        Ok(Num { val, range })
    }

    fn acos(&self, n: Num<T>, range: Range) -> crate::Result<Num<T>, T> {
        let val = Val::float(self.to_f64(n)?.acos());
        Ok(Num { val, range })
    }

    fn atan(&self, n: Num<T>, range: Range) -> crate::Result<Num<T>, T> {
        let val = Val::float(self.to_f64(n)?.atan());
        Ok(Num { val, range })
    }

    fn gcd(&self, n1: Num<T>, n2: Num<T>, range: Range) -> crate::Result<Num<T>, T> {
        match (self.to_int(n1)?, self.to_int(n2)?) {
            (Some(mut a), Some(mut b)) => {
                let mut _t = 0;
                while b != 0 {
                    _t = b;
                    b = a % b;
                    a = _t;
                }
                let val = Val::int(a);
                Ok(Num::new(val, range))
            }
            _ => Err(crate::Error::FractionGcd(n1, n2)),
        }
    }

    fn min(&self, nums: Vec<Num<T>>, range: Range) -> crate::Result<Num<T>, T> {
        let mut min = None;
        for n in nums {
            let val = self.to_f64(n)?;
            match min {
                None => min = Some((n, val)),
                Some((_, m_val)) => {
                    if val < m_val {
                        min = Some((n, val));
                    }
                }
            }
        }

        let (min, _) = min.expect("Iterator should at least contain 1 element");
        Ok(Num::new(min.val, range))
    }

    fn max(&self, nums: Vec<Num<T>>, range: Range) -> crate::Result<Num<T>, T> {
        let mut max = None;
        for n in nums {
            let val = self.to_f64(n)?;
            match max {
                None => max = Some((n, val)),
                Some((_, m_val)) => {
                    if val > m_val {
                        max = Some((n, val));
                    }
                }
            }
        }

        let (max, _) = max.expect("Iterator should at least contain 1 element");
        Ok(Num::new(max.val, range))
    }

    fn clamp(
        &self,
        num: Num<T>,
        min: Num<T>,
        max: Num<T>,
        range: Range,
    ) -> crate::Result<Num<T>, T> {
        let val = match (self.to_int(num)?, self.to_int(min)?, self.to_int(max)?) {
            (Some(v), Some(lo), Some(hi)) => {
                if lo > hi {
                    return Err(crate::Error::InvalidClampBounds(min, max));
                }
                Val::int(v.clamp(lo, hi))
            }
            _ => {
                let v = self.to_f64(num)?;
                let lo = self.to_f64(min)?;
                let hi = self.to_f64(max)?;
                // floating point weirdness, negated assertion of stdlib
                #[allow(clippy::neg_cmp_op_on_partial_ord)]
                if !(lo <= hi) {
                    return Err(crate::Error::InvalidClampBounds(min, max));
                }
                Val::float(v.clamp(lo, hi))
            }
        };
        Ok(Num::new(val, range))
    }

    fn degree(&self, n: Num<T>, range: Range) -> crate::Result<Num<T>, T> {
        let val = Val::float(self.to_f64(n)?.to_radians());
        Ok(Num { val, range })
    }

    fn factorial(&self, n: Num<T>, range: Range) -> crate::Result<Num<T>, T> {
        let val = match self.to_int(n)? {
            Some(a) => {
                if a < 0 {
                    return Err(crate::Error::NegativeFactorial(n));
                } else {
                    let mut val: i128 = 1;
                    for i in 1..=a {
                        match val.checked_mul(i) {
                            Some(v) => val = v,
                            None => return Err(crate::Error::FactorialOverflow(n)),
                        }
                    }
                    Val::int(val)
                }
            }
            _ => return Err(crate::Error::FractionFactorial(n)),
        };
        Ok(Num { val, range })
    }
}

#[cfg(test)]
mod test {
    use crate::{Context, DummyProvider, Val, Var, Calc, Range, Num, PlainVal};

    #[test]
    fn resolve_var() {
        let ctx = Context {
            provider: DummyProvider,
            vars: vec![Var {
                name: "x".into(),
                value: Some(Val::int(4)),
            }],
            ..Default::default()
        };
        let calc = Calc::Num(Num::new(Val::Var(0), Range::pos(0)));
        
        let val = ctx.eval(&calc).unwrap();
        assert_eq!(PlainVal::Int(4), val);
    }
}
