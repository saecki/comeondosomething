use std::convert::TryFrom;
use std::f64::consts;

use crate::{Context, Ext, Num, PlainVal, Provider, Range, Val, VarId};

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
pub struct Calc<T: Ext> {
    pub typ: CalcType<T>,
    pub range: Range,
}

impl<T: Ext> Calc<T> {
    pub fn new(typ: CalcType<T>, range: Range) -> Self {
        Self { typ, range }
    }

    pub fn num(num: Num<T>) -> Self {
        Self::new(CalcType::Num(num), num.range)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum CalcType<T: Ext> {
    Empty,
    Error,
    Num(Num<T>),
    Neg(Box<Calc<T>>),
    Add(Box<Calc<T>>, Box<Calc<T>>),
    Sub(Box<Calc<T>>, Box<Calc<T>>),
    Mul(Box<Calc<T>>, Box<Calc<T>>),
    Div(Box<Calc<T>>, Box<Calc<T>>),
    IntDiv(Box<Calc<T>>, Box<Calc<T>>),
    Rem(Box<Calc<T>>, Box<Calc<T>>),
    Pow(Box<Calc<T>>, Box<Calc<T>>),
    Ln(Box<Calc<T>>),
    Log(Box<Calc<T>>, Box<Calc<T>>),
    Sqrt(Box<Calc<T>>),
    Ncr(Box<Calc<T>>, Box<Calc<T>>),
    Sin(Box<Calc<T>>),
    Cos(Box<Calc<T>>),
    Tan(Box<Calc<T>>),
    Asin(Box<Calc<T>>),
    Acos(Box<Calc<T>>),
    Atan(Box<Calc<T>>),
    Gcd(Box<Calc<T>>, Box<Calc<T>>),
    Min(Vec<Calc<T>>),
    Max(Vec<Calc<T>>),
    Clamp(Box<Calc<T>>, Box<Calc<T>>, Box<Calc<T>>),
    Degree(Box<Calc<T>>),
    Factorial(Box<Calc<T>>),
    Assignment(VarId, Box<Calc<T>>),
}

pub enum ValResult {
    Resolved(PlainVal),
    Undefined(String),
    CircularRef(Vec<String>),
}

pub enum Return<T: Ext> {
    Num(Num<T>),
    Unit(Range),
}

impl<T: Ext, P: Provider<T>> Context<T, P> {
    /// Evaluate all calculations and return the last value.
    pub fn eval_all(&mut self, calcs: &[Calc<T>]) -> crate::Result<Option<PlainVal>, T> {
        match calcs.split_last() {
            Some((last, others)) => {
                for c in others {
                    self.eval(c)?;
                }
                self.eval(last)
            }
            None => Err(crate::Error::MissingCalculation),
        }
    }

    pub fn eval(&mut self, calc: &Calc<T>) -> crate::Result<Option<PlainVal>, T> {
        match self.eval_calc(calc)? {
            Return::Num(n) => Ok(Some(self.plain_val(n)?)),
            Return::Unit(_) => Ok(None),
        }
    }

    pub fn eval_nums(&mut self, args: &[Calc<T>]) -> crate::Result<Vec<Num<T>>, T> {
        let mut nums = Vec::with_capacity(args.len());
        for a in args {
            nums.push(self.eval_num(a)?);
        }
        Ok(nums)
    }

    pub fn eval_num(&mut self, calc: &Calc<T>) -> crate::Result<Num<T>, T> {
        match self.eval_calc(calc)? {
            Return::Num(n) => Ok(n),
            Return::Unit(r) => Err(crate::Error::ExpectedValue(r)),
        }
    }

    pub fn eval_calc(&mut self, calc: &Calc<T>) -> crate::Result<Return<T>, T> {
        let r = calc.range;
        match &calc.typ {
            CalcType::Empty => Ok(Return::Unit(r)),
            CalcType::Error => Err(crate::Error::Parsing(r)),
            CalcType::Num(n) => ok(*n),
            CalcType::Neg(a) => {
                let n = self.eval_num(a)?;
                self.neg(n, r)
            }
            CalcType::Add(a, b) => {
                let n1 = self.eval_num(a)?;
                let n2 = self.eval_num(b)?;
                self.add(n1, n2, r)
            }
            CalcType::Sub(a, b) => {
                let n1 = self.eval_num(a)?;
                let n2 = self.eval_num(b)?;
                self.sub(n1, n2, r)
            }
            CalcType::Mul(a, b) => {
                let n1 = self.eval_num(a)?;
                let n2 = self.eval_num(b)?;
                self.mul(n1, n2, r)
            }
            CalcType::Div(a, b) => {
                let n1 = self.eval_num(a)?;
                let n2 = self.eval_num(b)?;
                self.div(n1, n2, r)
            }
            CalcType::IntDiv(a, b) => {
                let n1 = self.eval_num(a)?;
                let n2 = self.eval_num(b)?;
                self.int_div(n1, n2, r)
            }
            CalcType::Rem(a, b) => {
                let n1 = self.eval_num(a)?;
                let n2 = self.eval_num(b)?;
                self.rem(n1, n2, r)
            }
            CalcType::Pow(a, b) => {
                let n1 = self.eval_num(a)?;
                let n2 = self.eval_num(b)?;
                self.pow(n1, n2, r)
            }
            CalcType::Ln(a) => {
                let n1 = self.eval_num(a)?;
                self.ln(n1, r)
            }
            CalcType::Log(a, b) => {
                let n1 = self.eval_num(a)?;
                let n2 = self.eval_num(b)?;
                self.log(n1, n2, r)
            }
            CalcType::Sqrt(a) => {
                let n1 = self.eval_num(a)?;
                self.sqrt(n1, r)
            }
            CalcType::Ncr(a, b) => {
                let n1 = self.eval_num(a)?;
                let n2 = self.eval_num(b)?;
                self.ncr(n1, n2, r)
            }
            CalcType::Sin(a) => {
                let n2 = self.eval_num(a)?;
                self.sin(n2, r)
            }
            CalcType::Cos(a) => {
                let n = self.eval_num(a)?;
                self.cos(n, r)
            }
            CalcType::Tan(a) => {
                let n = self.eval_num(a)?;
                self.tan(n, r)
            }
            CalcType::Asin(a) => {
                let n = self.eval_num(a)?;
                self.asin(n, r)
            }
            CalcType::Acos(a) => {
                let n = self.eval_num(a)?;
                self.acos(n, r)
            }
            CalcType::Atan(a) => {
                let n = self.eval_num(a)?;
                self.atan(n, r)
            }
            CalcType::Gcd(a, b) => {
                let n1 = self.eval_num(a)?;
                let n2 = self.eval_num(b)?;
                self.gcd(n1, n2, r)
            }
            CalcType::Min(args) => {
                let nums = self.eval_nums(args)?;
                self.min(nums, r)
            }
            CalcType::Max(args) => {
                let nums = self.eval_nums(args)?;
                self.max(nums, r)
            }
            CalcType::Clamp(num, min, max) => {
                let n1 = self.eval_num(num)?;
                let n2 = self.eval_num(min)?;
                let n3 = self.eval_num(max)?;
                self.clamp(n1, n2, n3, r)
            }
            CalcType::Degree(a) => {
                let n = self.eval_num(a)?;
                self.degree(n, r) // TODO add rad modifier and require a typed angle value as input for trigeometrical functions
            }
            CalcType::Factorial(a) => {
                let n = self.eval_num(a)?;
                self.factorial(n, r)
            }
            CalcType::Assignment(a, b) => {
                let n = self.eval_num(b)?;
                self.assign(*a, n, r)
            }
        }
        .map(|mut r| {
            if let Return::Num(n) = &mut r {
                n.val = n.val.maybe_int();
            }
            r
        })
    }

    pub fn plain_val(&self, num: Num<T>) -> crate::Result<PlainVal, T> {
        match self.resolve_val(num.val) {
            ValResult::Resolved(p) => Ok(p),
            ValResult::Undefined(name) => Err(crate::Error::UndefinedVar(name, num.range)),
            ValResult::CircularRef(names) => Err(crate::Error::CircularRef(names, num.range)),
        }
    }

    pub fn to_f64(&self, num: Num<T>) -> crate::Result<f64, T> {
        Ok(self.plain_val(num)?.to_f64())
    }

    pub fn to_int(&self, num: Num<T>) -> crate::Result<Option<i128>, T> {
        Ok(self.plain_val(num)?.to_int())
    }

    pub fn resolve_val(&self, val: Val<T>) -> ValResult {
        let mut ids = Vec::new();
        self.resolve_var(&mut ids, val)
    }

    fn resolve_var(&self, checked_ids: &mut Vec<VarId>, val: Val<T>) -> ValResult {
        match val {
            Val::Plain(p) => ValResult::Resolved(p),
            Val::Ext(e) => ValResult::Resolved(self.provider.plain_val(e)),
            Val::Var(id) if checked_ids.contains(&id) => {
                checked_ids.push(id);
                let names = checked_ids
                    .iter()
                    .map(|id| self.vars[*id].name.clone())
                    .collect();
                return ValResult::CircularRef(names);
            }
            Val::Var(id) => {
                checked_ids.push(id);
                let var = &self.vars[id];
                match var.value {
                    Some(v) => self.resolve_var(checked_ids, v),
                    None => ValResult::Undefined(var.name.clone()),
                }
            }
        }
    }

    fn neg(&self, n: Num<T>, range: Range) -> crate::Result<Return<T>, T> {
        let val = match n.val {
            Val::Plain(PlainVal::Int(i)) => Val::int(-i),
            _ => Val::float(-self.to_f64(n)?),
        };
        ok(Num { val, range })
    }

    fn add(&self, n1: Num<T>, n2: Num<T>, range: Range) -> crate::Result<Return<T>, T> {
        let val = match (self.to_int(n1)?, self.to_int(n2)?) {
            (Some(a), Some(b)) => match a.checked_add(b) {
                Some(v) => Val::int(v),
                None => return Err(crate::Error::AddOverflow(n1, n2)),
            },
            _ => Val::float(self.to_f64(n1)? + self.to_f64(n2)?),
        };
        ok(Num { val, range })
    }

    fn sub(&self, n1: Num<T>, n2: Num<T>, range: Range) -> crate::Result<Return<T>, T> {
        let val = match (self.to_int(n1)?, self.to_int(n2)?) {
            (Some(a), Some(b)) => match a.checked_sub(b) {
                Some(v) => Val::int(v),
                None => return Err(crate::Error::SubOverflow(n1, n2)),
            },
            _ => Val::float(self.to_f64(n1)? - self.to_f64(n2)?),
        };
        ok(Num { val, range })
    }

    fn mul(&self, n1: Num<T>, n2: Num<T>, range: Range) -> crate::Result<Return<T>, T> {
        let val = match (self.to_int(n1)?, self.to_int(n2)?) {
            (Some(a), Some(b)) => match a.checked_mul(b) {
                Some(v) => Val::int(v),
                None => return Err(crate::Error::MulOverflow(n1, n2)),
            },
            _ => Val::float(self.to_f64(n1)? * self.to_f64(n2)?),
        };
        ok(Num { val, range })
    }

    fn div(&self, n1: Num<T>, n2: Num<T>, range: Range) -> crate::Result<Return<T>, T> {
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
        ok(Num { val, range })
    }

    fn int_div(&self, n1: Num<T>, n2: Num<T>, range: Range) -> crate::Result<Return<T>, T> {
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
        ok(Num { val, range })
    }

    fn rem(&self, n1: Num<T>, n2: Num<T>, range: Range) -> crate::Result<Return<T>, T> {
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
        ok(Num { val, range })
    }

    fn pow(&self, n1: Num<T>, n2: Num<T>, range: Range) -> crate::Result<Return<T>, T> {
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
        ok(Num { val, range })
    }

    fn ln(&self, n: Num<T>, range: Range) -> crate::Result<Return<T>, T> {
        let val = Val::float(self.to_f64(n)?.ln());
        ok(Num { val, range })
    }

    fn log(&self, base: Num<T>, n: Num<T>, range: Range) -> crate::Result<Return<T>, T> {
        let val = Val::float(self.to_f64(n)?.log(self.to_f64(base)?));
        ok(Num { val, range })
    }

    fn sqrt(&self, n: Num<T>, range: Range) -> crate::Result<Return<T>, T> {
        let val = Val::float(self.to_f64(n)?.sqrt());
        ok(Num { val, range })
    }

    fn ncr(&self, n1: Num<T>, n2: Num<T>, range: Range) -> crate::Result<Return<T>, T> {
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
        ok(Num { val, range })
    }

    fn sin(&self, n: Num<T>, range: Range) -> crate::Result<Return<T>, T> {
        let val = Val::float(self.to_f64(n)?.sin());
        ok(Num { val, range })
    }

    fn cos(&self, n: Num<T>, range: Range) -> crate::Result<Return<T>, T> {
        let val = Val::float(self.to_f64(n)?.cos());
        ok(Num { val, range })
    }

    fn tan(&self, n: Num<T>, range: Range) -> crate::Result<Return<T>, T> {
        let val = Val::float(self.to_f64(n)?.tan());
        ok(Num { val, range })
    }

    fn asin(&self, n: Num<T>, range: Range) -> crate::Result<Return<T>, T> {
        let val = Val::float(self.to_f64(n)?.asin());
        ok(Num { val, range })
    }

    fn acos(&self, n: Num<T>, range: Range) -> crate::Result<Return<T>, T> {
        let val = Val::float(self.to_f64(n)?.acos());
        ok(Num { val, range })
    }

    fn atan(&self, n: Num<T>, range: Range) -> crate::Result<Return<T>, T> {
        let val = Val::float(self.to_f64(n)?.atan());
        ok(Num { val, range })
    }

    fn gcd(&self, n1: Num<T>, n2: Num<T>, range: Range) -> crate::Result<Return<T>, T> {
        match (self.to_int(n1)?, self.to_int(n2)?) {
            (Some(mut a), Some(mut b)) => {
                let mut _t = 0;
                while b != 0 {
                    _t = b;
                    b = a % b;
                    a = _t;
                }
                let val = Val::int(a);
                ok(Num::new(val, range))
            }
            _ => Err(crate::Error::FractionGcd(n1, n2)),
        }
    }

    fn min(&self, nums: Vec<Num<T>>, range: Range) -> crate::Result<Return<T>, T> {
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
        ok(Num::new(min.val, range))
    }

    fn max(&self, nums: Vec<Num<T>>, range: Range) -> crate::Result<Return<T>, T> {
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
        ok(Num::new(max.val, range))
    }

    fn clamp(
        &self,
        num: Num<T>,
        min: Num<T>,
        max: Num<T>,
        range: Range,
    ) -> crate::Result<Return<T>, T> {
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
        ok(Num::new(val, range))
    }

    fn degree(&self, n: Num<T>, range: Range) -> crate::Result<Return<T>, T> {
        let val = Val::float(self.to_f64(n)?.to_radians());
        ok(Num { val, range })
    }

    fn factorial(&self, n: Num<T>, range: Range) -> crate::Result<Return<T>, T> {
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
        ok(Num { val, range })
    }

    fn assign(&mut self, var_id: VarId, b: Num<T>, range: Range) -> crate::Result<Return<T>, T> {
        let val = self.plain_val(b)?;
        self.vars[var_id].value = Some(Val::Plain(val));
        Ok(Return::Unit(range))
    }
}

fn ok<T: Ext>(num: Num<T>) -> crate::Result<Return<T>, T> {
    Ok(Return::Num(num))
}

#[cfg(test)]
mod test {
    use crate::{Calc, Context, DummyProvider, Num, PlainVal, Range, Val, Var};

    #[test]
    fn resolve_var() {
        let mut ctx = Context {
            provider: DummyProvider,
            vars: vec![Var {
                name: "x".into(),
                value: Some(Val::int(4)),
            }],
            ..Default::default()
        };
        let calc = Calc::num(Num::new(Val::Var(0), Range::pos(0)));

        let val = ctx.eval(&calc).unwrap().unwrap();
        assert_eq!(PlainVal::Int(4), val);
    }

    #[test]
    fn undefined_var() {
        let mut ctx = Context {
            provider: DummyProvider,
            vars: vec![Var {
                name: "x".into(),
                value: None,
            }],
            ..Default::default()
        };
        let calc = Calc::num(Num::new(Val::Var(0), Range::pos(0)));

        let val = ctx.eval(&calc).unwrap_err();
        assert_eq!(crate::Error::UndefinedVar("x".into(), Range::pos(0)), val);
    }

    #[test]
    fn circular_ref() {
        let mut ctx = Context {
            provider: DummyProvider,
            vars: vec![
                Var {
                    name: "x".into(),
                    value: Some(Val::Var(1)),
                },
                Var {
                    name: "y".into(),
                    value: Some(Val::Var(0)),
                },
            ],
            ..Default::default()
        };
        let calc = Calc::num(Num::new(Val::Var(0), Range::pos(0)));

        let val = ctx.eval(&calc).unwrap_err();
        assert_eq!(
            crate::Error::CircularRef(vec!["x".into(), "y".into(), "x".into()], Range::pos(0)),
            val
        );
    }

    #[test]
    fn self_ref() {
        let mut ctx = Context {
            provider: DummyProvider,
            vars: vec![Var {
                name: "x".into(),
                value: Some(Val::Var(0)),
            }],
            ..Default::default()
        };
        let calc = Calc::num(Num::new(Val::Var(0), Range::pos(0)));

        let val = ctx.eval(&calc).unwrap_err();
        assert_eq!(
            crate::Error::CircularRef(vec!["x".into(), "x".into()], Range::pos(0)),
            val
        );
    }
}
