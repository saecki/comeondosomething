use std::convert::TryFrom;

use crate::{Context, Data, Range, Val, ValT, VarId};

#[cfg(test)]
mod test;
mod val;

#[derive(Clone, Debug, PartialEq)]
pub struct Expr {
    pub typ: ExprType,
    pub range: Range,
}

impl Expr {
    pub fn new(typ: ExprType, range: Range) -> Self {
        Self { typ, range }
    }

    pub fn val(val: Val) -> Self {
        Self::new(ExprType::Val(val), val.range)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExprType {
    Empty,
    Error,
    Val(Val),
    Neg(Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    IntDiv(Box<Expr>, Box<Expr>),
    Rem(Box<Expr>, Box<Expr>),
    Pow(Box<Expr>, Box<Expr>),
    Ln(Box<Expr>),
    Log(Box<Expr>, Box<Expr>),
    Sqrt(Box<Expr>),
    Ncr(Box<Expr>, Box<Expr>),
    Sin(Box<Expr>),
    Cos(Box<Expr>),
    Tan(Box<Expr>),
    Asin(Box<Expr>),
    Acos(Box<Expr>),
    Atan(Box<Expr>),
    Gcd(Box<Expr>, Box<Expr>),
    Min(Vec<Expr>),
    Max(Vec<Expr>),
    Clamp(Box<Expr>, Box<Expr>, Box<Expr>),
    Degree(Box<Expr>),
    Factorial(Box<Expr>),
    Assignment(VarId, Box<Expr>),
    Print(Vec<Expr>),
    Println(Vec<Expr>),
    Spill,
}

pub enum ValResult {
    Resolved(Data),
    Undefined(String),
    CircularRef(Vec<String>),
}

pub enum Return {
    Val(Val),
    Unit(Range),
}

impl Context {
    /// Evaluate all expressions and return the last value.
    pub fn eval_all(&mut self, exprs: &[Expr]) -> crate::Result<Option<Data>> {
        match exprs.split_last() {
            Some((last, others)) => {
                for c in others {
                    self.eval(c)?;
                }
                self.eval(last)
            }
            None => Err(crate::Error::MissingExpr),
        }
    }

    pub fn eval(&mut self, expr: &Expr) -> crate::Result<Option<Data>> {
        match self.eval_expr(expr)? {
            Return::Data(n) => Ok(Some(self.to_data(n)?)),
            Return::Unit(_) => Ok(None),
        }
    }

    pub fn eval_nums(&mut self, args: &[Expr]) -> crate::Result<Vec<Val>> {
        let mut nums = Vec::with_capacity(args.len());
        for a in args {
            nums.push(self.eval_num(a)?);
        }
        Ok(nums)
    }

    pub fn eval_num(&mut self, expr: &Expr) -> crate::Result<Val> {
        match self.eval_expr(expr)? {
            Return::Data(v) => Ok(v),
            Return::Unit(r) => Err(crate::Error::ExpectedValue(r)),
        }
    }

    pub fn eval_expr(&mut self, expr: &Expr) -> crate::Result<Return> {
        let r = expr.range;
        match &expr.typ {
            ExprType::Empty => Ok(Return::Unit(r)),
            ExprType::Error => Err(crate::Error::Parsing(r)),
            ExprType::Val(n) => ok(*n),
            ExprType::Neg(a) => {
                let n = self.eval_num(a)?;
                self.neg(n, r)
            }
            ExprType::Add(a, b) => {
                let n1 = self.eval_num(a)?;
                let n2 = self.eval_num(b)?;
                self.add(n1, n2, r)
            }
            ExprType::Sub(a, b) => {
                let n1 = self.eval_num(a)?;
                let n2 = self.eval_num(b)?;
                self.sub(n1, n2, r)
            }
            ExprType::Mul(a, b) => {
                let n1 = self.eval_num(a)?;
                let n2 = self.eval_num(b)?;
                self.mul(n1, n2, r)
            }
            ExprType::Div(a, b) => {
                let n1 = self.eval_num(a)?;
                let n2 = self.eval_num(b)?;
                self.div(n1, n2, r)
            }
            ExprType::IntDiv(a, b) => {
                let n1 = self.eval_num(a)?;
                let n2 = self.eval_num(b)?;
                self.int_div(n1, n2, r)
            }
            ExprType::Rem(a, b) => {
                let n1 = self.eval_num(a)?;
                let n2 = self.eval_num(b)?;
                self.rem(n1, n2, r)
            }
            ExprType::Pow(a, b) => {
                let n1 = self.eval_num(a)?;
                let n2 = self.eval_num(b)?;
                self.pow(n1, n2, r)
            }
            ExprType::Ln(a) => {
                let n1 = self.eval_num(a)?;
                self.ln(n1, r)
            }
            ExprType::Log(a, b) => {
                let n1 = self.eval_num(a)?;
                let n2 = self.eval_num(b)?;
                self.log(n1, n2, r)
            }
            ExprType::Sqrt(a) => {
                let n1 = self.eval_num(a)?;
                self.sqrt(n1, r)
            }
            ExprType::Ncr(a, b) => {
                let n1 = self.eval_num(a)?;
                let n2 = self.eval_num(b)?;
                self.ncr(n1, n2, r)
            }
            ExprType::Sin(a) => {
                let n2 = self.eval_num(a)?;
                self.sin(n2, r)
            }
            ExprType::Cos(a) => {
                let n = self.eval_num(a)?;
                self.cos(n, r)
            }
            ExprType::Tan(a) => {
                let n = self.eval_num(a)?;
                self.tan(n, r)
            }
            ExprType::Asin(a) => {
                let n = self.eval_num(a)?;
                self.asin(n, r)
            }
            ExprType::Acos(a) => {
                let n = self.eval_num(a)?;
                self.acos(n, r)
            }
            ExprType::Atan(a) => {
                let n = self.eval_num(a)?;
                self.atan(n, r)
            }
            ExprType::Gcd(a, b) => {
                let n1 = self.eval_num(a)?;
                let n2 = self.eval_num(b)?;
                self.gcd(n1, n2, r)
            }
            ExprType::Min(args) => {
                let nums = self.eval_nums(args)?;
                self.min(nums, r)
            }
            ExprType::Max(args) => {
                let nums = self.eval_nums(args)?;
                self.max(nums, r)
            }
            ExprType::Clamp(num, min, max) => {
                let n1 = self.eval_num(num)?;
                let n2 = self.eval_num(min)?;
                let n3 = self.eval_num(max)?;
                self.clamp(n1, n2, n3, r)
            }
            ExprType::Degree(a) => {
                let n = self.eval_num(a)?;
                self.degree(n, r) // TODO add rad modifier and require a typed angle value as input for trigeometrical functions
            }
            ExprType::Factorial(a) => {
                let n = self.eval_num(a)?;
                self.factorial(n, r)
            }
            ExprType::Assignment(a, b) => {
                let n = self.eval_num(b)?;
                self.assign(*a, n, r)
            }
            ExprType::Print(args) => {
                let nums = self.eval_nums(args)?;
                self.print(nums, r)
            }
            ExprType::Println(args) => {
                let nums = self.eval_nums(args)?;
                self.println(nums, r)
            }
            ExprType::Spill => self.spill(r),
        }
        .map(|mut r| {
            if let Return::Data(n) = &mut r {
                n.typ = n.typ.maybe_int();
            }
            r
        })
    }

    fn neg(&self, n: Val, range: Range) -> crate::Result<Return> {
        let val = match n.typ {
            ValT::Data(Data::Int(i)) => ValT::int(-i),
            _ => ValT::float(-self.to_f64(n)?),
        };
        ok(Val { typ: val, range })
    }

    fn add(&self, n1: Val, n2: Val, range: Range) -> crate::Result<Return> {
        let val = match (self.to_int(n1)?, self.to_int(n2)?) {
            (Some(a), Some(b)) => match a.checked_add(b) {
                Some(v) => ValT::int(v),
                None => return Err(crate::Error::AddOverflow(n1, n2)),
            },
            _ => ValT::float(self.to_f64(n1)? + self.to_f64(n2)?),
        };
        ok(Val { typ: val, range })
    }

    fn sub(&self, n1: Val, n2: Val, range: Range) -> crate::Result<Return> {
        let val = match (self.to_int(n1)?, self.to_int(n2)?) {
            (Some(a), Some(b)) => match a.checked_sub(b) {
                Some(v) => ValT::int(v),
                None => return Err(crate::Error::SubOverflow(n1, n2)),
            },
            _ => ValT::float(self.to_f64(n1)? - self.to_f64(n2)?),
        };
        ok(Val { typ: val, range })
    }

    fn mul(&self, n1: Val, n2: Val, range: Range) -> crate::Result<Return> {
        let val = match (self.to_int(n1)?, self.to_int(n2)?) {
            (Some(a), Some(b)) => match a.checked_mul(b) {
                Some(v) => ValT::int(v),
                None => return Err(crate::Error::MulOverflow(n1, n2)),
            },
            _ => ValT::float(self.to_f64(n1)? * self.to_f64(n2)?),
        };
        ok(Val { typ: val, range })
    }

    fn div(&self, n1: Val, n2: Val, range: Range) -> crate::Result<Return> {
        let val = match (self.to_int(n1)?, self.to_int(n2)?) {
            (Some(a), Some(b)) => {
                if b == 0 {
                    return Err(crate::Error::DivideByZero(n1, n2));
                } else if a % b == 0 {
                    ValT::int(a / b)
                } else {
                    ValT::float(a as f64 / b as f64)
                }
            }
            _ => {
                let divisor = self.to_f64(n2)?;
                if divisor == 0.0 {
                    return Err(crate::Error::DivideByZero(n1, n2));
                } else {
                    ValT::float(self.to_f64(n1)? / divisor)
                }
            }
        };
        ok(Val { typ: val, range })
    }

    fn int_div(&self, n1: Val, n2: Val, range: Range) -> crate::Result<Return> {
        let val = match (self.to_int(n1)?, self.to_int(n2)?) {
            (Some(a), Some(b)) => {
                if b == 0 {
                    return Err(crate::Error::DivideByZero(n1, n2));
                } else {
                    ValT::int(a / b)
                }
            }
            _ => return Err(crate::Error::FractionEuclidDiv(n1, n2)),
        };
        ok(Val { typ: val, range })
    }

    fn rem(&self, n1: Val, n2: Val, range: Range) -> crate::Result<Return> {
        let val = match (self.to_int(n1)?, self.to_int(n2)?) {
            (Some(a), Some(b)) => {
                if b == 0 {
                    return Err(crate::Error::RemainderByZero(n1, n2));
                } else {
                    let r = a % b;
                    if (r > 0 && b < 0) || (r < 0 && b > 0) {
                        ValT::int(r + b)
                    } else {
                        ValT::int(r)
                    }
                }
            }
            _ => return Err(crate::Error::FractionRemainder(n1, n2)),
        };
        ok(Val { typ: val, range })
    }

    fn pow(&self, n1: Val, n2: Val, range: Range) -> crate::Result<Return> {
        let val = match (self.to_int(n1)?, self.to_int(n2)?) {
            (Some(a), Some(b)) => {
                if let Ok(exp) = u32::try_from(b) {
                    ValT::int(a.pow(exp))
                } else if let Ok(exp) = i32::try_from(b) {
                    ValT::float((a as f64).powi(exp))
                } else {
                    ValT::float((a as f64).powf(b as f64))
                }
            }
            (None, Some(b)) => {
                if let Ok(exp) = i32::try_from(b) {
                    ValT::float((self.to_f64(n1)?).powi(exp))
                } else {
                    ValT::float((self.to_f64(n1)?).powf(b as f64))
                }
            }
            _ => ValT::float(self.to_f64(n1)?.powf(self.to_f64(n2)?)),
        };
        ok(Val { typ: val, range })
    }

    fn ln(&self, n: Val, range: Range) -> crate::Result<Return> {
        let val = ValT::float(self.to_f64(n)?.ln());
        ok(Val { typ: val, range })
    }

    fn log(&self, base: Val, n: Val, range: Range) -> crate::Result<Return> {
        let val = ValT::float(self.to_f64(n)?.log(self.to_f64(base)?));
        ok(Val { typ: val, range })
    }

    fn sqrt(&self, n: Val, range: Range) -> crate::Result<Return> {
        let val = ValT::float(self.to_f64(n)?.sqrt());
        ok(Val { typ: val, range })
    }

    fn ncr(&self, n1: Val, n2: Val, range: Range) -> crate::Result<Return> {
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

                ValT::int(val)
            }
            _ => return Err(crate::Error::FractionNcr(n1, n2)),
        };
        ok(Val { typ: val, range })
    }

    fn sin(&self, n: Val, range: Range) -> crate::Result<Return> {
        let val = ValT::float(self.to_f64(n)?.sin());
        ok(Val { typ: val, range })
    }

    fn cos(&self, n: Val, range: Range) -> crate::Result<Return> {
        let val = ValT::float(self.to_f64(n)?.cos());
        ok(Val { typ: val, range })
    }

    fn tan(&self, n: Val, range: Range) -> crate::Result<Return> {
        let val = ValT::float(self.to_f64(n)?.tan());
        ok(Val { typ: val, range })
    }

    fn asin(&self, n: Val, range: Range) -> crate::Result<Return> {
        let val = ValT::float(self.to_f64(n)?.asin());
        ok(Val { typ: val, range })
    }

    fn acos(&self, n: Val, range: Range) -> crate::Result<Return> {
        let val = ValT::float(self.to_f64(n)?.acos());
        ok(Val { typ: val, range })
    }

    fn atan(&self, n: Val, range: Range) -> crate::Result<Return> {
        let val = ValT::float(self.to_f64(n)?.atan());
        ok(Val { typ: val, range })
    }

    fn gcd(&self, n1: Val, n2: Val, range: Range) -> crate::Result<Return> {
        match (self.to_int(n1)?, self.to_int(n2)?) {
            (Some(mut a), Some(mut b)) => {
                let mut _t = 0;
                while b != 0 {
                    _t = b;
                    b = a % b;
                    a = _t;
                }
                let val = ValT::int(a);
                ok(Val::new(val, range))
            }
            _ => Err(crate::Error::FractionGcd(n1, n2)),
        }
    }

    fn min(&self, nums: Vec<Val>, range: Range) -> crate::Result<Return> {
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
        ok(Val::new(min.typ, range))
    }

    fn max(&self, nums: Vec<Val>, range: Range) -> crate::Result<Return> {
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
        ok(Val::new(max.typ, range))
    }

    fn clamp(&self, num: Val, min: Val, max: Val, range: Range) -> crate::Result<Return> {
        let val = match (self.to_int(num)?, self.to_int(min)?, self.to_int(max)?) {
            (Some(v), Some(lo), Some(hi)) => {
                if lo > hi {
                    return Err(crate::Error::InvalidClampBounds(min, max));
                }
                ValT::int(v.clamp(lo, hi))
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
                ValT::float(v.clamp(lo, hi))
            }
        };
        ok(Val::new(val, range))
    }

    fn degree(&self, n: Val, range: Range) -> crate::Result<Return> {
        let val = ValT::float(self.to_f64(n)?.to_radians());
        ok(Val { typ: val, range })
    }

    fn factorial(&self, n: Val, range: Range) -> crate::Result<Return> {
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
                    ValT::int(val)
                }
            }
            _ => return Err(crate::Error::FractionFactorial(n)),
        };
        ok(Val { typ: val, range })
    }

    fn assign(&mut self, id: VarId, b: Val, range: Range) -> crate::Result<Return> {
        let val = self.to_data(b)?;
        self.var_mut(id).value = Some(ValT::Data(val));
        Ok(Return::Unit(range))
    }

    fn print(&mut self, nums: Vec<Val>, range: Range) -> crate::Result<Return> {
        if let Some((first, others)) = nums.split_first() {
            print!("{}", self.to_data(*first)?);
            for n in others {
                print!(" {}", self.to_data(*n)?);
            }
        }
        Ok(Return::Unit(range))
    }

    fn println(&mut self, nums: Vec<Val>, range: Range) -> crate::Result<Return> {
        self.print(nums, range)?;
        println!();
        Ok(Return::Unit(range))
    }

    fn spill(&mut self, range: Range) -> crate::Result<Return> {
        for var in self.vars.iter() {
            if let Some(val) = var.value {
                if let ValResult::Resolved(v) = self.resolve_val(val) {
                    println!("{} = {}", var.name, v);
                }
            }
        }
        Ok(Return::Unit(range))
    }
}

fn ok(num: Val) -> crate::Result<Return> {
    Ok(Return::Data(num))
}
