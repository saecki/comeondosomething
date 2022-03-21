use std::convert::TryFrom;
use std::fmt::Display;

use crate::{Context, Expr, Range, Val, VarId};

#[cfg(test)]
mod test;
mod val;

#[derive(Clone, Debug, PartialEq)]
pub struct Ast {
    pub typ: AstT,
    pub range: Range,
}

impl Ast {
    pub fn new(typ: AstT, range: Range) -> Self {
        Self { typ, range }
    }

    pub fn val(val: Expr) -> Self {
        Self::new(AstT::Expr(val), val.range)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum AstT {
    Empty,
    Error,
    Expr(Expr),
    Assign(VarId, Box<Ast>),
    Neg(Box<Ast>),
    Add(Box<Ast>, Box<Ast>),
    Sub(Box<Ast>, Box<Ast>),
    Mul(Box<Ast>, Box<Ast>),
    Div(Box<Ast>, Box<Ast>),
    IntDiv(Box<Ast>, Box<Ast>),
    Rem(Box<Ast>, Box<Ast>),
    Pow(Box<Ast>, Box<Ast>),
    Eq(Box<Ast>, Box<Ast>),
    Ne(Box<Ast>, Box<Ast>),
    Lt(Box<Ast>, Box<Ast>),
    Le(Box<Ast>, Box<Ast>),
    Gt(Box<Ast>, Box<Ast>),
    Ge(Box<Ast>, Box<Ast>),
    Or(Box<Ast>, Box<Ast>),
    And(Box<Ast>, Box<Ast>),
    BwOr(Box<Ast>, Box<Ast>),
    BwAnd(Box<Ast>, Box<Ast>),
    Not(Box<Ast>),
    Degree(Box<Ast>),
    Radian(Box<Ast>),
    Factorial(Box<Ast>),
    Ln(Box<Ast>),
    Log(Box<Ast>, Box<Ast>),
    Sqrt(Box<Ast>),
    Ncr(Box<Ast>, Box<Ast>),
    Sin(Box<Ast>),
    Cos(Box<Ast>),
    Tan(Box<Ast>),
    Asin(Box<Ast>),
    Acos(Box<Ast>),
    Atan(Box<Ast>),
    Gcd(Box<Ast>, Box<Ast>),
    Min(Vec<Ast>),
    Max(Vec<Ast>),
    Clamp(Box<Ast>, Box<Ast>, Box<Ast>),
    Print(Vec<Ast>),
    Println(Vec<Ast>),
    Spill,
    Assert(Box<Ast>),
    AssertEq(Box<Ast>, Box<Ast>),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Return {
    Val(ValRange),
    Unit(Range),
}

impl Display for Return {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Val(v) => write!(f, "{v}"),
            Self::Unit(_) => write!(f, "()"),
        }
    }
}

impl Return {
    pub fn range(&self) -> Range {
        match self {
            Self::Val(v) => v.range,
            Self::Unit(r) => *r,
        }
    }

    pub fn to_val(&self) -> crate::Result<ValRange> {
        match self {
            Self::Val(v) => Ok(*v),
            Self::Unit(r) => Err(crate::Error::ExpectedValue(*r)),
        }
    }

    pub fn to_f64(&self) -> crate::Result<f64> {
        self.to_val()?.to_f64()
    }

    pub fn to_bool(&self) -> crate::Result<bool> {
        self.to_val()?.to_bool()
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ValRange {
    pub val: Val,
    pub range: Range,
}

impl std::ops::Deref for ValRange {
    type Target = Val;

    fn deref(&self) -> &Self::Target {
        &self.val
    }
}

impl std::ops::DerefMut for ValRange {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.val
    }
}

impl Display for ValRange {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.val)
    }
}

impl ValRange {
    pub const fn new(val: Val, range: Range) -> Self {
        Self { val, range }
    }

    pub fn to_f64(&self) -> crate::Result<f64> {
        match self.val {
            Val::Int(i) => Ok(i as f64),
            Val::Float(f) => Ok(f),
            Val::Bool(_) => Err(crate::Error::ExpectedNumber(*self)),
        }
    }

    pub fn to_bool(&self) -> crate::Result<bool> {
        match self.val {
            Val::Bool(b) => Ok(b),
            Val::Int(_) | Val::Float(_) => Err(crate::Error::ExpectedBool(*self)),
        }
    }
}

impl Context {
    /// Evaluate all ast's and return the last value.
    pub fn eval_all(&mut self, asts: &[Ast]) -> crate::Result<Option<Val>> {
        match asts.split_last() {
            Some((last, others)) => {
                for c in others {
                    self.eval(c)?;
                }
                self.eval(last)
            }
            None => Err(crate::Error::MissingExpr),
        }
    }

    pub fn eval(&mut self, ast: &Ast) -> crate::Result<Option<Val>> {
        match self.eval_ast(ast)? {
            Return::Val(v) => Ok(Some(v.val)),
            Return::Unit(_) => Ok(None),
        }
    }

    pub fn eval_to_vals(&mut self, args: &[Ast]) -> crate::Result<Vec<ValRange>> {
        let mut vals = Vec::with_capacity(args.len());
        for a in args {
            vals.push(self.eval_to_val(a)?);
        }
        Ok(vals)
    }

    pub fn eval_to_val(&mut self, ast: &Ast) -> crate::Result<ValRange> {
        self.eval_ast(ast)?.to_val()
    }

    pub fn eval_to_bool(&mut self, ast: &Ast) -> crate::Result<bool> {
        self.eval_ast(ast)?.to_bool()
    }

    pub fn eval_to_f64(&mut self, ast: &Ast) -> crate::Result<f64> {
        self.eval_ast(ast)?.to_f64()
    }

    pub fn eval_ast(&mut self, ast: &Ast) -> crate::Result<Return> {
        let r = ast.range;
        match &ast.typ {
            AstT::Empty => Ok(Return::Unit(r)),
            AstT::Error => Err(crate::Error::Parsing(r)),
            AstT::Expr(e) => ok(self.to_val(e)?, r),
            AstT::Assign(a, b) => self.assign(*a, b, r),
            AstT::Neg(a) => self.neg(a, r),
            AstT::Add(a, b) => self.add(a, b, r),
            AstT::Sub(a, b) => self.sub(a, b, r),
            AstT::Mul(a, b) => self.mul(a, b, r),
            AstT::Div(a, b) => self.div(a, b, r),
            AstT::IntDiv(a, b) => self.int_div(a, b, r),
            AstT::Rem(a, b) => self.rem(a, b, r),
            AstT::Pow(a, b) => self.pow(a, b, r),
            AstT::Eq(a, b) => self.eq(a, b, r),
            AstT::Ne(a, b) => self.ne(a, b, r),
            AstT::Lt(a, b) => self.lt(a, b, r),
            AstT::Le(a, b) => self.le(a, b, r),
            AstT::Gt(a, b) => self.gt(a, b, r),
            AstT::Ge(a, b) => self.ge(a, b, r),
            AstT::Or(a, b) => self.or(a, b, r),
            AstT::And(a, b) => self.and(a, b, r),
            AstT::BwOr(a, b) => self.bw_or(a, b, r),
            AstT::BwAnd(a, b) => self.bw_and(a, b, r),
            AstT::Not(a) => self.not(a, r),
            AstT::Degree(a) => self.degree(a, r),
            AstT::Radian(a) => self.radian(a, r),
            AstT::Factorial(a) => self.factorial(a, r),
            AstT::Ln(a) => self.ln(a, r),
            AstT::Log(a, b) => self.log(a, b, r),
            AstT::Sqrt(a) => self.sqrt(a, r),
            AstT::Ncr(a, b) => self.ncr(a, b, r),
            AstT::Sin(a) => self.sin(a, r),
            AstT::Cos(a) => self.cos(a, r),
            AstT::Tan(a) => self.tan(a, r),
            AstT::Asin(a) => self.asin(a, r),
            AstT::Acos(a) => self.acos(a, r),
            AstT::Atan(a) => self.atan(a, r),
            AstT::Gcd(a, b) => self.gcd(a, b, r),
            AstT::Min(args) => self.min(args, r),
            AstT::Max(args) => self.max(args, r),
            AstT::Clamp(num, min, max) => self.clamp(num, min, max, r),
            AstT::Print(args) => self.print(args, r),
            AstT::Println(args) => self.println(args, r),
            AstT::Spill => self.spill(r),
            AstT::Assert(a) => self.assert(a, r),
            AstT::AssertEq(a, b) => self.assert_eq(a, b, r),
        }
        .map(|mut r| {
            if let Return::Val(v) = &mut r {
                if let Some(i) = v.to_int() {
                    v.val = Val::Int(i);
                }
            }
            r
        })
    }

    fn assign(&mut self, id: VarId, n: &Ast, range: Range) -> crate::Result<Return> {
        let v = self.eval_to_val(n)?;
        self.set_var(id, Some(v.val));
        Ok(Return::Unit(range))
    }

    fn neg(&mut self, n: &Ast, range: Range) -> crate::Result<Return> {
        let v = self.eval_to_val(n)?;
        let val = match v.val {
            Val::Int(i) => Val::Int(-i),
            _ => Val::Float(-v.to_f64()?),
        };
        ok(val, range)
    }

    fn add(&mut self, n1: &Ast, n2: &Ast, range: Range) -> crate::Result<Return> {
        let va = self.eval_to_val(n1)?;
        let vb = self.eval_to_val(n2)?;

        let val = match (va.val, vb.val) {
            (Val::Int(a), Val::Int(b)) => match a.checked_add(b) {
                Some(v) => Val::Int(v),
                None => return Err(crate::Error::AddOverflow(va, vb)),
            },
            _ => Val::Float(va.to_f64()? + vb.to_f64()?),
        };
        ok(val, range)
    }

    fn sub(&mut self, n1: &Ast, n2: &Ast, range: Range) -> crate::Result<Return> {
        let va = self.eval_to_val(n1)?;
        let vb = self.eval_to_val(n2)?;

        let val = match (va.val, vb.val) {
            (Val::Int(a), Val::Int(b)) => match a.checked_sub(b) {
                Some(v) => Val::Int(v),
                None => return Err(crate::Error::SubOverflow(va, vb)),
            },
            _ => Val::Float(va.to_f64()? - vb.to_f64()?),
        };
        ok(val, range)
    }

    fn mul(&mut self, n1: &Ast, n2: &Ast, range: Range) -> crate::Result<Return> {
        let va = self.eval_to_val(n1)?;
        let vb = self.eval_to_val(n2)?;

        let val = match (va.val, vb.val) {
            (Val::Int(a), Val::Int(b)) => match a.checked_mul(b) {
                Some(v) => Val::Int(v),
                None => return Err(crate::Error::MulOverflow(va, vb)),
            },
            _ => Val::Float(va.to_f64()? * vb.to_f64()?),
        };
        ok(val, range)
    }

    fn div(&mut self, n1: &Ast, n2: &Ast, range: Range) -> crate::Result<Return> {
        let va = self.eval_to_val(n1)?;
        let vb = self.eval_to_val(n2)?;

        let val = match (va.val, vb.val) {
            (Val::Int(a), Val::Int(b)) => {
                if b == 0 {
                    return Err(crate::Error::DivideByZero(va, vb));
                } else if a % b == 0 {
                    Val::Int(a / b)
                } else {
                    Val::Float(a as f64 / b as f64)
                }
            }
            _ => {
                let divisor = vb.to_f64()?;
                if divisor == 0.0 {
                    return Err(crate::Error::DivideByZero(va, vb));
                } else {
                    Val::Float(va.to_f64()? / divisor)
                }
            }
        };
        ok(val, range)
    }

    fn int_div(&mut self, n1: &Ast, n2: &Ast, range: Range) -> crate::Result<Return> {
        let va = self.eval_to_val(n1)?;
        let vb = self.eval_to_val(n2)?;

        let val = match (va.val, vb.val) {
            (Val::Int(a), Val::Int(b)) => {
                if b == 0 {
                    return Err(crate::Error::DivideByZero(va, vb));
                } else {
                    Val::Int(a / b)
                }
            }
            _ => return Err(crate::Error::FractionEuclidDiv(va, vb)),
        };
        ok(val, range)
    }

    fn rem(&mut self, n1: &Ast, n2: &Ast, range: Range) -> crate::Result<Return> {
        let va = self.eval_to_val(n1)?;
        let vb = self.eval_to_val(n2)?;

        let val = match (va.val, vb.val) {
            (Val::Int(a), Val::Int(b)) => {
                if b == 0 {
                    return Err(crate::Error::RemainderByZero(va, vb));
                } else {
                    let r = a % b;
                    if (r > 0 && b < 0) || (r < 0 && b > 0) {
                        Val::Int(r + b)
                    } else {
                        Val::Int(r)
                    }
                }
            }
            _ => return Err(crate::Error::FractionRemainder(va, vb)),
        };
        ok(val, range)
    }

    fn pow(&mut self, n1: &Ast, n2: &Ast, range: Range) -> crate::Result<Return> {
        let va = self.eval_to_val(n1)?;
        let vb = self.eval_to_val(n2)?;

        let val = match (va.val, vb.val) {
            (Val::Int(base), Val::Int(exp)) => {
                if let Ok(e) = u32::try_from(exp) {
                    Val::Int(base.pow(e))
                } else if let Ok(e) = i32::try_from(exp) {
                    Val::Float((base as f64).powi(e))
                } else {
                    return Err(crate::Error::PowOverflow(va, vb));
                }
            }
            _ => Val::Float(va.to_f64()?.powf(vb.to_f64()?)),
        };
        ok(val, range)
    }

    fn eq(&mut self, a: &Ast, b: &Ast, range: Range) -> crate::Result<Return> {
        let a = self.eval_to_val(a)?;
        let b = self.eval_to_val(b)?;

        ok(Val::Bool(a.val == b.val), range)
    }

    fn ne(&mut self, a: &Ast, b: &Ast, range: Range) -> crate::Result<Return> {
        let a = self.eval_to_val(a)?;
        let b = self.eval_to_val(b)?;

        ok(Val::Bool(a.val != b.val), range)
    }

    fn lt(&mut self, a: &Ast, b: &Ast, range: Range) -> crate::Result<Return> {
        let va = self.eval_to_f64(a)?;
        let vb = self.eval_to_f64(b)?;

        ok(Val::Bool(va < vb), range)
    }

    fn le(&mut self, a: &Ast, b: &Ast, range: Range) -> crate::Result<Return> {
        let va = self.eval_to_f64(a)?;
        let vb = self.eval_to_f64(b)?;

        ok(Val::Bool(va <= vb), range)
    }

    fn gt(&mut self, a: &Ast, b: &Ast, range: Range) -> crate::Result<Return> {
        let va = self.eval_to_f64(a)?;
        let vb = self.eval_to_f64(b)?;

        ok(Val::Bool(va > vb), range)
    }

    fn ge(&mut self, a: &Ast, b: &Ast, range: Range) -> crate::Result<Return> {
        let va = self.eval_to_f64(a)?;
        let vb = self.eval_to_f64(b)?;

        ok(Val::Bool(va >= vb), range)
    }

    fn or(&mut self, a: &Ast, b: &Ast, range: Range) -> crate::Result<Return> {
        let a = self.eval_to_bool(a)?;
        let b = self.eval_to_bool(b)?;

        ok(Val::Bool(a || b), range)
    }

    fn and(&mut self, a: &Ast, b: &Ast, range: Range) -> crate::Result<Return> {
        let a = self.eval_to_bool(a)?;
        let b = self.eval_to_bool(b)?;

        ok(Val::Bool(a && b), range)
    }

    fn bw_or(&mut self, a: &Ast, b: &Ast, range: Range) -> crate::Result<Return> {
        let va = self.eval_to_val(a)?;
        let vb = self.eval_to_val(b)?;

        let val = match (va.val, vb.val) {
            (Val::Int(a), Val::Int(b)) => Val::Int(a | b),
            (Val::Bool(a), Val::Bool(b)) => Val::Bool(a | b),
            _ => return Err(crate::Error::InvalidBwOr(va, vb)),
        };

        ok(val, range)
    }

    fn bw_and(&mut self, a: &Ast, b: &Ast, range: Range) -> crate::Result<Return> {
        let va = self.eval_to_val(a)?;
        let vb = self.eval_to_val(b)?;

        let val = match (va.val, vb.val) {
            (Val::Int(a), Val::Int(b)) => Val::Int(a & b),
            (Val::Bool(a), Val::Bool(b)) => Val::Bool(a & b),
            _ => return Err(crate::Error::InvalidBwAnd(va, vb)),
        };

        ok(val, range)
    }

    fn not(&mut self, a: &Ast, range: Range) -> crate::Result<Return> {
        let va = self.eval_to_bool(a)?;
        ok(Val::Bool(!va), range)
    }

    // TODO add a angle value type as input for trigeometrical functions
    fn degree(&mut self, n: &Ast, range: Range) -> crate::Result<Return> {
        let rad = self.eval_to_f64(n)?.to_radians();
        ok(Val::Float(rad), range)
    }

    fn radian(&mut self, n: &Ast, range: Range) -> crate::Result<Return> {
        let rad = self.eval_to_f64(n)?;
        ok(Val::Float(rad), range)
    }

    fn factorial(&mut self, n: &Ast, range: Range) -> crate::Result<Return> {
        let v = self.eval_to_val(n)?;
        match v.val {
            Val::Int(i) => {
                if i < 0 {
                    Err(crate::Error::NegativeFactorial(v))
                } else {
                    let mut f: i128 = 1;
                    for i in 1..=i {
                        match f.checked_mul(i) {
                            Some(v) => f = v,
                            None => return Err(crate::Error::FactorialOverflow(v)),
                        }
                    }

                    ok(Val::Int(f), range)
                }
            }
            _ => Err(crate::Error::FractionFactorial(v)),
        }
    }

    fn ln(&mut self, n: &Ast, range: Range) -> crate::Result<Return> {
        let val = self.eval_to_f64(n)?.ln();
        ok(Val::Float(val), range)
    }

    fn log(&mut self, base: &Ast, num: &Ast, range: Range) -> crate::Result<Return> {
        let b = self.eval_to_f64(base)?;
        let n = self.eval_to_f64(num)?;
        let val = Val::Float(n.log(b));
        ok(val, range)
    }

    fn sqrt(&mut self, n: &Ast, range: Range) -> crate::Result<Return> {
        let val = self.eval_to_f64(n)?.sqrt();
        ok(Val::Float(val), range)
    }

    fn ncr(&mut self, n1: &Ast, n2: &Ast, range: Range) -> crate::Result<Return> {
        let va = self.eval_to_val(n1)?;
        let vb = self.eval_to_val(n2)?;
        let val = match (va.val, vb.val) {
            (Val::Int(n), Val::Int(mut r)) => {
                if r < 0 {
                    return Err(crate::Error::NegativeNcr(vb));
                }
                if n < r {
                    return Err(crate::Error::InvalidNcr(va, vb));
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

                Val::Int(val)
            }
            _ => return Err(crate::Error::FractionNcr(va, vb)),
        };
        ok(val, range)
    }

    fn sin(&mut self, n: &Ast, range: Range) -> crate::Result<Return> {
        let a = self.eval_to_f64(n)?.sin();
        ok(Val::Float(a), range)
    }

    fn cos(&mut self, n: &Ast, range: Range) -> crate::Result<Return> {
        let a = self.eval_to_f64(n)?.cos();
        ok(Val::Float(a), range)
    }

    fn tan(&mut self, n: &Ast, range: Range) -> crate::Result<Return> {
        let a = self.eval_to_f64(n)?.tan();
        ok(Val::Float(a), range)
    }

    fn asin(&mut self, n: &Ast, range: Range) -> crate::Result<Return> {
        let a = self.eval_to_f64(n)?.asin();
        ok(Val::Float(a), range)
    }

    fn acos(&mut self, n: &Ast, range: Range) -> crate::Result<Return> {
        let a = self.eval_to_f64(n)?.acos();
        ok(Val::Float(a), range)
    }

    fn atan(&mut self, n: &Ast, range: Range) -> crate::Result<Return> {
        let a = self.eval_to_f64(n)?.atan();
        ok(Val::Float(a), range)
    }

    fn gcd(&mut self, n1: &Ast, n2: &Ast, range: Range) -> crate::Result<Return> {
        let va = self.eval_to_val(n1)?;
        let vb = self.eval_to_val(n2)?;
        match (va.val, vb.val) {
            (Val::Int(mut a), Val::Int(mut b)) => {
                let mut _t = 0;
                while b != 0 {
                    _t = b;
                    b = a % b;
                    a = _t;
                }
                ok(Val::Int(a), range)
            }
            _ => Err(crate::Error::FractionGcd(va, vb)),
        }
    }

    fn min(&mut self, args: &[Ast], range: Range) -> crate::Result<Return> {
        let mut min = None;
        for a in args {
            let val = self.eval_to_val(a)?.to_f64()?;
            match min {
                None => min = Some(val),
                Some(m) => {
                    if val < m {
                        min = Some(val);
                    }
                }
            }
        }

        let max = min.expect("Iterator should at least contain 1 element");
        ok(Val::Float(max), range)
    }

    fn max(&mut self, args: &[Ast], range: Range) -> crate::Result<Return> {
        let mut max = None;
        for a in args {
            let val = self.eval_to_f64(a)?;
            match max {
                None => max = Some(val),
                Some(m) => {
                    if val > m {
                        max = Some(val);
                    }
                }
            }
        }

        let max = max.expect("Iterator should at least contain 1 element");
        ok(Val::Float(max), range)
    }

    fn clamp(&mut self, num: &Ast, min: &Ast, max: &Ast, range: Range) -> crate::Result<Return> {
        let vnum = self.eval_to_val(num)?;
        let vmin = self.eval_to_val(min)?;
        let vmax = self.eval_to_val(max)?;

        let val = match (vnum.val, vmin.val, vmax.val) {
            (Val::Int(num), Val::Int(min), Val::Int(max)) => {
                if min > max {
                    return Err(crate::Error::InvalidClampBounds(vmin, vmax));
                }
                Val::Int(num.clamp(min, max))
            }
            _ => {
                let num = vnum.to_f64()?;
                let min = vmin.to_f64()?;
                let max = vmax.to_f64()?;
                // floating point weirdness, negated assertion of stdlib
                #[allow(clippy::neg_cmp_op_on_partial_ord)]
                if !(min <= max) {
                    return Err(crate::Error::InvalidClampBounds(vmin, vmax));
                }
                Val::Float(num.clamp(min, max))
            }
        };
        ok(val, range)
    }

    fn print(&mut self, args: &[Ast], range: Range) -> crate::Result<Return> {
        let vals = self.eval_to_vals(args)?;
        if let Some((first, others)) = vals.split_first() {
            print!("{first}");
            for v in others {
                print!(" {v}");
            }
        }
        Ok(Return::Unit(range))
    }

    fn println(&mut self, args: &[Ast], range: Range) -> crate::Result<Return> {
        self.print(args, range)?;
        println!();
        Ok(Return::Unit(range))
    }

    fn spill(&mut self, range: Range) -> crate::Result<Return> {
        for var in self.vars.iter() {
            if let Some(val) = var.value {
                println!("{} = {}", var.name, val);
            }
        }
        Ok(Return::Unit(range))
    }

    fn assert(&mut self, a: &Ast, range: Range) -> crate::Result<Return> {
        let va = self.eval_to_bool(a)?;

        if !va {
            return Err(crate::Error::AssertFailed(a.range));
        }

        Ok(Return::Unit(range))
    }

    fn assert_eq(&mut self, a: &Ast, b: &Ast, range: Range) -> crate::Result<Return> {
        let va = self.eval_to_val(a)?;
        let vb = self.eval_to_val(b)?;

        if va.val != vb.val {
            return Err(crate::Error::AssertEqFailed(va, vb));
        }

        Ok(Return::Unit(range))
    }
}

fn ok(val: Val, range: Range) -> crate::Result<Return> {
    Ok(Return::Val(ValRange::new(val, range)))
}
