use std::convert::TryFrom;

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
    Neg(Box<Ast>),
    Add(Box<Ast>, Box<Ast>),
    Sub(Box<Ast>, Box<Ast>),
    Mul(Box<Ast>, Box<Ast>),
    Div(Box<Ast>, Box<Ast>),
    IntDiv(Box<Ast>, Box<Ast>),
    Rem(Box<Ast>, Box<Ast>),
    Pow(Box<Ast>, Box<Ast>),
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
    Degree(Box<Ast>),
    Factorial(Box<Ast>),
    Assignment(VarId, Box<Ast>),
    Print(Vec<Ast>),
    Println(Vec<Ast>),
    Spill,
}

pub enum Return {
    Val(Val, Range),
    Unit(Range),
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
            Return::Val(d, _) => Ok(Some(d)),
            Return::Unit(_) => Ok(None),
        }
    }

    pub fn eval_to_vals(&mut self, args: &[Ast]) -> crate::Result<Vec<(Val, Range)>> {
        let mut vals = Vec::with_capacity(args.len());
        for a in args {
            vals.push(self.eval_to_val(a)?);
        }
        Ok(vals)
    }

    pub fn eval_to_val(&mut self, ast: &Ast) -> crate::Result<(Val, Range)> {
        match self.eval_ast(ast)? {
            Return::Val(v, r) => Ok((v, r)),
            Return::Unit(r) => Err(crate::Error::ExpectedValue(r)),
        }
    }

    pub fn eval_to_f64(&mut self, ast: &Ast) -> crate::Result<f64> {
        match self.eval_ast(ast)? {
            Return::Val(v, _) => Ok(v.to_f64()),
            Return::Unit(r) => Err(crate::Error::ExpectedValue(r)),
        }
    }

    pub fn eval_ast(&mut self, ast: &Ast) -> crate::Result<Return> {
        let r = ast.range;
        match &ast.typ {
            AstT::Empty => Ok(Return::Unit(r)),
            AstT::Error => Err(crate::Error::Parsing(r)),
            AstT::Expr(e) => ok(self.to_val(e)?, r),
            AstT::Neg(a) => self.neg(a, r),
            AstT::Add(a, b) => self.add(a, b, r),
            AstT::Sub(a, b) => self.sub(a, b, r),
            AstT::Mul(a, b) => self.mul(a, b, r),
            AstT::Div(a, b) => self.div(a, b, r),
            AstT::IntDiv(a, b) => self.int_div(a, b, r),
            AstT::Rem(a, b) => self.rem(a, b, r),
            AstT::Pow(a, b) => self.pow(a, b, r),
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
            AstT::Degree(a) => self.degree(a, r), // TODO add rad modifier and require a typed angle value as input for trigeometrical functions
            AstT::Factorial(a) => self.factorial(a, r),
            AstT::Assignment(a, b) => self.assign(*a, b, r),
            AstT::Print(args) => self.print(args, r),
            AstT::Println(args) => self.println(args, r),
            AstT::Spill => self.spill(r),
        }
        .map(|mut r| {
            if let Return::Val(v, _) = &mut r {
                if let Some(i) = v.to_int() {
                    *v = Val::Int(i);
                }
            }
            r
        })
    }

    fn neg(&mut self, n: &Ast, range: Range) -> crate::Result<Return> {
        let (a, _) = self.eval_to_val(n)?;
        let val = match a {
            Val::Int(i) => Val::Int(-i),
            _ => Val::Float(a.to_f64()),
        };
        ok(val, range)
    }

    fn add(&mut self, n1: &Ast, n2: &Ast, range: Range) -> crate::Result<Return> {
        let (a, a_r) = self.eval_to_val(n1)?;
        let (b, b_r) = self.eval_to_val(n2)?;

        let val = match (a, b) {
            (Val::Int(a), Val::Int(b)) => match a.checked_add(b) {
                Some(v) => Val::Int(v),
                None => return Err(crate::Error::AddOverflow(a_r, b_r)),
            },
            _ => Val::Float(a.to_f64() + b.to_f64()),
        };
        ok(val, range)
    }

    fn sub(&mut self, n1: &Ast, n2: &Ast, range: Range) -> crate::Result<Return> {
        let (a, a_r) = self.eval_to_val(n1)?;
        let (b, b_r) = self.eval_to_val(n2)?;

        let val = match (a, b) {
            (Val::Int(a), Val::Int(b)) => match a.checked_sub(b) {
                Some(v) => Val::Int(v),
                None => return Err(crate::Error::SubOverflow(a_r, b_r)),
            },
            _ => Val::Float(a.to_f64() - b.to_f64()),
        };
        ok(val, range)
    }

    fn mul(&mut self, n1: &Ast, n2: &Ast, range: Range) -> crate::Result<Return> {
        let (a, a_r) = self.eval_to_val(n1)?;
        let (b, b_r) = self.eval_to_val(n2)?;

        let val = match (a, b) {
            (Val::Int(a), Val::Int(b)) => match a.checked_mul(b) {
                Some(v) => Val::Int(v),
                None => return Err(crate::Error::MulOverflow(a_r, b_r)),
            },
            _ => Val::Float(a.to_f64() * b.to_f64()),
        };
        ok(val, range)
    }

    fn div(&mut self, n1: &Ast, n2: &Ast, range: Range) -> crate::Result<Return> {
        let (a, a_r) = self.eval_to_val(n1)?;
        let (b, b_r) = self.eval_to_val(n2)?;

        let val = match (a, b) {
            (Val::Int(a), Val::Int(b)) => {
                if b == 0 {
                    return Err(crate::Error::DivideByZero(a_r, b_r));
                } else if a % b == 0 {
                    Val::Int(a / b)
                } else {
                    Val::Float(a as f64 / b as f64)
                }
            }
            _ => {
                let divisor = b.to_f64();
                if divisor == 0.0 {
                    return Err(crate::Error::DivideByZero(a_r, b_r));
                } else {
                    Val::Float(a.to_f64() / divisor)
                }
            }
        };
        ok(val, range)
    }

    fn int_div(&mut self, n1: &Ast, n2: &Ast, range: Range) -> crate::Result<Return> {
        let (a, a_r) = self.eval_to_val(n1)?;
        let (b, b_r) = self.eval_to_val(n2)?;

        let val = match (a, b) {
            (Val::Int(a), Val::Int(b)) => {
                if b == 0 {
                    return Err(crate::Error::DivideByZero(a_r, b_r));
                } else {
                    Val::Int(a / b)
                }
            }
            _ => return Err(crate::Error::FractionEuclidDiv(a_r, b_r)),
        };
        ok(val, range)
    }

    fn rem(&mut self, n1: &Ast, n2: &Ast, range: Range) -> crate::Result<Return> {
        let (a, a_r) = self.eval_to_val(n1)?;
        let (b, b_r) = self.eval_to_val(n2)?;

        let val = match (a, b) {
            (Val::Int(a), Val::Int(b)) => {
                if b == 0 {
                    return Err(crate::Error::RemainderByZero(a_r, b_r));
                } else {
                    let r = a % b;
                    if (r > 0 && b < 0) || (r < 0 && b > 0) {
                        Val::Int(r + b)
                    } else {
                        Val::Int(r)
                    }
                }
            }
            _ => return Err(crate::Error::FractionRemainder(a_r, b_r)),
        };
        ok(val, range)
    }

    fn pow(&mut self, n1: &Ast, n2: &Ast, range: Range) -> crate::Result<Return> {
        let (a, a_r) = self.eval_to_val(n1)?;
        let (b, b_r) = self.eval_to_val(n2)?;

        let val = match (a, b) {
            (Val::Int(base), Val::Int(exp)) => {
                if let Ok(e) = u32::try_from(exp) {
                    Val::Int(base.pow(e))
                } else if let Ok(e) = i32::try_from(exp) {
                    Val::Float((e as f64).powi(e))
                } else {
                    return Err(crate::Error::PowOverflow(a_r, b_r));
                }
            }
            _ => Val::Float(a.to_f64().powf(b.to_f64())),
        };
        ok(val, range)
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
        let (a, a_r) = self.eval_to_val(n1)?;
        let (b, b_r) = self.eval_to_val(n2)?;
        let val = match (a, b) {
            (Val::Int(n), Val::Int(mut r)) => {
                if r < 0 {
                    return Err(crate::Error::NegativeNcr(a_r, b_r));
                }
                if n < r {
                    return Err(crate::Error::InvalidNcr(a_r, b_r));
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
            _ => return Err(crate::Error::FractionNcr(a_r, b_r)),
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
        let (a, a_r) = self.eval_to_val(n1)?;
        let (b, b_r) = self.eval_to_val(n2)?;
        match (a, b) {
            (Val::Int(mut a), Val::Int(mut b)) => {
                let mut _t = 0;
                while b != 0 {
                    _t = b;
                    b = a % b;
                    a = _t;
                }
                ok(Val::Int(a), range)
            }
            _ => Err(crate::Error::FractionGcd(a_r, b_r)),
        }
    }

    fn min(&mut self, args: &[Ast], range: Range) -> crate::Result<Return> {
        let mut min = None;
        for a in args {
            let val = self.eval_to_val(a)?.0.to_f64();
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
            let val = self.eval_to_val(a)?.0.to_f64();
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
        let (num, _) = self.eval_to_val(num)?;
        let (min, min_r) = self.eval_to_val(min)?;
        let (max, max_r) = self.eval_to_val(max)?;

        let val = match (num, min, max) {
            (Val::Int(v), Val::Int(lo), Val::Int(hi)) => {
                if lo > hi {
                    return Err(crate::Error::InvalidClampBounds(min_r, max_r));
                }
                Val::Int(v.clamp(lo, hi))
            }
            _ => {
                let v = num.to_f64();
                let lo = min.to_f64();
                let hi = max.to_f64();
                // floating point weirdness, negated assertion of stdlib
                #[allow(clippy::neg_cmp_op_on_partial_ord)]
                if !(lo <= hi) {
                    return Err(crate::Error::InvalidClampBounds(min_r, max_r));
                }
                Val::Float(v.clamp(lo, hi))
            }
        };
        ok(val, range)
    }

    fn degree(&mut self, n: &Ast, range: Range) -> crate::Result<Return> {
        let (val, _) = self.eval_to_val(n)?;
        let rad = val.to_f64().to_radians();
        ok(Val::Float(rad), range)
    }

    fn factorial(&mut self, n: &Ast, range: Range) -> crate::Result<Return> {
        let (val, r) = self.eval_to_val(n)?;
        let val = match val {
            Val::Int(i) => {
                if i < 0 {
                    return Err(crate::Error::NegativeFactorial(r));
                } else {
                    let mut f: i128 = 1;
                    for i in 1..=i {
                        match f.checked_mul(i) {
                            Some(v) => f = v,
                            None => return Err(crate::Error::FactorialOverflow(r)),
                        }
                    }
                    Val::Int(f)
                }
            }
            _ => return Err(crate::Error::FractionFactorial(r)),
        };
        ok(val, range)
    }

    fn assign(&mut self, id: VarId, n: &Ast, range: Range) -> crate::Result<Return> {
        let (val, _) = self.eval_to_val(n)?;
        self.set_var(id, Some(val));
        Ok(Return::Unit(range))
    }

    fn print(&mut self, args: &[Ast], range: Range) -> crate::Result<Return> {
        let vals = self.eval_to_vals(&args)?;
        if let Some(((first, _), others)) = vals.split_first() {
            print!("{}", first);
            for (v, _) in others {
                print!(" {}", v);
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
}

fn ok(val: Val, range: Range) -> crate::Result<Return> {
    Ok(Return::Val(val, range))
}
