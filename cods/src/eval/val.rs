use crate::{Context, Expr, ExprT, Val, Var, VarId};

impl Val {
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
        }
    }

    pub fn to_f64(&self) -> f64 {
        match self {
            Self::Int(i) => *i as f64,
            Self::Float(f) => *f,
        }
    }
}

impl Context {
    pub fn to_f64(&self, expr: &Expr) -> crate::Result<f64> {
        Ok(self.to_val(expr)?.to_f64())
    }

    pub fn to_int(&self, expr: &Expr) -> crate::Result<Option<i128>> {
        Ok(self.to_val(expr)?.to_int())
    }

    pub fn to_val(&self, expr: &Expr) -> crate::Result<Val> {
        match self.resolve_val(expr.typ) {
            Ok(d) => Ok(d),
            Err(name) => Err(crate::Error::UndefinedVar(name, expr.range)),
        }
    }

    fn resolve_val(&self, expr: ExprT) -> Result<Val, String> {
        match expr {
            ExprT::Val(p) => Ok(p),
            ExprT::Var(id) => {
                let var = self.var(id);
                match var.value {
                    Some(d) => Ok(d),
                    None => Err(var.name.clone()),
                }
            }
        }
    }

    pub fn var(&self, id: VarId) -> &Var {
        &self.vars[id.0]
    }

    pub fn set_var(&mut self, id: VarId, val: Option<Val>) {
        // TODO const vars
        self.vars[id.0].value = val;
    }
}
