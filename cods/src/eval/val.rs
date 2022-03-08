use std::f64::consts;

use crate::{Context, Ext, Num, PlainVal, Provider, Val, ValResult, Var, VarId};

impl<T: Ext> Val<T> {
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

impl<T: Ext, P: Provider<T>> Context<T, P> {
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
                    .map(|id| self.var(*id).name.clone())
                    .collect();
                ValResult::CircularRef(names)
            }
            Val::Var(id) => {
                checked_ids.push(id);
                let var = &self.var(id);
                match var.value {
                    Some(v) => self.resolve_var(checked_ids, v),
                    None => ValResult::Undefined(var.name.clone()),
                }
            }
        }
    }

    pub fn var(&self, id: VarId) -> &Var<T> {
        &self.vars[id.0]
    }

    pub fn var_mut(&mut self, id: VarId) -> &mut Var<T> {
        &mut self.vars[id.0]
    }
}
