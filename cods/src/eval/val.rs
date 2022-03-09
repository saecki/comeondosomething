use crate::{Context, Ext, Val, Data, ValT, ValResult, Var, VarId};

impl ValT {
    pub fn maybe_int(self) -> Self {
        match self {
            Self::Data(Data::Float(f)) => {
                let i = f as i128;
                #[allow(clippy::float_cmp)]
                if i as f64 == f {
                    Self::Data(Data::Int(i))
                } else {
                    Self::Data(Data::Float(f))
                }
            }
            v => v,
        }
    }
}

impl Data {
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
    pub fn to_f64(&self, val: Val) -> crate::Result<f64> {
        Ok(self.to_data(val)?.to_f64())
    }

    pub fn to_int(&self, val: Val) -> crate::Result<Option<i128>> {
        Ok(self.to_data(val)?.to_int())
    }

    pub fn to_data(&self, val: Val) -> crate::Result<Data> {
        match self.resolve_val(val.typ) {
            ValResult::Resolved(p) => Ok(p),
            ValResult::Undefined(name) => Err(crate::Error::UndefinedVar(name, val.range)),
            ValResult::CircularRef(names) => Err(crate::Error::CircularRef(names, val.range)),
        }
    }

    pub fn resolve_val(&self, val: ValT) -> ValResult {
        let mut ids = Vec::new();
        self.resolve_var(&mut ids, val)
    }

    fn resolve_var(&self, checked_ids: &mut Vec<VarId>, val: ValT) -> ValResult {
        match val {
            ValT::Data(p) => ValResult::Resolved(p),
            ValT::Ext(e) => ValResult::Resolved(self.resolve_ext(e)),
            ValT::Var(id) if checked_ids.contains(&id) => {
                checked_ids.push(id);
                let names = checked_ids
                    .iter()
                    .map(|id| self.var(*id).name.clone())
                    .collect();
                ValResult::CircularRef(names)
            }
            ValT::Var(id) => {
                checked_ids.push(id);
                let var = &self.var(id);
                match var.value {
                    Some(v) => self.resolve_var(checked_ids, v),
                    None => ValResult::Undefined(var.name.clone()),
                }
            }
        }
    }

    pub fn resolve_ext(&self, ext: Ext) -> Data {
        self.providers[ext.provider].plain_val(ext.id)
    }

    pub fn var(&self, id: VarId) -> &Var {
        &self.vars[id.0]
    }

    pub fn var_mut(&mut self, id: VarId) -> &mut Var {
        &mut self.vars[id.0]
    }
}
