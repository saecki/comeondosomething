use crate::{Context, Data, Ext, Val, ValT, Var, VarId};

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
            Ok(d) => Ok(d),
            Err(name) => Err(crate::Error::UndefinedVar(name, val.range)),
        }
    }

    pub fn resolve_val(&self, val: ValT) -> Result<Data, String> {
        match val {
            ValT::Data(p) => Ok(p),
            ValT::Ext(e) => Ok(self.resolve_ext(e)),
            ValT::Var(id) => {
                let var = self.var(id);
                match var.value {
                    Some(d) => Ok(d),
                    None => Err(var.name.clone()),
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
