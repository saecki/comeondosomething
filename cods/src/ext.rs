use std::f64::consts;
use std::fmt;
use std::str::FromStr;

use crate::Val;

/// A provider for external values ([`Ext`]) that are injected into the scope.
pub trait Provider<T: Ext> {
    fn ext_to_f64(&self, ext: T) -> f64;

    fn val_to_f64(&self, val: Val<T>) -> f64 {
        match val {
            Val::Int(i) => i as f64,
            Val::Float(f) => f,
            Val::Ext(v) => self.ext_to_f64(v),
            Val::TAU => consts::TAU,
            Val::PI => consts::PI,
            Val::E => consts::E,
        }
    }
}

/// An external value which is injected into the scope by a [`Provider`].
pub trait Ext: Copy + fmt::Debug + fmt::Display + FromStr {}

