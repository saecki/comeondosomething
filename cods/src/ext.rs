use std::fmt;
use std::str::FromStr;

use crate::{PlainVal, Val};

/// A provider for external values ([`Ext`]) that are injected into the scope.
pub trait Provider<T: Ext> {
    fn ext_to_plain_val(&self, ext: T) -> PlainVal;

    fn to_plain_val(&self, val: Val<T>) -> PlainVal {
        match val {
            Val::Plain(p) => p,
            Val::Ext(e) => self.ext_to_plain_val(e),
        }
    }

    fn to_f64(&self, val: Val<T>) -> f64 {
        self.to_plain_val(val).to_f64()
    }


    /// Convert any value that isn't a fraction to [`Self::Int`];
    fn to_int(&self, val: Val<T>) -> Option<i128> {
        match val {
            Val::Plain(p) => p.to_int(),
            Val::Ext(e) => self.ext_to_plain_val(e).to_int(),
        }
    }
}

/// An external value which is injected into the scope by a [`Provider`].
pub trait Ext: Copy + fmt::Debug + fmt::Display + FromStr {}
