use std::fmt;

use crate::PlainVal;

/// A provider for external values ([`Ext`]) that are injected into the scope.
pub trait Provider<T: Ext> {
    fn plain_val(&self, ext: T) -> PlainVal;

    fn parse(&self, literal: &str) -> Option<T>;
}

/// An external value which is injected into the scope by a [`Provider`].
pub trait Ext: Copy + fmt::Debug {}
