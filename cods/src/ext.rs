use std::fmt;

use crate::PlainVal;

/// A provider for external values ([`Ext`]) that are injected into the scope.
pub trait Provider: fmt::Debug {
    fn plain_val(&self, id: ExtId) -> PlainVal;

    fn parse(&self, literal: &str) -> Option<ExtId>;
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ExtId(pub usize);

/// An external value which is injected into the scope by a [`Provider`].
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Ext {
    pub provider: usize,
    pub id: ExtId,
}

impl Ext {
    pub fn new(provider: usize, id: ExtId) -> Self {
        Self { provider, id }
    }
}
