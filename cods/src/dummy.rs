use core::fmt;
use std::str::FromStr;

use crate::{Provider, Var};

pub struct DummyProvider;

impl Provider<DummyVar> for DummyProvider {
    fn var_to_f64(&self, _var: DummyVar) -> f64 {
        0.0
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct DummyVar;

impl Var for DummyVar {}

impl FromStr for DummyVar {
    type Err = ();

    fn from_str(_: &str) -> std::result::Result<Self, Self::Err> {
        Err(())
    }
}

impl fmt::Display for DummyVar {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Err(fmt::Error)
    }
}
