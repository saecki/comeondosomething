use core::fmt;
use std::str::FromStr;

use crate::{Ext, Provider};

pub struct DummyProvider;

impl Provider<ExtDummy> for DummyProvider {
    fn ext_to_f64(&self, _ext: ExtDummy) -> f64 {
        0.0
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct ExtDummy;

impl Ext for ExtDummy {}

impl FromStr for ExtDummy {
    type Err = ();

    fn from_str(_: &str) -> std::result::Result<Self, Self::Err> {
        Err(())
    }
}

impl fmt::Display for ExtDummy {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Err(fmt::Error)
    }
}
