use core::fmt;
use std::str::FromStr;

use crate::{Ext, PlainVal, Provider};

pub struct DummyProvider;

impl Provider<ExtDummy> for DummyProvider {
    fn ext_to_plain_val(&self, _ext: ExtDummy) -> PlainVal {
        unreachable!()
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
