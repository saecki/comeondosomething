use core::fmt;
use std::str::FromStr;

use crate::{Ext, PlainVal, Provider};

pub struct DummyProvider;

impl Provider<ExtDummy> for DummyProvider {
    fn plain_val(&self, _ext: ExtDummy) -> PlainVal {
        unreachable!()
    }
    
    fn parse(&self, _literal: &str) -> Option<ExtDummy> {
        None
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
