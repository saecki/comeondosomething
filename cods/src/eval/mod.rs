use crate::{Ast, Val};

pub use val::*;

#[cfg(test)]
mod test;
mod val;

pub fn eval_all(asts: &[Ast]) -> crate::Result<Val> {
    todo!()
}
