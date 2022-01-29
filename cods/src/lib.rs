use std::result;

pub use calc::*;
pub use display::*;
pub use error::*;
pub use group::*;
pub use parse::*;
pub use style::*;
pub use token::*;

pub mod calc;
mod display;
mod error;
mod group;
mod parse;
mod style;
mod token;

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Context {
    errors: Vec<Error>,
    warnings: Vec<Warning>,
}

pub fn calc(string: impl AsRef<str>) -> result::Result<(Val, Vec<Warning>), Vec<Error>> {
    let mut ctx = Context::default();

    let tokens = match ctx.tokenize(string.as_ref()) {
        Err(e) => {
            ctx.errors.push(e);
            return Err(ctx.errors);
        }
        Ok(t) => t,
    };

    let items = match ctx.group(&tokens) {
        Err(e) => {
            ctx.errors.push(e);
            return Err(ctx.errors);
        }
        Ok(i) => i,
    };

    let calculation = match ctx.parse(&items) {
        Err(e) => {
            ctx.errors.push(e);
            return Err(ctx.errors);
        }
        Ok(c) => c,
    };

    if !ctx.errors.is_empty() {
        return Err(ctx.errors);
    }

    match calculation.calc() {
        Err(e) => Err(vec![e]),
        Ok(v) => Ok((v, ctx.warnings)),
    }
}
