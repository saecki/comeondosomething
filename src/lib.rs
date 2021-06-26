pub use calc::*;
pub use error::*;
pub use lex::*;
pub use parse::*;
pub use token::*;

mod calc;
mod error;
mod lex;
mod parse;
mod token;

pub fn calc(string: impl AsRef<str>) -> crate::Result<Val> {
    let tokens = tokenize(string.as_ref())?;
    let items = lex(&tokens)?;
    let calculation = parse(&items)?;
    calculation.calc()
}
