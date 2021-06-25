pub use error::*;
pub use lex::*;
pub use parse::*;
pub use token::*;

mod error;
mod lex;
mod parse;
mod token;

pub fn calc(string: impl AsRef<str>) -> crate::Result<f64> {
    let tokens = tokenize(string.as_ref())?;
    let items = lex(&tokens)?;
    let calculation = parse(&items)?;
    Ok(calculation.calc())
}
