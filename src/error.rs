use crate::{range, Op, Par, Range};

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Clone, Debug, PartialEq)]
pub enum Error {
    MissingOperand(Range),
    MissingOperator(Range),
    MissingClosingParenthesis(Par),
    UnexpectedOperator(Op),
    UnexpectedParenthesis(Par),
    MismatchedParenthesis { opening: Par, found: Par },
    InvalidCharacter { char: char, range: Range },
    NumberFormatException(Range),
}

impl Error {
    pub fn show(&self, _string: &str) -> String {
        let mut out = String::new();
        match self {
            Self::MissingOperand(p) => mark_range(&mut out, *p),
            Self::MissingOperator(p) => mark_range(&mut out, *p),
            Self::MissingClosingParenthesis(p) => mark_range(&mut out, p.range()),
            Self::UnexpectedOperator(o) => mark_range(&mut out, o.range()),
            Self::UnexpectedParenthesis(p) => mark_range(&mut out, p.range()),
            Self::MismatchedParenthesis { opening, found } => {
                mark_range(&mut out, opening.range());
                let start = found.range().start - opening.range().end;
                let end = start + found.range().len();
                mark_range(&mut out, range(start, end));
            }
            Self::InvalidCharacter { char: _, range } => mark_range(&mut out, *range),
            Self::NumberFormatException(r) => mark_range(&mut out, *r),
        }
        out.push('\n');
        out.push_str(self.description());

        out
    }

    fn description(&self) -> &'static str {
        match self {
            Self::MissingOperand(_) => "Missing an operand",
            Self::MissingOperator(_) => "Missing an operator",
            Self::MissingClosingParenthesis(_) => "Missing a closing parenthesis",
            Self::UnexpectedOperator(_) => "Found an unexpected operator",
            Self::UnexpectedParenthesis(_) => "Found an unexpected parenthesis",
            Self::MismatchedParenthesis { .. } => "Parenthesis do not match",
            Self::InvalidCharacter { .. } => "Found an invalid character",
            Self::NumberFormatException { .. } => "Found an invalid number literal",
        }
    }
}

fn mark_range(out: &mut String, range: Range) {
    let Range { start, end } = range;
    out.extend((0..start).map(|_| ' '));
    out.extend((start..end).map(|_| '^'));
}
