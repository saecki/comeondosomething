use std::{
    cmp::min,
    fmt::{self, Write},
};

use unicode_width::UnicodeWidthChar;

use crate::{range, span, Cmd, Group, Mod, Num, Op, Par, Range};

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Clone, Debug, PartialEq)]
pub enum Error {
    MissingOperand(Range),
    MissingOperator(Range),
    MissingCommandParenthesis(Range),
    MissingClosingParenthesis(Par),
    UnexpectedGroup(Group),
    UnexpectedNumber(Num),
    UnexpectedOperator(Op),
    UnexpectedCommand(Cmd),
    UnexpectedModifier(Mod),
    UnexpectedParenthesis(Par),
    MismatchedParenthesis(Par, Par),
    InvalidNumberFormat(Range),
    DivideByZero(Num, Num),
    NegativeFactorial(Range),
}

impl Error {
    pub fn display<'a>(&'a self, input: &'a str) -> DisplayError<'a> {
        DisplayError { input, error: self }
    }

    pub fn description(&self) -> &'static str {
        match self {
            Self::MissingOperand(_) => "Missing an operand",
            Self::MissingOperator(_) => "Missing an operator",
            Self::MissingCommandParenthesis(_) => "Missing a command parenthesis",
            Self::MissingClosingParenthesis(_) => "Missing a matching closing parenthesis",
            Self::UnexpectedGroup(_) => "Found an unexpected group",
            Self::UnexpectedNumber(_) => "Found an unexpected number",
            Self::UnexpectedOperator(_) => "Found an unexpected operator",
            Self::UnexpectedCommand(_) => "Found an unexpected command",
            Self::UnexpectedModifier(_) => "Found an unexpected modifier",
            Self::UnexpectedParenthesis(_) => "Found an unexpected parenthesis",
            Self::MismatchedParenthesis(_, _) => "Parenthesis do not match",
            Self::InvalidNumberFormat(_) => "Invalid number format",
            Self::DivideByZero(_, _) => "Attempted to divide by 0",
            Self::NegativeFactorial(_) => {
                "Attempted to calculate the factorial of a negative number"
            }
        }
    }

    pub fn range(&self) -> Range {
        match self {
            Self::MissingOperand(r) => *r,
            Self::MissingOperator(r) => *r,
            Self::MissingCommandParenthesis(r) => *r,
            Self::MissingClosingParenthesis(p) => p.range(),
            Self::UnexpectedGroup(g) => g.range,
            Self::UnexpectedNumber(n) => n.range,
            Self::UnexpectedOperator(o) => o.range(),
            Self::UnexpectedCommand(c) => c.range(),
            Self::UnexpectedModifier(m) => m.range(),
            Self::UnexpectedParenthesis(p) => p.range(),
            Self::MismatchedParenthesis(a, b) => span(a.range(), b.range()), // TODO mark two separate ranges
            Self::InvalidNumberFormat(r) => *r,
            Self::DivideByZero(_a, b) => b.range, // TODO mark two separate ranges
            Self::NegativeFactorial(r) => *r,
        }
    }
}

pub struct DisplayError<'a> {
    input: &'a str,
    error: &'a Error,
}

impl fmt::Display for DisplayError<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Range { start, end } = self.error.range();

        let mut i = 0;
        for (nr, l) in self.input.lines().enumerate() {
            let count = l.chars().count();

            if start <= i + count && end >= i {
                let ms = start.saturating_sub(i);
                let me = min(end.checked_sub(i).unwrap_or(count), count);
                mark_range(f, nr + 1, l, range(ms, me))?;
            }

            i += count + 1;
        }

        write!(f, "     \x1B[;31m{}\x1B[0m", self.error.description())?;
        Ok(())
    }
}

fn mark_range(f: &mut fmt::Formatter<'_>, line_nr: usize, line: &str, range: Range) -> fmt::Result {
    let offset: usize = line
        .chars()
        .take(range.start)
        .filter_map(|c| c.width())
        .sum();

    let mut width: usize = line
        .chars()
        .skip(range.start)
        .take(range.len())
        .filter_map(|c| c.width())
        .sum();
    if width == 0 {
        width = 1;
    };

    write!(f, "{:02} │ {}\n   │ ", line_nr, line)?;
    for _ in 0..offset {
        f.write_char(' ')?;
    }
    for _ in 0..width {
        f.write_str("\x1B[;31m")?;
        f.write_char('^')?;
        f.write_str("\x1B[0m")?;
    }
    f.write_char('\n')?;

    Ok(())
}
