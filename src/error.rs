use std::{
    cmp::{max, min},
    fmt::{self, Write},
};

use unicode_width::UnicodeWidthChar;

use crate::{range, Cmd, Group, Mod, Num, Op, Par, Range};

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
    MismatchedParenthesis { opening: Par, closing: Par },
    InvalidCharacter { char: char, range: Range },
    InvalidNumberFormat(Range),
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
            Self::MissingClosingParenthesis(_) => "Missing a closing parenthesis",
            Self::UnexpectedGroup(_) => "Found an unexpected group",
            Self::UnexpectedNumber(_) => "Found an unexpected number",
            Self::UnexpectedOperator(_) => "Found an unexpected operator",
            Self::UnexpectedCommand(_) => "Found an unexpected command",
            Self::UnexpectedModifier(_) => "Found an unexpected modifier",
            Self::UnexpectedParenthesis(_) => "Found an unexpected parenthesis",
            Self::MismatchedParenthesis { .. } => "Parenthesis do not match",
            Self::InvalidCharacter { .. } => "Found an invalid character",
            Self::InvalidNumberFormat(_) => "Invalid number format",
        }
    }

    pub fn range(&self) -> Range {
        match self {
            Error::MissingOperand(p) => *p,
            Error::MissingOperator(p) => *p,
            Error::MissingCommandParenthesis(r) => *r,
            Error::MissingClosingParenthesis(p) => p.range(),
            Error::UnexpectedGroup(g) => g.range,
            Error::UnexpectedNumber(n) => n.range,
            Error::UnexpectedOperator(o) => o.range(),
            Error::UnexpectedCommand(c) => c.range(),
            Error::UnexpectedModifier(m) => m.range(),
            Error::UnexpectedParenthesis(p) => p.range(),
            Error::MismatchedParenthesis { opening, closing } => {
                range(opening.range().start, closing.range().end)
            }
            Error::InvalidCharacter { char: _, range } => *range,
            Error::InvalidNumberFormat(r) => *r,
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
                let ms = max(diff_or(start, i, 0), 0);
                let me = min(diff_or(end, i, count), count);
                mark_range(f, nr + 1, l, range(ms, me))?;
            }

            i += count + 1;
        }

        write!(f, "     \x1B[;31m{}\x1B[0m", self.error.description())?;
        Ok(())
    }
}

const fn diff_or(a: usize, b: usize, c: usize) -> usize {
    if a >= b {
        a - b
    } else {
        c
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
