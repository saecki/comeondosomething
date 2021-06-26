use std::cmp::min;
use std::fmt;
use std::fmt::Write;
use std::marker::PhantomData;

use unicode_width::UnicodeWidthChar;

use crate::{range, span, Cmd, Group, Mod, Num, Op, Par, Range};

pub type Result<T> = std::result::Result<T, Error>;

pub trait UserFacing<C: Color>: Sized + fmt::Debug {
    fn description(&self) -> String;
    fn range(&self) -> Range;

    fn display<'a>(&'a self, input: &'a str) -> DisplayUserFacing<'a, Self, C> {
        DisplayUserFacing {
            input,
            error: self,
            c: PhantomData::<C>,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Error {
    Parsing(Range),
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

impl UserFacing<Red> for Error {
    fn description(&self) -> String {
        match self {
            Self::Parsing(_) => format!("A parsing error occured"),
            Self::MissingOperand(_) => format!("Missing an operand"),
            Self::MissingOperator(_) => format!("Missing an operator"),
            Self::MissingCommandParenthesis(_) => format!("Missing a command parenthesis"),
            Self::MissingClosingParenthesis(_) => format!("Missing a matching closing parenthesis"),
            Self::UnexpectedGroup(_) => format!("Found an unexpected group"),
            Self::UnexpectedNumber(_) => format!("Found an unexpected number"),
            Self::UnexpectedOperator(_) => format!("Found an unexpected operator"),
            Self::UnexpectedCommand(_) => format!("Found an unexpected command"),
            Self::UnexpectedModifier(_) => format!("Found an unexpected modifier"),
            Self::UnexpectedParenthesis(_) => format!("Found an unexpected parenthesis"),
            Self::MismatchedParenthesis(_, _) => format!("Parenthesis do not match"),
            Self::InvalidNumberFormat(_) => format!("Invalid number format"),
            Self::DivideByZero(_, _) => format!("Attempted to divide by 0"),
            Self::NegativeFactorial(_) => {
                format!("Attempted to calculate the factorial of a negative number")
            }
        }
    }

    fn range(&self) -> Range {
        match self {
            Self::Parsing(r) => *r,
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

#[derive(Clone, Debug, PartialEq)]
pub enum Warning {
    ConfusingCase(&'static str, Range),
}

impl UserFacing<Yellow> for Warning {
    fn description(&self) -> String {
        match self {
            Self::ConfusingCase(lit, _) => format!("Confusing casing, consider writing '{}'", lit),
        }
    }

    fn range(&self) -> Range {
        match self {
            Self::ConfusingCase(_, r) => *r,
        }
    }
}

pub struct DisplayUserFacing<'a, U: UserFacing<C>, C: Color> {
    input: &'a str,
    error: &'a U,
    c: PhantomData<C>,
}

impl<U: UserFacing<C>, C: Color> fmt::Display for DisplayUserFacing<'_, U, C> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Range { start, end } = self.error.range();

        println!("{:?}", self.error);

        let mut i = 0;
        for (nr, l) in self.input.lines().enumerate() {
            let count = l.chars().count();

            if start <= i + count && end >= i {
                let ms = start.saturating_sub(i);
                let me = min(end.checked_sub(i).unwrap_or(count), count);
                mark_range::<C>(f, nr + 1, l, range(ms, me))?;
            }

            i += count + 1;
        }

        write!(
            f,
            "     {}{}{}",
            C::ansi_start(),
            self.error.description(),
            C::ansi_end()
        )?;
        Ok(())
    }
}

fn mark_range<C: Color>(
    f: &mut fmt::Formatter<'_>,
    line_nr: usize,
    line: &str,
    range: Range,
) -> fmt::Result {
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
        f.write_str(C::ansi_start())?;
        f.write_char('^')?;
        f.write_str(C::ansi_end())?;
    }
    f.write_char('\n')?;

    Ok(())
}

pub trait Color: Sized {
    fn ansi_start() -> &'static str;
    fn ansi_end() -> &'static str {
        "\x1B[0m"
    }
}

pub struct Red;
impl Color for Red {
    fn ansi_start() -> &'static str {
        "\x1B[;31m"
    }
}

pub struct Yellow;
impl Color for Yellow {
    fn ansi_start() -> &'static str {
        "\x1B[;33m"
    }
}
