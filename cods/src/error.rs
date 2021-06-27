use std::cmp::min;
use std::fmt;
use std::fmt::Write;
use std::marker::PhantomData;

use unicode_width::UnicodeWidthChar;

use crate::{range, Cmd, Group, Mod, Num, Op, Par, Range};

pub type Result<T> = std::result::Result<T, Error>;

pub trait UserFacing<C: Color>: Sized + fmt::Debug {
    fn description(&self) -> String;
    fn ranges(&self) -> Vec<Range>;

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
    MissingClosingParenthesis(Par),
    MissingCommandParenthesis(Range),
    MissingCommandArguments {
        range: Range,
        expected: usize,
        found: usize,
    },
    UnexpectedCommandArguments {
        ranges: Vec<Range>,
        expected: usize,
        found: usize,
    },
    UnexpectedGroup(Group),
    UnexpectedNumber(Num),
    UnexpectedOperator(Op),
    UnexpectedCommand(Cmd),
    UnexpectedModifier(Mod),
    UnexpectedParenthesis(Par),
    InvalidNumberFormat(Range),
    DivideByZero(Num, Num),
    NegativeFactorial(Range),
}

impl UserFacing<Red> for Error {
    fn description(&self) -> String {
        match self {
            Self::Parsing(_) => "A parsing error occured".into(),
            Self::MissingOperand(_) => "Missing an operand".into(),
            Self::MissingOperator(_) => "Missing an operator".into(),
            Self::MissingCommandParenthesis(_) => "Missing a command parenthesis".into(),
            Self::MissingClosingParenthesis(_) => "Missing a matching closing parenthesis".into(),
            Self::MissingCommandArguments {
                expected, found, ..
            } => {
                let missing = expected - found;
                let arg_s = if missing == 1 { "" } else { "s" };
                let are_is = if *expected == 1 { "is" } else { "are" };
                let were_was = if *found == 1 { "was" } else { "were" };
                format!(
                    "Missing {} command argument{}, {} {} required, but only {} {} found",
                    missing, arg_s, expected, are_is, found, were_was,
                )
            }
            Self::UnexpectedCommandArguments {
                expected, found, ..
            } => {
                let over = found - expected;
                let arg_s = if over == 1 { "s" } else { "" };
                let are_is = if *expected == 1 { "is" } else { "are" };
                let were_was = if *found == 1 { "was" } else { "were" };
                format!(
                    "Found {} unexpected command argument{}, only {} {} required, but {} {} found",
                    over, arg_s, expected, are_is, found, were_was,
                )
            }
            Self::UnexpectedGroup(_) => "Found an unexpected group".into(),
            Self::UnexpectedNumber(_) => "Found an unexpected number".into(),
            Self::UnexpectedOperator(_) => "Found an unexpected operator".into(),
            Self::UnexpectedCommand(_) => "Found an unexpected command".into(),
            Self::UnexpectedModifier(_) => "Found an unexpected modifier".into(),
            Self::UnexpectedParenthesis(_) => "Found an unexpected parenthesis".into(),
            Self::InvalidNumberFormat(_) => "Invalid number format".into(),
            Self::DivideByZero(_, _) => "Attempted to divide by 0".into(),
            Self::NegativeFactorial(_) => {
                "Attempted to calculate the factorial of a negative number".into()
            }
        }
    }

    fn ranges(&self) -> Vec<Range> {
        match self {
            Self::Parsing(r) => vec![*r],
            Self::MissingOperand(r) => vec![*r],
            Self::MissingOperator(r) => vec![*r],
            Self::MissingCommandParenthesis(r) => vec![*r],
            Self::MissingClosingParenthesis(p) => vec![p.range()],
            Self::MissingCommandArguments { range: pos, .. } => vec![*pos],
            Self::UnexpectedCommandArguments { ranges, .. } => ranges.clone(),
            Self::UnexpectedGroup(g) => vec![g.range],
            Self::UnexpectedNumber(n) => vec![n.range],
            Self::UnexpectedOperator(o) => vec![o.range()],
            Self::UnexpectedCommand(c) => vec![c.range()],
            Self::UnexpectedModifier(m) => vec![m.range()],
            Self::UnexpectedParenthesis(p) => vec![p.range()],
            Self::InvalidNumberFormat(r) => vec![*r],
            Self::DivideByZero(a, b) => vec![a.range, b.range],
            Self::NegativeFactorial(r) => vec![*r],
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Warning {
    ConfusingCase(Range, &'static str),
    NegationBehindAdd(Range, Range),
    NegationBehindSub(Range, Range),
    NegationBehindMul(Range, Range),
    NegationBehindDiv(Range, Range),
    MismatchedParenthesis(Par, Par),
}

impl UserFacing<Yellow> for Warning {
    fn description(&self) -> String {
        match self {
            Self::ConfusingCase(_, lit) => format!("Confusing casing, consider writing '{}'", lit),
            Self::NegationBehindAdd(_, _) => {
                "Negation directly behind addition, consider making this a subtraction".into()
            }
            Self::NegationBehindSub(_, _) => {
                "Negation directly behind subtraction, consider making this an addition".into()
            }
            Self::NegationBehindMul(_, _) => {
                "Negation directly behind multiplication, consider negating the whole term".into()
            }
            Self::NegationBehindDiv(_, _) => {
                "Negation directly behind division, consider negating the whole term".into()
            }
            Self::MismatchedParenthesis(_, _) => "Parenthesis do not match".into(),
        }
    }

    fn ranges(&self) -> Vec<Range> {
        match self {
            Self::ConfusingCase(r, _) => vec![*r],
            Self::NegationBehindAdd(r1, r2) => vec![*r1, *r2],
            Self::NegationBehindSub(r1, r2) => vec![*r1, *r2],
            Self::NegationBehindMul(r1, r2) => vec![*r1, *r2],
            Self::NegationBehindDiv(r1, r2) => vec![*r1, *r2],
            Self::MismatchedParenthesis(a, b) => vec![a.range(), b.range()],
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
        let ranges = self.error.ranges();
        let lines = range_lines(self.input);

        for (nr, (lr, l)) in lines.iter().enumerate() {
            let intersecting: Vec<_> = ranges
                .iter()
                .filter(|r| r.intersects(lr))
                .map(|r| {
                    let ms = r.start.saturating_sub(lr.start);
                    let me = min(r.end.saturating_sub(lr.start), lr.len());
                    range(ms, me)
                })
                .collect();

            if !intersecting.is_empty() {
                mark_ranges::<C>(f, nr + 1, l, &intersecting)?;
            }
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

fn mark_ranges<C: Color>(
    f: &mut fmt::Formatter<'_>,
    line_nr: usize,
    line: &str,
    ranges: &[Range],
) -> fmt::Result {
    write!(f, "{:02} │ {}\n   │ ", line_nr, line)?;

    let mut chars = line.chars();
    let mut pos = 0;
    let mut peeked = 0;

    for r in ranges {
        let mut offset = 0;
        for _ in pos..(r.start) {
            if let Some(c) = chars.next() {
                pos += 1;
                offset += c.width().unwrap_or(0);
            }
        }
        for _ in 0..peeked {
            if offset > 0 {
                offset -= 1;
                peeked -= 1;
            }
        }

        let mut width = 0;
        for _ in (pos)..(r.end) {
            if let Some(c) = chars.next() {
                pos += 1;
                width += c.width().unwrap_or(0);
            }
        }
        if width == 0 && peeked == 0 {
            width = 1;
        };
        for _ in 0..peeked {
            if width > 1 {
                offset -= 1;
                peeked -= 1;
            }
        }

        for _ in 0..offset {
            f.write_char(' ')?;
        }
        f.write_str(C::ansi_start())?;
        for _ in 0..width {
            f.write_char('^')?;
        }
        f.write_str(C::ansi_end())?;
    }

    f.write_char('\n')?;

    Ok(())
}

fn range_lines(string: &str) -> Vec<(Range, &str)> {
    let mut lines = Vec::new();
    let mut line_start = (0, 0);
    let mut pos = (0, 0);
    let mut pushed_line = false;

    let mut chars = string.chars();
    while let Some(c) = chars.next() {
        match c {
            '\r' => {
                if !pushed_line {
                    let range = range(line_start.0, pos.0);
                    let line = &string[line_start.1..pos.1];
                    lines.push((range, line));
                }
                pushed_line = true;
            }
            '\n' => {
                if !pushed_line {
                    let range = range(line_start.0, pos.0);
                    let line = &string[line_start.1..pos.1];
                    lines.push((range, line));
                }

                // We know these chars are 1 byte wide
                line_start = (pos.0 + 1, pos.1 + 1);
                pushed_line = false;
            }
            _ => pushed_line = false,
        }

        pos.0 += 1;
        pos.1 = string.len() - chars.as_str().len();
    }

    if !pushed_line {
        let range = range(line_start.0, pos.0);
        let line = &string[line_start.1..pos.1];
        lines.push((range, line));
    }

    lines
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

pub struct Green;
impl Color for Green {
    fn ansi_start() -> &'static str {
        "\x1B[;32m"
    }
}

pub struct Yellow;
impl Color for Yellow {
    fn ansi_start() -> &'static str {
        "\x1B[;33m"
    }
}
