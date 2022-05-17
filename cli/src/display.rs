use std::fmt::Write;
use std::fmt::{self, Display};
use std::marker::PhantomData;

use cods::UserFacing;
use unicode_width::UnicodeWidthChar;

use crate::style::{LRed, LYellow};
use crate::{Color, LBlue, ANSI_ESC};

impl DisplayUserFacing<LRed> for cods::Error {}
impl DisplayUserFacing<LYellow> for cods::Warning {}
pub trait DisplayUserFacing<C: Color>: UserFacing {
    fn display<'a>(&'a self, input: &'a str) -> FmtUserFacing<'a, Self, C> {
        FmtUserFacing {
            input,
            error: self,
            c: PhantomData::<C>,
        }
    }
}

pub struct FmtUserFacing<'a, U: DisplayUserFacing<C>, C: Color> {
    input: &'a str,
    error: &'a U,
    c: PhantomData<C>,
}

impl<U: DisplayUserFacing<C>, C: Color> Display for FmtUserFacing<'_, U, C> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let spans = self.error.spans();
        let mut hl_lines = Vec::new();
        for (nr, l) in self.input.lines().enumerate() {
            let nr = nr as u32;
            let intersecting: Vec<_> = spans
                .iter()
                .filter(|s| s.start.line <= nr && s.end.line >= nr)
                .map(|s| {
                    let hl_start = if s.start.line == nr { s.start.col } else { 0 };
                    let hl_end = if s.end.line == nr {
                        s.end.col
                    } else {
                        l.chars().count() as u32
                    };
                    (hl_start, hl_end)
                })
                .collect();

            if !intersecting.is_empty() {
                hl_lines.push((nr + 1, l, intersecting));
            }
        }

        let nr_width = hl_lines
            .last()
            .map_or(2, |(nr, _, _)| (*nr as f32).log10() as usize + 1);

        if let Some((first, others)) = hl_lines.split_first() {
            let (nr, l, spans) = first;
            mark_spans::<C>(f, *nr, nr_width, l, spans)?;

            let mut last_nr = *nr;
            for (nr, l, spans) in others {
                if last_nr + 1 < *nr {
                    writeln!(f, "{blue}...{esc}", blue = LBlue::bold(), esc = ANSI_ESC)?;
                }
                mark_spans::<C>(f, *nr, nr_width, l, spans)?;
                last_nr = *nr;
            }
        }

        let prefix = format!(
            "{spc:nr_width$} {blue}│{esc} {col}",
            spc = ' ',
            col = C::bold(),
            blue = LBlue::bold(),
            esc = ANSI_ESC,
        );
        self.error.description(f, &prefix, ANSI_ESC)?;
        f.write_str(ANSI_ESC)?;
        Ok(())
    }
}

#[allow(clippy::mut_range_bound)]
fn mark_spans<C: Color>(
    f: &mut fmt::Formatter<'_>,
    line_nr: u32,
    nr_width: usize,
    line: &str,
    spans: &[(u32, u32)],
) -> fmt::Result {
    write!(
        f,
        "{blue}{nr:nr_w$} │{esc} {ln}\n{spc:nr_w$} {blue}│{esc} ",
        nr = line_nr,
        nr_w = nr_width,
        ln = line,
        spc = ' ',
        blue = LBlue::bold(),
        esc = ANSI_ESC,
    )?;

    let mut chars = line.chars();
    let mut pos = 0;
    let mut peeked = 0;

    for &(start, end) in spans {
        let mut offset = 0;
        for _ in pos..start {
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
        for _ in pos..end {
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
        write!(f, "{}", C::bold())?;
        for _ in 0..width {
            f.write_char('^')?;
        }
        f.write_str(ANSI_ESC)?;
    }

    f.write_char('\n')?;

    Ok(())
}
