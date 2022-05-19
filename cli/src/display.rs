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

struct HlLine<'a> {
    nr: u32,
    text: &'a str,
    spans: Vec<(u32, u32)>,
}

impl<'a> HlLine<'a> {
    fn new(nr: u32, text: &'a str, spans: Vec<(u32, u32)>) -> Self {
        Self { nr, text, spans }
    }
}

impl<U: DisplayUserFacing<C>, C: Color> Display for FmtUserFacing<'_, U, C> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let spans = self.error.spans();
        let mut visible_lines = Vec::new();
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
                visible_lines.push(HlLine::new(nr + 1, l, intersecting));
            }
        }

        {
            let mut i = 0;
            while i + 1 < visible_lines.len() {
                let a = &visible_lines[i];
                let b = &visible_lines[i + 1];

                let gap = b.nr - a.nr - 1;
                if gap <= 10 {
                    for (nr, l) in self
                        .input
                        .lines()
                        .enumerate()
                        .skip(a.nr as usize)
                        .take(gap as usize)
                    {
                        let hl_line = HlLine::new(nr as u32 + 1, l, Vec::new());
                        visible_lines.insert(i + 1, hl_line);
                        i += 1;
                    }
                }
                i += 1;
            }
        }

        let nr_width = visible_lines
            .last()
            .map_or(2, |hl_line| (hl_line.nr as f32).log10() as usize + 1);

        if let Some((first, others)) = visible_lines.split_first() {
            let hl_line = first;
            print_line(f, hl_line.nr, nr_width, hl_line.text)?;
            if !hl_line.spans.is_empty() {
                mark_spans::<C>(f, nr_width, hl_line.text, &hl_line.spans)?;
            }

            let mut last_nr = hl_line.nr;
            for hl_line in others {
                if last_nr + 1 < hl_line.nr {
                    writeln!(f, "{blue}...{esc}", blue = LBlue::bold(), esc = ANSI_ESC)?;
                }
                print_line(f, hl_line.nr, nr_width, hl_line.text)?;
                if !hl_line.spans.is_empty() {
                    mark_spans::<C>(f, nr_width, hl_line.text, &hl_line.spans)?;
                }
                last_nr = hl_line.nr;
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

fn print_line(
    f: &mut fmt::Formatter<'_>,
    line_nr: u32,
    nr_width: usize,
    line: &str,
) -> fmt::Result {
    writeln!(
        f,
        "{blue}{nr:nr_w$} │{esc} {ln}",
        nr = line_nr,
        nr_w = nr_width,
        ln = line,
        blue = LBlue::bold(),
        esc = ANSI_ESC,
    )
}

fn mark_spans<C: Color>(
    f: &mut fmt::Formatter<'_>,
    nr_width: usize,
    line: &str,
    spans: &[(u32, u32)],
) -> fmt::Result {
    write!(
        f,
        "{spc:nr_w$} {blue}│{esc} ",
        nr_w = nr_width,
        spc = ' ',
        blue = LBlue::bold(),
        esc = ANSI_ESC,
    )?;

    let mut inside = 0;
    for (i, c) in line.chars().enumerate() {
        let i = i as u32;
        for &(start, end) in spans {
            if start == i {
                inside += 1;
            }
            if end == i {
                inside -= 1;
            }
        }

        let width = c.width().unwrap_or(0);
        if inside == 0 {
            for _ in 0..width {
                f.write_char(' ')?;
            }
        } else {
            write!(f, "{}", C::bold())?;
            for _ in 0..width {
                f.write_char('^')?;
            }
            f.write_str(ANSI_ESC)?;
        }
    }

    f.write_char('\n')?;

    Ok(())
}
