use crate::{Span, Context};

use super::Lexer;

impl Context {
    pub(super) fn escape_char(&mut self, lexer: &mut Lexer<'_>) -> Result<char, EscError> {
        let esc_start = lexer.pos();
        let c = match lexer.next() {
            Some(c) => c,
            None => {
                let s = Span::pos(lexer.pos());
                return Err(EscError {
                    error: crate::Error::MissingEscapeChar(s),
                    end_str: false,
                    fail: true,
                });
            }
        };

        let escaped = match c {
            'x' => unicode_escape_char(lexer, 2, esc_start)?,
            'u' => unicode_escape_char(lexer, 4, esc_start)?,
            _ => match c {
                '0' => '\0',
                'b' => '\u{8}',
                't' => '\t',
                'n' => '\n',
                'r' => '\r',
                '"' => '"',
                '\\' => '\\',
                _ => {
                    return Err(EscError {
                        error: crate::Error::InvalidEscapeChar(c, Span::pos(lexer.pos())),
                        end_str: false,
                        fail: false,
                    })
                }
            },
        };

        Ok(escaped)
    }
}

pub struct EscError {
    pub error: crate::Error,
    pub end_str: bool,
    pub fail: bool,
}

fn unicode_escape_char(
    lexer: &mut Lexer<'_>,
    expected: usize,
    esc_start: usize,
) -> Result<char, EscError> {
    if let Some('{') = lexer.peek() {
        lexer.next();
        return braced_unicode_escape_char(lexer, esc_start);
    }

    let mut cp = 0;
    let mut i = 0;
    while let Some(c) = lexer.next() {
        if c == ' ' || c == '"' {
            let span = Span::pos(lexer.pos());
            let end_str = c == '"';
            return Err(EscError {
                error: crate::Error::MissingUnicodeEscapeChar {
                    expected,
                    found: i,
                    span,
                },
                end_str,
                fail: false,
            });
        }

        let digit = unicode_escape_hex(lexer, c)?;

        cp <<= 4;
        cp += digit;

        i += 1;

        if i == expected {
            break;
        }
    }

    parse_unicode_cp(lexer, cp, esc_start)
}

fn braced_unicode_escape_char(lexer: &mut Lexer<'_>, esc_start: usize) -> Result<char, EscError> {
    let mut cp = 0;
    let mut i = 0;
    while let Some(c) = lexer.next() {
        if c == '}' {
            break;
        }

        if c == ' ' || c == '"' {
            let a_s = Span::pos(lexer.pos());
            let b_s = Span::pos(lexer.pos() - (i + 1));
            let end_str = c == '"';
            return Err(EscError {
                error: crate::Error::MissingClosingUnicodeEscapePar(b_s, a_s),
                end_str,
                fail: false,
            });
        }

        let digit = unicode_escape_hex(lexer, c)?;

        cp <<= 4;
        cp += digit;

        i += 1;
    }

    if i > 6 {
        let s = Span::of(esc_start, lexer.pos() + 1);
        return Err(EscError {
            error: crate::Error::OverlongUnicodeEscape(s),
            end_str: false,
            fail: true,
        });
    }

    parse_unicode_cp(lexer, cp, esc_start)
}

fn unicode_escape_hex(lexer: &Lexer<'_>, c: char) -> Result<u32, EscError> {
    match c {
        '0'..='9' => Ok(c as u32 - '0' as u32),
        'a'..='f' => Ok(c as u32 - 'a' as u32 + 10),
        'A'..='F' => Ok(c as u32 - 'A' as u32 + 10),
        _ => {
            let s = Span::pos(lexer.pos());
            Err(EscError {
                error: crate::Error::InvalidUnicodeEscapeChar(c, s),
                end_str: false,
                fail: false,
            })
        }
    }
}

fn parse_unicode_cp(lexer: &mut Lexer<'_>, cp: u32, esc_start: usize) -> Result<char, EscError> {
    match char::from_u32(cp) {
        Some(char) => Ok(char),
        None => {
            let s = Span::of(esc_start, lexer.pos() + 1);
            Err(EscError {
                error: crate::Error::InvalidUnicodeScalar(cp, s),
                end_str: false,
                fail: true,
            })
        }
    }
}
