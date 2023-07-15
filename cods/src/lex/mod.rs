use std::iter::Peekable;
use std::str::Chars;

use crate::{Context, Pos, Span};

pub use token::*;

mod str;
#[cfg(test)]
mod test;
mod token;

struct Lexer<'a> {
    input: &'a str,
    chars: Peekable<Chars<'a>>,
    char_cursor: Pos,
    prev_byte_cursor: usize,
    byte_cursor: usize,
    literal: Option<LiteralState>,
    str_literal: String,
    tokens: Vec<Token>,
}

struct LiteralState {
    start: usize,
    end: usize,
}

impl<'a> Lexer<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            input,
            chars: input.chars().peekable(),
            char_cursor: Pos::new(0, 0),
            prev_byte_cursor: 0,
            byte_cursor: 0,
            literal: None,
            str_literal: String::new(),
            tokens: Vec::new(),
        }
    }

    fn next(&mut self) -> Option<char> {
        self.char_cursor.col += 1;
        self.prev_byte_cursor = self.byte_cursor;
        match self.chars.next() {
            Some(c) => {
                self.byte_cursor += c.len_utf8();
                Some(c)
            }
            None => None,
        }
    }

    fn peek(&mut self) -> Option<char> {
        self.chars.peek().copied()
    }

    fn next_if(&mut self, expected: char) -> Option<char> {
        if let Some(c) = self.peek() {
            if c == expected {
                return self.next();
            }
        }

        None
    }

    fn next_if_not(&mut self, expected: char) -> Option<char> {
        if let Some(c) = self.peek() {
            if c != expected {
                return self.next();
            }
        }

        None
    }

    fn new_line(&mut self) {
        self.char_cursor.line += 1;
        self.char_cursor.col = 0;
    }

    const fn pos(&self) -> Pos {
        Pos::new(
            self.char_cursor.line,
            self.char_cursor.col.saturating_sub(1),
        )
    }

    const fn end_pos(&self) -> Pos {
        self.char_cursor
    }

    /// If this is `Some` it is guaranteed to contain at least on character.
    fn literal(&self) -> Option<&str> {
        self.literal.as_ref().map(|l| &self.input[l.start..l.end])
    }

    fn continue_literal(&mut self) {
        match &mut self.literal {
            None => {
                self.literal = Some(LiteralState {
                    start: self.prev_byte_cursor,
                    end: self.byte_cursor,
                });
            }
            Some(l) => {
                l.end = self.byte_cursor;
            }
        }
    }

    fn end_literal(&mut self) {
        self.literal = None;
    }
}

impl Context {
    pub fn lex(&mut self, string: &str) -> crate::Result<Vec<Token>> {
        let mut lexer = Lexer::new(string);

        while let Some(c) = lexer.next() {
            let span = Span::from(lexer.pos());
            match c {
                '"' => self.string_literal(&mut lexer)?,
                '\'' => self.char_literal(&mut lexer)?,
                ' ' | '\t' | '\r' => self.end_literal(&mut lexer)?,
                '\n' => {
                    self.new_atom(&mut lexer, Token::pct(PctT::Newln, span))?;
                    lexer.new_line();
                }
                '+' => {
                    if let Some(lit) = lexer.literal() {
                        if lit.ends_with('e') && lit.chars().next().unwrap().is_ascii_digit() {
                            // This is part of a float literal
                            lexer.continue_literal();
                            continue;
                        }
                    }

                    self.two_char_op(&mut lexer, OpT::Add, OpT::AddAssign, '=')?
                }
                '-' => {
                    if let Some(lit) = lexer.literal() {
                        if lit.ends_with('e') && lit.chars().next().unwrap().is_ascii_digit() {
                            // This is part of a float literal
                            lexer.continue_literal();
                            continue;
                        }
                    }

                    match lexer.peek() {
                        Some('=') => {
                            lexer.next();
                            let s = Span::new(span.start, lexer.end_pos());
                            self.new_atom(&mut lexer, Token::op(OpT::SubAssign, s))?;
                        }
                        Some('>') => {
                            lexer.next();
                            let s = Span::new(span.start, lexer.end_pos());
                            self.new_atom(&mut lexer, Token::pct(PctT::Arrow, s))?;
                        }
                        _ => {
                            self.new_atom(&mut lexer, Token::op(OpT::Sub, span))?;
                        }
                    }
                }
                '*' => match lexer.peek() {
                    Some('=') => {
                        lexer.next();
                        let s = Span::new(span.start, lexer.end_pos());
                        self.new_atom(&mut lexer, Token::op(OpT::MulAssign, s))?;
                    }
                    Some('*') => {
                        lexer.next();
                        let s = Span::new(span.start, lexer.end_pos());
                        self.new_atom(&mut lexer, Token::op(OpT::Pow, s))?;
                    }
                    _ => {
                        self.new_atom(&mut lexer, Token::op(OpT::Mul, span))?;
                    }
                },
                '/' => match lexer.peek() {
                    Some('=') => {
                        lexer.next();
                        let s = Span::new(span.start, lexer.end_pos());
                        self.new_atom(&mut lexer, Token::op(OpT::DivAssign, s))?;
                    }
                    Some('/') => {
                        lexer.next();
                        self.line_comment(&mut lexer)?;
                    }
                    Some('*') => {
                        lexer.next();
                        self.block_comment(&mut lexer)?;
                    }
                    _ => {
                        self.new_atom(&mut lexer, Token::op(OpT::Div, span))?;
                    }
                },
                '%' => self.two_char_op(&mut lexer, OpT::Rem, OpT::RemAssign, '=')?,
                '=' => match lexer.peek() {
                    Some('=') => {
                        lexer.next();
                        let s = Span::new(span.start, lexer.end_pos());
                        self.new_atom(&mut lexer, Token::op(OpT::Eq, s))?;
                    }
                    Some('>') => {
                        lexer.next();
                        let s = Span::new(span.start, lexer.end_pos());
                        self.new_atom(&mut lexer, Token::pct(PctT::FatArrow, s))?;
                    }
                    _ => {
                        self.new_atom(&mut lexer, Token::op(OpT::Assign, span))?;
                    }
                },
                '.' => match lexer.peek() {
                    Some('.') => {
                        lexer.next();
                        let op = match lexer.next_if('=') {
                            Some(_) => OpT::RangeIn,
                            None => OpT::RangeEx,
                        };
                        let s = Span::new(span.start, lexer.end_pos());
                        self.new_atom(&mut lexer, Token::op(op, s))?;
                    }
                    Some(c)
                        if lexer
                            .literal()
                            .map(|l| l.chars().all(|c| c.is_ascii_digit()) && c.is_ascii_digit())
                            .unwrap_or(false) =>
                    {
                        lexer.continue_literal();
                    }
                    _ => self.new_atom(&mut lexer, Token::op(OpT::Dot, span))?,
                },
                '<' => match lexer.peek() {
                    Some('<') => {
                        lexer.next();
                        let op = match lexer.next_if('=') {
                            Some(_) => OpT::ShlAssign,
                            None => OpT::Shl,
                        };
                        let s = Span::new(span.start, lexer.end_pos());
                        self.new_atom(&mut lexer, Token::op(op, s))?;
                    }
                    Some('=') => {
                        lexer.next();
                        let s = Span::new(span.start, lexer.end_pos());
                        self.new_atom(&mut lexer, Token::op(OpT::Le, s))?;
                    }
                    _ => {
                        self.new_atom(&mut lexer, Token::op(OpT::Lt, span))?;
                    }
                },
                '>' => match lexer.peek() {
                    Some('>') => {
                        lexer.next();
                        let op = match lexer.next_if('=') {
                            Some(_) => OpT::ShrAssign,
                            None => OpT::Shr,
                        };
                        let s = Span::new(span.start, lexer.end_pos());
                        self.new_atom(&mut lexer, Token::op(op, s))?;
                    }
                    Some('=') => {
                        lexer.next();
                        let s = Span::new(span.start, lexer.end_pos());
                        self.new_atom(&mut lexer, Token::op(OpT::Ge, s))?;
                    }
                    _ => {
                        self.new_atom(&mut lexer, Token::op(OpT::Gt, span))?;
                    }
                },
                '|' => match lexer.peek() {
                    Some('|') => {
                        lexer.next();
                        let op = match lexer.next_if('=') {
                            Some(_) => OpT::OrAssign,
                            None => OpT::Or,
                        };
                        let s = Span::new(span.start, lexer.end_pos());
                        self.new_atom(&mut lexer, Token::op(op, s))?;
                    }
                    Some('=') => {
                        lexer.next();
                        let s = Span::new(span.start, lexer.end_pos());
                        self.new_atom(&mut lexer, Token::op(OpT::BwOrAssign, s))?;
                    }
                    _ => {
                        self.new_atom(&mut lexer, Token::op(OpT::BwOr, span))?;
                    }
                },
                '^' => self.two_char_op(&mut lexer, OpT::Xor, OpT::XorAssign, '=')?,
                '&' => match lexer.peek() {
                    Some('&') => {
                        lexer.next();
                        let op = match lexer.next_if('=') {
                            Some(_) => OpT::AndAssign,
                            None => OpT::And,
                        };
                        let s = Span::new(span.start, lexer.end_pos());
                        self.new_atom(&mut lexer, Token::op(op, s))?;
                    }
                    Some('=') => {
                        lexer.next();
                        let s = Span::new(span.start, lexer.end_pos());
                        self.new_atom(&mut lexer, Token::op(OpT::BwAndAssign, s))?;
                    }
                    _ => {
                        self.new_atom(&mut lexer, Token::op(OpT::BwAnd, span))?;
                    }
                },
                '!' => self.two_char_op(&mut lexer, OpT::Bang, OpT::Ne, '=')?,
                '(' => self.new_atom(&mut lexer, Token::par(ParT::RoundOpen, span))?,
                '[' => self.new_atom(&mut lexer, Token::par(ParT::SquareOpen, span))?,
                '{' => self.new_atom(&mut lexer, Token::par(ParT::CurlyOpen, span))?,
                ')' => self.new_atom(&mut lexer, Token::par(ParT::RoundClose, span))?,
                ']' => self.new_atom(&mut lexer, Token::par(ParT::SquareClose, span))?,
                '}' => self.new_atom(&mut lexer, Token::par(ParT::CurlyClose, span))?,
                ',' => self.new_atom(&mut lexer, Token::pct(PctT::Comma, span))?,
                ';' => self.new_atom(&mut lexer, Token::pct(PctT::Semi, span))?,
                ':' => self.new_atom(&mut lexer, Token::pct(PctT::Colon, span))?,
                _ => lexer.continue_literal(),
            }
        }

        self.end_literal(&mut lexer)?;

        Ok(lexer.tokens)
    }

    fn new_atom(&mut self, lexer: &mut Lexer<'_>, token: Token) -> crate::Result<()> {
        self.end_literal(lexer)?;
        lexer.tokens.push(token);
        Ok(())
    }

    fn two_char_op(
        &mut self,
        lexer: &mut Lexer<'_>,
        one: OpT,
        two: OpT,
        expected: char,
    ) -> crate::Result<()> {
        let start = lexer.pos();
        match lexer.next_if(expected) {
            Some(_) => {
                let s = Span::new(start, lexer.end_pos());
                self.new_atom(lexer, Token::op(two, s))
            }
            None => {
                let s = Span::from(start);
                self.new_atom(lexer, Token::op(one, s))
            }
        }
    }

    fn end_literal(&mut self, lexer: &mut Lexer<'_>) -> crate::Result<()> {
        let Some(literal) = lexer.literal() else {
            return Ok(());
        };

        let end = lexer.pos();
        let start_col = end.col - literal.chars().count() as u32;
        let span = Span::new(Pos::new(end.line, start_col), end);

        let token = match literal {
            "true" => Token::val(Val::Bool(true), span),
            "false" => Token::val(Val::Bool(false), span),
            "mod" => Token::op(OpT::RemEuclid, span),
            "as" => Token::op(OpT::As, span),
            "is" => Token::op(OpT::Is, span),
            "if" => Token::kw(KwT::If, span),
            "else" => Token::kw(KwT::Else, span),
            "match" => Token::kw(KwT::Match, span),
            "while" => Token::kw(KwT::While, span),
            "for" => Token::kw(KwT::For, span),
            "in" => Token::kw(KwT::In, span),
            "fn" => Token::kw(KwT::Fn, span),
            "return" => Token::kw(KwT::Return, span),
            "let" => Token::kw(KwT::Let, span),
            "mut" => Token::kw(KwT::Mut, span),
            _ => {
                if literal.chars().next().unwrap().is_ascii_digit() {
                    let val = parse_num_literal(literal, span)?;
                    Token::Val(val)
                } else {
                    for (i, c) in literal.char_indices() {
                        match c {
                            '0'..='9' => (),
                            'a'..='z' => (),
                            'A'..='Z' => (),
                            '_' => (),
                            _ => {
                                let span = Span::from(span.start.plus(i as u32));
                                return Err(crate::Error::InvalidIdentChar(span));
                            }
                        }
                    }

                    let id = self.idents.push(literal);
                    Token::ident(id, span)
                }
            }
        };

        lexer.end_literal();
        lexer.tokens.push(token);

        Ok(())
    }

    fn char_literal(&mut self, lexer: &mut Lexer<'_>) -> crate::Result<()> {
        self.end_literal(lexer)?;

        let start = lexer.pos();
        let char = match lexer.next() {
            Some('\'') => {
                let span = Span::new(start, lexer.end_pos());
                self.errors.push(crate::Error::EmptyCharLiteral(span));
                lexer.tokens.push(Token::val(Val::Char('\0'), span));
                return Ok(());
            }
            Some('\\') => match self.escape_char(lexer) {
                Ok(c) => c,
                Err(e) => {
                    self.errors.push(e.error);
                    let span = Span::new(start, lexer.end_pos());
                    lexer.tokens.push(Token::val(Val::Char('\0'), span));
                    return Ok(());
                }
            },
            Some(c) => c,
            None => {
                let span = Span::new(start, lexer.end_pos());
                return Err(crate::Error::EmptyCharLiteral(span));
            }
        };

        if lexer.next_if('\'').is_none() {
            let s = Span::from(start);
            self.errors.push(crate::Error::MissingClosingQuote(s));
        }

        let span = Span::new(start, lexer.end_pos());
        lexer.tokens.push(Token::val(Val::Char(char), span));

        Ok(())
    }

    fn string_literal(&mut self, lexer: &mut Lexer<'_>) -> crate::Result<()> {
        self.end_literal(lexer)?;

        let start = lexer.pos();
        while let Some(c) = lexer.next() {
            match c {
                '"' => {
                    self.end_string_literal(lexer, start)?;
                    return Ok(());
                }
                '\\' if lexer.peek() == Some('\n') => {
                    lexer.next();

                    while let Some(c) = lexer.peek() {
                        if c.is_ascii_whitespace() {
                            lexer.next();
                        } else {
                            break;
                        }
                    }
                }
                '\\' => match self.escape_char(lexer) {
                    Ok(c) => lexer.str_literal.push(c),
                    Err(e) => {
                        if e.fail {
                            if e.end_str {
                                self.end_string_literal(lexer, start)?;
                            }
                            return Err(e.error);
                        }

                        self.errors.push(e.error);
                        if e.end_str {
                            self.end_string_literal(lexer, start)?;
                            return Ok(());
                        }
                    }
                },
                _ => lexer.str_literal.push(c),
            }
        }

        let s = Span::from(start);
        Err(crate::Error::MissingClosingQuote(s))
    }

    fn end_string_literal(&mut self, lexer: &mut Lexer<'_>, start: Pos) -> crate::Result<()> {
        let str = Val::Str(lexer.str_literal.clone());
        let span = Span::new(start, lexer.end_pos());
        lexer.tokens.push(Token::val(str, span));
        lexer.str_literal.clear();
        Ok(())
    }

    fn line_comment(&mut self, lexer: &mut Lexer<'_>) -> crate::Result<()> {
        while lexer.next_if_not('\n').is_some() {}
        Ok(())
    }

    fn block_comment(&mut self, lexer: &mut Lexer<'_>) -> crate::Result<()> {
        while let Some(c) = lexer.next() {
            if c == '\n' {
                lexer.new_line();
            } else if c == '*' && lexer.next_if('/').is_some() {
                break;
            }
        }
        Ok(())
    }
}

fn parse_num_literal(literal: &str, span: Span) -> crate::Result<ValSpan> {
    let mut chars = literal.char_indices().peekable();
    let (_, c) = chars.next().expect("literals are always non empty");

    let mut int_accum;
    match c {
        '0' => match chars.next() {
            Some((_, 'b')) => {
                let num = parse_prefixed_integer_literal::<1>(chars, span)?;
                return Ok(ValSpan::new(Val::Int(num), span));
            }
            Some((_, 'o')) => {
                let num = parse_prefixed_integer_literal::<3>(chars, span)?;
                return Ok(ValSpan::new(Val::Int(num), span));
            }
            Some((_, 'x')) => {
                let num = parse_prefixed_integer_literal::<4>(chars, span)?;
                return Ok(ValSpan::new(Val::Int(num), span));
            }
            Some((_, '.')) => {
                let num = parse_float(literal, span)?;
                return Ok(ValSpan::new(Val::Float(num), span));
            }
            Some((_, radix)) => {
                return Err(crate::Error::InvalidIntRadix(
                    radix,
                    Span::pos(span.start.line, span.start.col + 1),
                ))
            }
            None => {
                return Ok(ValSpan::new(Val::Int(0), span));
            }
        },
        '1'..='9' => {
            int_accum = (c as u32 - '0' as u32) as i128;
        }
        // don't allow `.5` notation, because it's stupid
        _ => unreachable!("number literals have to start with [0-9]"),
    }

    let mut int_overflow = false;
    let mut last_underscore = false;
    while let Some((i, c)) = chars.next() {
        last_underscore = false;
        match c {
            '0'..='9' => {
                if !int_overflow {
                    match int_accum.checked_mul(10) {
                        Some(val) => {
                            let digit = (c as u32) - ('0' as u32);
                            int_accum = val + digit as i128;
                        }
                        None => int_overflow = true,
                    }
                }
            }
            '.' => {
                let num = parse_float(literal, span)?;
                return Ok(ValSpan::new(Val::Float(num), span));
            }
            'f' => {
                if chars.next().is_some() {
                    let span = Span::new(span.start.plus(i as u32 + 1), span.end);
                    return Err(crate::Error::TrailingFloatLitChars(span));
                }

                let num = parse_float(&literal[..literal.len() - 1], span)?;
                return Ok(ValSpan::new(Val::Float(num), span));
            }
            'e' | 'E' => {
                let num = parse_float(literal, span)?;
                return Ok(ValSpan::new(Val::Float(num), span));
            }
            '_' => {
                last_underscore = true;
                continue;
            }
            _ => {
                let span = Span::from(span.start.plus(i as u32));
                return Err(crate::Error::InvalidNumChar(c, span));
            }
        }
    }

    if int_overflow {
        return Err(crate::Error::IntOverflow(span));
    }

    if last_underscore {
        return Err(crate::Error::IntEndsWithUnderscore(span.end()));
    }

    Ok(ValSpan::new(Val::Int(int_accum), span))
}

fn parse_prefixed_integer_literal<const BITS: u32>(
    mut chars: impl Iterator<Item = (usize, char)>,
    span: Span,
) -> crate::Result<i128> {
    let max_value: u32 = 2u32.pow(BITS);
    let mut accum: i128 = 0;

    for i in 0.. {
        let c = match chars.next() {
            Some((_, c)) => c,
            None if i == 0 => {
                return Err(crate::Error::MissingIntDigits(span.end()));
            }
            None => break,
        };

        let digit = match c {
            '0'..='9' => {
                let n = (c as u32) - ('0' as u32);
                if n >= max_value {
                    let span = Span::from(span.start.plus(i));
                    return Err(crate::Error::IntDigitTooLarge(c, span));
                }
                n
            }
            'a'..='f' => {
                let n = 10 + (c as u32) - ('a' as u32);
                if n >= max_value {
                    let span = Span::from(span.start.plus(i));
                    return Err(crate::Error::IntDigitTooLarge(c, span));
                }
                n
            }
            'A'..='F' => {
                let n = 10 + (c as u32) - ('A' as u32);
                if n >= max_value {
                    let span = Span::from(span.start.plus(i));
                    return Err(crate::Error::IntDigitTooLarge(c, span));
                }
                n
            }
            // allow underscore as visual separator
            '_' => continue,
            _ => {
                let span = Span::from(span.start.plus(i as u32));
                return Err(crate::Error::InvalidNumChar(c, span));
            }
        };

        let Some(shifted) = accum.checked_shl(BITS) else {
            return Err(crate::Error::IntOverflow(span));
        };

        accum = shifted;
        accum += digit as i128;
    }

    Ok(accum)
}

fn parse_float(literal: &str, span: Span) -> crate::Result<f64> {
    // TODO: disallow underscores at start and end of integer part, fractional part and exponent.
    let stripped: String = literal.chars().filter(|c| *c != '_').collect();
    stripped
        .parse()
        .map_err(|_| crate::Error::InvalidFloatLiteral(span))
}
