use std::iter::Peekable;
use std::str::Chars;

use crate::{Context, Span};

pub use token::*;

mod str;
#[cfg(test)]
mod test;
mod token;

struct Lexer<'a> {
    tokens: Vec<Token>,
    literal: String,
    chars: Peekable<Chars<'a>>,
    cursor: u32,
}

impl<'a> Lexer<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            tokens: Vec::new(),
            literal: String::new(),
            chars: input.chars().peekable(),
            cursor: 0,
        }
    }

    fn next(&mut self) -> Option<char> {
        self.cursor += 1;
        self.chars.next()
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

    const fn pos(&self) -> u32 {
        self.cursor.saturating_sub(1)
    }
}

impl Context {
    pub fn lex(&mut self, string: &str) -> crate::Result<Vec<Token>> {
        let mut lexer = Lexer::new(string);

        while let Some(c) = lexer.next() {
            let span = Span::pos(lexer.pos());
            match c {
                '"' => self.string_literal(&mut lexer)?,
                ' ' | '\r' => self.end_literal(&mut lexer)?,
                '\n' => self.new_atom(&mut lexer, Token::pct(PctT::Newln, span))?,
                '+' => self.two_char_op(&mut lexer, OpT::Add, OpT::AddAssign, '=')?,
                '-' => match lexer.peek() {
                    Some('=') => {
                        lexer.next();
                        let s = Span::of(span.start, lexer.pos() + 1);
                        self.new_atom(&mut lexer, Token::op(OpT::SubAssign, s))?;
                    }
                    Some('>') => {
                        lexer.next();
                        let s = Span::of(span.start, lexer.pos() + 1);
                        self.new_atom(&mut lexer, Token::pct(PctT::Arrow, s))?;
                    }
                    _ => {
                        self.new_atom(&mut lexer, Token::op(OpT::Sub, span))?;
                    }
                },
                '*' => self.two_char_op(&mut lexer, OpT::Mul, OpT::MulAssign, '=')?,
                '/' => match lexer.peek() {
                    Some('=') => {
                        lexer.next();
                        let s = Span::of(span.start, lexer.pos() + 1);
                        self.new_atom(&mut lexer, Token::op(OpT::DivAssign, s))?;
                    }
                    Some('/') => {
                        lexer.next();
                        self.line_comment(&mut lexer)?;
                    }
                    _ => {
                        self.new_atom(&mut lexer, Token::op(OpT::Div, span))?;
                    }
                },
                '%' => self.new_atom(&mut lexer, Token::op(OpT::Rem, span))?,
                '=' => self.two_char_op(&mut lexer, OpT::Assign, OpT::Eq, '=')?,
                '.' => match lexer.peek() {
                    Some('.') => {
                        lexer.next();
                        let op = match lexer.next_if('=') {
                            Some(_) => OpT::RangeIn,
                            None => OpT::RangeEx,
                        };
                        let s = Span::of(span.start, lexer.pos() + 1);
                        self.new_atom(&mut lexer, Token::op(op, s))?;
                    }
                    Some(c) if c.is_digit(10) => lexer.literal.push('.'),
                    _ => self.new_atom(&mut lexer, Token::op(OpT::Dot, span))?,
                },
                '<' => self.two_char_op(&mut lexer, OpT::Lt, OpT::Le, '=')?,
                '>' => self.two_char_op(&mut lexer, OpT::Gt, OpT::Ge, '=')?,
                '|' => self.two_char_op(&mut lexer, OpT::BwOr, OpT::Or, '|')?,
                '&' => self.two_char_op(&mut lexer, OpT::BwAnd, OpT::And, '&')?,
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
                c => lexer.literal.push(c),
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
        match lexer.next_if(expected) {
            Some(_) => {
                let s = Span::of(lexer.pos() - 1, lexer.pos() + 1);
                self.new_atom(lexer, Token::op(two, s))
            }
            None => {
                let s = Span::pos(lexer.pos());
                self.new_atom(lexer, Token::op(one, s))
            }
        }
    }

    fn end_literal(&mut self, lexer: &mut Lexer<'_>) -> crate::Result<()> {
        if lexer.literal.is_empty() {
            return Ok(());
        }

        let start = lexer.pos() - lexer.literal.chars().count() as u32;
        let span = Span::of(start, lexer.pos());

        let literal = lexer.literal.as_str();
        let token = match literal {
            "true" => Token::val(Val::Bool(true), span),
            "false" => Token::val(Val::Bool(false), span),
            "mod" => Token::op(OpT::RemEuclid, span),
            "as" => Token::op(OpT::As, span),
            "is" => Token::op(OpT::Is, span),
            "if" => Token::kw(KwT::If, span),
            "else" => Token::kw(KwT::Else, span),
            "while" => Token::kw(KwT::While, span),
            "for" => Token::kw(KwT::For, span),
            "in" => Token::kw(KwT::In, span),
            "fun" => Token::kw(KwT::Fun, span),
            "val" => Token::kw(KwT::Val, span),
            "var" => Token::kw(KwT::Var, span),
            _ => {
                if literal.chars().next().unwrap().is_digit(10) {
                    let num = if let Ok(i) = literal.parse::<i128>() {
                        Val::Int(i)
                    } else if let Ok(f) = literal.parse::<f64>() {
                        Val::Float(f)
                    } else {
                        return Err(crate::Error::InvalidNumberFormat(span));
                    };
                    Token::val(num, span)
                } else {
                    for (i, c) in literal.char_indices() {
                        match c {
                            '0'..='9' => (),
                            'a'..='z' => (),
                            'A'..='Z' => (),
                            '_' => (),
                            _ => {
                                return Err(crate::Error::InvalidChar(Span::pos(
                                    span.start + i as u32,
                                )))
                            }
                        }
                    }

                    let id = self.idents.push(literal);
                    Token::ident(id, span)
                }
            }
        };

        lexer.literal.clear();
        lexer.tokens.push(token);

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
                    Ok(c) => lexer.literal.push(c),
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
                _ => lexer.literal.push(c),
            }
        }

        let s = Span::pos(start);
        Err(crate::Error::MissingClosingQuote(s))
    }

    fn end_string_literal(&mut self, lexer: &mut Lexer<'_>, start: u32) -> crate::Result<()> {
        let str = Val::Str(lexer.literal.clone());
        let span = Span::of(start, lexer.pos() + 1);
        lexer.tokens.push(Token::val(str, span));
        lexer.literal.clear();
        Ok(())
    }

    fn line_comment(&mut self, lexer: &mut Lexer<'_>) -> crate::Result<()> {
        while let Some(c) = lexer.next() {
            if c == '\n' {
                break;
            }
        }
        Ok(())
    }
}
