use std::collections::VecDeque;
use std::iter::Peekable;
use std::str::Chars;

use crate::{Context, Pos, Span};

pub use token::*;

mod str;
#[cfg(test)]
mod test;
mod token;

pub struct TokenStream<'a> {
    lexer: Lexer<'a>,
    state: StreamState,
}

impl<'a> TokenStream<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            lexer: Lexer::new(input),
            state: StreamState::Next,
        }
    }
}

impl Context {
    pub fn peek_token<'a>(
        &mut self,
        stream: &'a mut TokenStream,
    ) -> crate::Result<Option<&'a Token>> {
        if !stream.lexer.tokens.is_empty() {
            return Ok(stream.lexer.tokens.get(0));
        }
        if stream.state == StreamState::Next {
            stream.state = self.lex_next(&mut stream.lexer)?;
            return Ok(stream.lexer.tokens.get(0));
        }

        Ok(None)
    }

    pub fn next_token(&mut self, stream: &mut TokenStream<'_>) -> crate::Result<Option<Token>> {
        if !stream.lexer.tokens.is_empty() {
            return Ok(stream.lexer.tokens.pop_front());
        }
        if stream.state == StreamState::Next {
            stream.state = self.lex_next(&mut stream.lexer)?;
            return Ok(stream.lexer.tokens.pop_front());
        }

        Ok(None)
    }
}

#[derive(PartialEq, Eq)]
enum StreamState {
    Next,
    Eof,
}

struct Lexer<'a> {
    tokens: VecDeque<Token>,
    literal: String,
    chars: Peekable<Chars<'a>>,
    cursor: Pos,
}

impl<'a> Lexer<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            tokens: VecDeque::new(),
            literal: String::new(),
            chars: input.chars().peekable(),
            cursor: Pos::new(0, 0),
        }
    }

    fn next(&mut self) -> Option<char> {
        self.cursor.col += 1;
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

    fn new_line(&mut self) {
        self.cursor.line += 1;
        self.cursor.col = 0;
    }

    const fn pos(&self) -> Pos {
        Pos::new(self.cursor.line, self.cursor.col.saturating_sub(1))
    }

    const fn end_pos(&self) -> Pos {
        self.cursor
    }
}

impl Context {
    fn lex_next(&mut self, lexer: &mut Lexer<'_>) -> crate::Result<StreamState> {
        while lexer.tokens.is_empty() {
            if let Some(c) = lexer.next() {
                let span = Span::from(lexer.pos());
                match c {
                    '"' => self.string_literal(lexer)?,
                    '\'' => self.char_literal(lexer)?,
                    ' ' | '\r' => self.end_literal(lexer)?,
                    '\n' => {
                        self.new_atom(lexer, Token::pct(PctT::Newln, span))?;
                        lexer.new_line();
                    }
                    '+' => self.two_char_op(lexer, OpT::Add, OpT::AddAssign, '=')?,
                    '-' => match lexer.peek() {
                        Some('=') => {
                            lexer.next();
                            let s = Span::new(span.start, lexer.end_pos());
                            self.new_atom(lexer, Token::op(OpT::SubAssign, s))?;
                        }
                        Some('>') => {
                            lexer.next();
                            let s = Span::new(span.start, lexer.end_pos());
                            self.new_atom(lexer, Token::pct(PctT::Arrow, s))?;
                        }
                        _ => {
                            self.new_atom(lexer, Token::op(OpT::Sub, span))?;
                        }
                    },
                    '*' => self.two_char_op(lexer, OpT::Mul, OpT::MulAssign, '=')?,
                    '/' => match lexer.peek() {
                        Some('=') => {
                            lexer.next();
                            let s = Span::new(span.start, lexer.end_pos());
                            self.new_atom(lexer, Token::op(OpT::DivAssign, s))?;
                        }
                        Some('/') => {
                            lexer.next();
                            self.line_comment(lexer)?;
                        }
                        Some('*') => {
                            lexer.next();
                            self.block_comment(lexer)?;
                        }
                        _ => {
                            self.new_atom(lexer, Token::op(OpT::Div, span))?;
                        }
                    },
                    '%' => self.new_atom(lexer, Token::op(OpT::Rem, span))?,
                    '=' => self.two_char_op(lexer, OpT::Assign, OpT::Eq, '=')?,
                    '.' => match lexer.peek() {
                        Some('.') => {
                            lexer.next();
                            let op = match lexer.next_if('=') {
                                Some(_) => OpT::RangeIn,
                                None => OpT::RangeEx,
                            };
                            let s = Span::new(span.start, lexer.end_pos());
                            self.new_atom(lexer, Token::op(op, s))?;
                        }
                        Some(c)
                            if !lexer.literal.is_empty()
                                && lexer.literal.chars().all(|c| c.is_digit(10))
                                && c.is_digit(10) =>
                        {
                            lexer.literal.push('.')
                        }
                        _ => self.new_atom(lexer, Token::op(OpT::Dot, span))?,
                    },
                    '<' => self.two_char_op(lexer, OpT::Lt, OpT::Le, '=')?,
                    '>' => self.two_char_op(lexer, OpT::Gt, OpT::Ge, '=')?,
                    '|' => self.two_char_op(lexer, OpT::BwOr, OpT::Or, '|')?,
                    '&' => self.two_char_op(lexer, OpT::BwAnd, OpT::And, '&')?,
                    '!' => self.two_char_op(lexer, OpT::Bang, OpT::Ne, '=')?,
                    '(' => self.new_atom(lexer, Token::par(ParT::RoundOpen, span))?,
                    '[' => self.new_atom(lexer, Token::par(ParT::SquareOpen, span))?,
                    '{' => self.new_atom(lexer, Token::par(ParT::CurlyOpen, span))?,
                    ')' => self.new_atom(lexer, Token::par(ParT::RoundClose, span))?,
                    ']' => self.new_atom(lexer, Token::par(ParT::SquareClose, span))?,
                    '}' => self.new_atom(lexer, Token::par(ParT::CurlyClose, span))?,
                    ',' => self.new_atom(lexer, Token::pct(PctT::Comma, span))?,
                    ';' => self.new_atom(lexer, Token::pct(PctT::Semi, span))?,
                    ':' => self.new_atom(lexer, Token::pct(PctT::Colon, span))?,
                    c => lexer.literal.push(c),
                }
            } else {
                self.end_literal(lexer)?;
                return Ok(StreamState::Eof);
            }
        }

        Ok(StreamState::Next)
    }

    fn new_atom(&mut self, lexer: &mut Lexer<'_>, token: Token) -> crate::Result<()> {
        self.end_literal(lexer)?;
        lexer.tokens.push_back(token);
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
        if lexer.literal.is_empty() {
            return Ok(());
        }

        let end = lexer.pos();
        let start_col = end.col - lexer.literal.chars().count() as u32;
        let span = Span::new(Pos::new(end.line, start_col), end);

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
            "return" => Token::kw(KwT::Return, span),
            "val" => Token::kw(KwT::Val, span),
            "var" => Token::kw(KwT::Var, span),
            _ => {
                if literal.chars().next().unwrap().is_digit(10) {
                    let num = if let Some(s) = literal.strip_prefix("0b") {
                        match i128::from_str_radix(s, 2) {
                            Ok(i) => Val::Int(i),
                            Err(_) => return Err(crate::Error::InvalidNumberFormat(span)),
                        }
                    } else if let Some(s) = literal.strip_prefix("0o") {
                        match i128::from_str_radix(s, 8) {
                            Ok(i) => Val::Int(i),
                            Err(_) => return Err(crate::Error::InvalidNumberFormat(span)),
                        }
                    } else if let Some(s) = literal.strip_prefix("0x") {
                        match i128::from_str_radix(s, 16) {
                            Ok(i) => Val::Int(i),
                            Err(_) => return Err(crate::Error::InvalidNumberFormat(span)),
                        }
                    } else if let Ok(i) = literal.parse::<i128>() {
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
                                    span.start.line,
                                    start_col + i as u32,
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
        lexer.tokens.push_back(token);

        Ok(())
    }

    fn char_literal(&mut self, lexer: &mut Lexer<'_>) -> crate::Result<()> {
        self.end_literal(lexer)?;

        let start = lexer.pos();
        let char = match lexer.next() {
            Some('\'') => {
                let span = Span::new(start, lexer.end_pos());
                self.errors.push(crate::Error::EmptyCharLiteral(span));
                lexer.tokens.push_back(Token::val(Val::Char('\0'), span));
                return Ok(());
            }
            Some('\\') => match self.escape_char(lexer) {
                Ok(c) => c,
                Err(e) => {
                    self.errors.push(e.error);
                    let span = Span::new(start, lexer.end_pos());
                    lexer.tokens.push_back(Token::val(Val::Char('\0'), span));
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
        lexer.tokens.push_back(Token::val(Val::Char(char), span));

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

        let s = Span::from(start);
        Err(crate::Error::MissingClosingQuote(s))
    }

    fn end_string_literal(&mut self, lexer: &mut Lexer<'_>, start: Pos) -> crate::Result<()> {
        let str = Val::Str(lexer.literal.clone());
        let span = Span::new(start, lexer.end_pos());
        lexer.tokens.push_back(Token::val(str, span));
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
