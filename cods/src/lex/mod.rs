use std::iter::Peekable;
use std::str::Chars;

use crate::{Context, Range};

pub use token::*;

mod str;
#[cfg(test)]
mod test;
mod token;

const LITERAL_SUFFIXES: [(&str, OpT); 4] = [
    ("_deg", OpT::Degree),
    ("deg", OpT::Degree),
    ("_rad", OpT::Radian),
    ("rad", OpT::Radian),
];

macro_rules! match_warn_case {
    (
        $lexer:ident,
        $range:ident,
        match $v:ident {
            $( $($lit:literal)|+ => $res:expr, )*
            _ => $catch_all:expr $(,)?
        }
    ) => {{
        $( $(
                if $lit == $v {
                    $res
                } else if $lit.eq_ignore_ascii_case(&$v) {
                    $lexer.warnings.push(crate::Warning::ConfusingCase($range, $lit));
                    $res
                } else
        )+ )*
        { $catch_all }
    }};
}

struct Lexer<'a> {
    tokens: Vec<Token>,
    literal: String,
    chars: Peekable<Chars<'a>>,
    cursor: usize,
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

    const fn pos(&self) -> usize {
        self.cursor.saturating_sub(1)
    }
}

impl Context {
    pub fn lex(&mut self, string: &str) -> crate::Result<Vec<Token>> {
        let mut lexer = Lexer::new(string);

        while let Some(c) = lexer.next() {
            let range = Range::pos(lexer.pos());
            match c {
                '"' => self.string_literal(&mut lexer)?,
                ' ' | '\r' => self.end_literal(&mut lexer)?,
                '\n' => self.new_atom(&mut lexer, Token::sep(SepT::Newln, range))?,
                '+' => self.new_atom(&mut lexer, Token::op(OpT::Add, range))?,
                '-' | '−' => self.new_atom(&mut lexer, Token::op(OpT::Sub, range))?,
                '*' | '×' => self.new_atom(&mut lexer, Token::op(OpT::Mul, range))?,
                '/' | '÷' => self.new_atom(&mut lexer, Token::op(OpT::Div, range))?,
                '%' => self.new_atom(&mut lexer, Token::op(OpT::Rem, range))?,
                '^' => self.new_atom(&mut lexer, Token::op(OpT::Pow, range))?,
                '=' => self.two_char_op(&mut lexer, OpT::Assign, OpT::Eq, '=')?,
                '<' => self.two_char_op(&mut lexer, OpT::Lt, OpT::Le, '=')?,
                '>' => self.two_char_op(&mut lexer, OpT::Gt, OpT::Ge, '=')?,
                '|' => self.two_char_op(&mut lexer, OpT::BwOr, OpT::Or, '|')?,
                '&' => self.two_char_op(&mut lexer, OpT::BwAnd, OpT::And, '&')?,
                '!' => self.two_char_op(&mut lexer, OpT::Bang, OpT::Ne, '=')?,
                '°' => self.new_atom(&mut lexer, Token::op(OpT::Degree, range))?,
                '(' => self.new_atom(&mut lexer, Token::par(ParT::RoundOpen, range))?,
                '[' => self.new_atom(&mut lexer, Token::par(ParT::SquareOpen, range))?,
                '{' => self.new_atom(&mut lexer, Token::par(ParT::CurlyOpen, range))?,
                ')' => self.new_atom(&mut lexer, Token::par(ParT::RoundClose, range))?,
                ']' => self.new_atom(&mut lexer, Token::par(ParT::SquareClose, range))?,
                '}' => self.new_atom(&mut lexer, Token::par(ParT::CurlyClose, range))?,
                ',' => self.new_atom(&mut lexer, Token::sep(SepT::Comma, range))?,
                ';' => self.new_atom(&mut lexer, Token::sep(SepT::Semi, range))?,
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
                let r = Range::of(lexer.pos() - 1, lexer.pos() + 1);
                self.new_atom(lexer, Token::op(two, r))
            }
            None => {
                let r = Range::pos(lexer.pos());
                self.new_atom(lexer, Token::op(one, r))
            }
        }
    }

    fn end_literal(&mut self, lexer: &mut Lexer<'_>) -> crate::Result<()> {
        if !lexer.literal.is_empty() {
            let start = lexer.pos() - lexer.literal.chars().count();
            let range = Range::of(start, lexer.pos());

            let literal = &lexer.literal;
            let token = match_warn_case! {
                self,
                range,
                match literal {
                    "π" | "pi" => Token::expr(ExprT::PI, range),
                    "τ" | "tau" => Token::expr(ExprT::TAU, range),
                    "e" => Token::expr(ExprT::E, range ),
                    "true" => Token::expr(ExprT::bool(true), range),
                    "false" => Token::expr(ExprT::bool(false), range),
                    "pow" => Token::fun(FunT::Pow, range),
                    "ln" => Token::fun(FunT::Ln, range),
                    "log" => Token::fun(FunT::Log, range),
                    "sqrt" => Token::fun(FunT::Sqrt, range),
                    "nCr" => Token::fun(FunT::Ncr, range),
                    "sin" => Token::fun(FunT::Sin, range),
                    "cos" => Token::fun(FunT::Cos, range),
                    "tan" => Token::fun(FunT::Tan, range),
                    "asin" => Token::fun(FunT::Asin, range),
                    "acos" => Token::fun(FunT::Acos, range),
                    "atan" => Token::fun(FunT::Atan, range),
                    "gcd" => Token::fun(FunT::Gcd, range),
                    "min" => Token::fun(FunT::Min, range),
                    "max" => Token::fun(FunT::Max, range),
                    "clamp" => Token::fun(FunT::Clamp, range),
                    "print" => Token::fun(FunT::Print, range),
                    "println" => Token::fun(FunT::Println, range),
                    "spill" => Token::fun(FunT::Spill, range),
                    "assert" => Token::fun(FunT::Assert, range),
                    "assert_eq" => Token::fun(FunT::AssertEq, range),
                    "div" => Token::op(OpT::IntDiv, range),
                    "mod" => Token::op(OpT::Rem, range),
                    "deg" => Token::op(OpT::Degree, range),
                    "rad" => Token::op(OpT::Radian, range),
                    "if" => Token::kw(KwT::If, range),
                    "else" => Token::kw(KwT::Else, range),
                    _ => {
                        if literal.chars().next().unwrap().is_digit(10) {
                            let mut mood = None;
                            let mut num_lit = literal.as_str();
                            for (s, op) in LITERAL_SUFFIXES {
                                if literal.ends_with(s) {
                                    let op_r = Range::of(range.end - s.len(), range.end);
                                    mood = Some(Token::op(op, op_r));

                                    num_lit = &literal[0..(literal.len() - s.len())];
                                    break;
                                }
                            }

                            let num_range = Range::of(start, start + num_lit.len());
                            let num = if let Ok(i) = num_lit.parse::<i128>() {
                                ExprT::int(i)
                            } else if let Ok(f) = num_lit.parse::<f64>() {
                                ExprT::float(f)
                            } else {
                                return Err(crate::Error::InvalidNumberFormat(num_range));
                            };
                            lexer.literal.clear();
                            lexer.tokens.push(Token::expr(num, num_range));

                            if let Some(m) = mood {
                                lexer.tokens.push(m);
                            }

                            return Ok(());
                        } else {
                            for (i, c) in literal.char_indices() {
                                match c {
                                    '0'..='9' => (),
                                    'a'..='z' => (),
                                    'A'..='Z' => (),
                                    '_' => (),
                                    _ => return Err(crate::Error::InvalidChar(Range::pos(range.start + i))),
                                }
                            }

                            let id = self.push_ident(literal);
                            Token::expr(ExprT::Var(id), range)
                        }
                    }
                }
            };

            lexer.literal.clear();
            lexer.tokens.push(token);
        }

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
                '\\' => match self.escape_char(lexer) {
                    Ok(c) => lexer.literal.push(c),
                    Err(e) => {
                        if e.fail {
                            if e.end_str {
                                self.end_string_literal(lexer, start)?;
                            }
                            return Err(e.error);
                        } else {
                            self.errors.push(e.error);
                            if e.end_str {
                                self.end_string_literal(lexer, start)?;
                                return Ok(());
                            }
                        }
                    }
                },
                _ => lexer.literal.push(c),
            }
        }

        let r = Range::pos(start);
        Err(crate::Error::MissingClosingQuote(r))
    }

    fn end_string_literal(&mut self, lexer: &mut Lexer<'_>, start: usize) -> crate::Result<()> {
        let str = Val::Str(lexer.literal.clone());
        let range = Range::of(start, lexer.pos() + 1);
        lexer.tokens.push(Token::expr(ExprT::Val(str), range));
        lexer.literal.clear();
        Ok(())
    }
}
