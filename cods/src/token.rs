use std::ops;

use crate::{Context, Var};

macro_rules! match_warn_case {
    (
        $state:ident,
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
                    $state.warnings.push(crate::Warning::ConfusingCase($range, $lit));
                    $res
                } else
        )+ )*
        { $catch_all }
    }};
}

struct Tokenizer<T: Var> {
    tokens: Vec<Token<T>>,
    literal: String,
    char_index: usize,
}

impl<T: Var> Tokenizer<T> {
    fn new() -> Self {
        Self {
            tokens: Vec::new(),
            literal: String::new(),
            char_index: 0,
        }
    }
}

impl<T: Var> Context<T> {
    pub fn tokenize(&mut self, string: &str) -> crate::Result<Vec<Token<T>>, T> {
        let mut state = Tokenizer::new();

        for c in string.chars() {
            let range = Range::pos(state.char_index);
            match c {
                ' ' | '\n' | '\r' => self.complete_literal(&mut state)?,
                '+' => self.new_token(&mut state, Token::Op(Op::Add(range)))?,
                '-' | '−' => self.new_token(&mut state, Token::Op(Op::Sub(range)))?,
                '*' | '×' => self.new_token(&mut state, Token::Op(Op::Mul(range)))?,
                '/' | '÷' => self.new_token(&mut state, Token::Op(Op::Div(range)))?,
                '%' => self.new_token(&mut state, Token::Op(Op::Rem(range)))?,
                '°' => self.new_token(&mut state, Token::Mod(Mod::Degree(range)))?,
                '!' => self.new_token(&mut state, Token::Mod(Mod::Factorial(range)))?,
                '^' => self.new_token(&mut state, Token::Op(Op::Pow(range)))?,
                '(' => self.new_token(&mut state, Token::Par(Par::RoundOpen(range)))?,
                '[' => self.new_token(&mut state, Token::Par(Par::SquareOpen(range)))?,
                '{' => self.new_token(&mut state, Token::Par(Par::CurlyOpen(range)))?,
                ')' => self.new_token(&mut state, Token::Par(Par::RoundClose(range)))?,
                ']' => self.new_token(&mut state, Token::Par(Par::SquareClose(range)))?,
                '}' => self.new_token(&mut state, Token::Par(Par::CurlyClose(range)))?,
                ',' => self.new_token(&mut state, Token::Sep(Sep::Comma(range)))?,
                c => state.literal.push(c),
            }
            state.char_index += 1;
        }

        self.complete_literal(&mut state)?;

        Ok(state.tokens)
    }

    fn new_token(&mut self, state: &mut Tokenizer<T>, token: Token<T>) -> crate::Result<(), T> {
        self.complete_literal(state)?;
        state.tokens.push(token);
        Ok(())
    }

    fn complete_literal(&mut self, state: &mut Tokenizer<T>) -> crate::Result<(), T> {
        if !state.literal.is_empty() {
            let start = state.char_index - state.literal.chars().count();
            let range = Range::of(start, state.char_index);

            let literal = &state.literal;
            let token = match_warn_case! {
                self,
                range,
                match literal {
                    "pow" => Token::Cmd(Cmd::Pow(range)),
                    "ln" => Token::Cmd(Cmd::Ln(range)),
                    "log" => Token::Cmd(Cmd::Log(range)),
                    "sqrt" => Token::Cmd(Cmd::Sqrt(range)),
                    "nCr" => Token::Cmd(Cmd::Ncr(range)),
                    "sin" => Token::Cmd(Cmd::Sin(range)),
                    "cos" => Token::Cmd(Cmd::Cos(range)),
                    "tan" => Token::Cmd(Cmd::Tan(range)),
                    "asin" => Token::Cmd(Cmd::Asin(range)),
                    "acos" => Token::Cmd(Cmd::Acos(range)),
                    "atan" => Token::Cmd(Cmd::Atan(range)),
                    "gcd" => Token::Cmd(Cmd::Gcd(range)),
                    "div" => Token::Op(Op::IntDiv(range)),
                    "mod" => Token::Op(Op::Rem(range)),
                    "π" | "pi" => Token::Num(Num::new(Val::PI, range)),
                    "τ" | "tau" => Token::Num(Num::new(Val::TAU, range)),
                    "e" => Token::Num(Num::new(Val::E, range )),
                    _ => {
                        if literal.chars().next().unwrap().is_digit(10) {
                            let val = if let Ok(i) = literal.parse::<i128>() {
                                Val::Int(i)
                            } else if let Ok(f) = literal.parse::<f64>() {
                                 Val::Float(f)
                            } else {
                                return Err(crate::Error::InvalidNumberFormat(range));
                            };
                            Token::Num(Num::new(val, range))
                        } else if let Ok(v) = literal.parse::<T>() {
                            Token::Num(Num::new(Val::Var(v), range))
                        } else {
                            return Err(crate::Error::UnknownValue(range));
                        }
                    }
                }
            };

            state.literal.clear();
            state.tokens.push(token);
        }

        Ok(())
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Token<T: Var> {
    Num(Num<T>),
    Op(Op),
    Cmd(Cmd),
    Mod(Mod),
    Par(Par),
    Sep(Sep),
}

impl<T: Var> Token<T> {
    pub fn is_num(&self) -> bool {
        matches!(self, Self::Num(_))
    }

    pub fn is_op(&self) -> bool {
        matches!(self, Self::Op(_))
    }

    pub fn is_cmd(&self) -> bool {
        matches!(self, Self::Cmd(_))
    }

    pub fn is_par(&self) -> bool {
        matches!(self, Self::Par(_))
    }

    pub fn is_sep(&self) -> bool {
        matches!(self, Self::Sep(_))
    }

    pub fn num(&self) -> Option<Num<T>> {
        match self {
            Self::Num(n) => Some(*n),
            _ => None,
        }
    }

    pub fn op(&self) -> Option<Op> {
        match self {
            Self::Op(o) => Some(*o),
            _ => None,
        }
    }

    pub fn par(&self) -> Option<Par> {
        match self {
            Self::Par(p) => Some(*p),
            _ => None,
        }
    }

    pub fn range(&self) -> Range {
        match self {
            Self::Num(n) => n.range,
            Self::Op(o) => o.range(),
            Self::Cmd(c) => c.range(),
            Self::Mod(m) => m.range(),
            Self::Par(p) => p.range(),
            Self::Sep(s) => s.range(),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Num<T: Var> {
    pub val: Val<T>,
    pub range: Range,
}

impl<T: Var> Num<T> {
    pub fn new(val: Val<T>, range: Range) -> Self {
        Self { val, range }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Val<T: Var> {
    Int(i128),
    Float(f64),
    Var(T),
    TAU,
    PI,
    E,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Op {
    Add(Range),
    Sub(Range),
    Mul(Range),
    Div(Range),
    IntDiv(Range),
    Rem(Range),
    Pow(Range),
}

impl Op {
    pub const fn priority(&self) -> usize {
        match self {
            Self::Pow(_) => 2,
            Self::Mul(_) | Self::Div(_) | Self::IntDiv(_) | Self::Rem(_) => 1,
            Self::Add(_) | Self::Sub(_) => 0,
        }
    }

    pub const fn range(&self) -> Range {
        match *self {
            Self::Add(r) => r,
            Self::Sub(r) => r,
            Self::Mul(r) => r,
            Self::Div(r) => r,
            Self::IntDiv(r) => r,
            Self::Rem(r) => r,
            Self::Pow(r) => r,
        }
    }

    pub const fn as_sign(&self) -> Option<Sign> {
        match self {
            Self::Add(_) => Some(Sign::Positive),
            Self::Sub(_) => Some(Sign::Negative),
            Self::Mul(_) | Self::Div(_) | Self::IntDiv(_) | Self::Rem(_) | Self::Pow(_) => None,
        }
    }

    pub const fn is_sign(&self) -> bool {
        self.as_sign().is_some()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Sign {
    Negative,
    Positive,
}

impl ops::Not for Sign {
    type Output = Sign;

    fn not(self) -> Self::Output {
        match self {
            Self::Negative => Self::Positive,
            Self::Positive => Self::Negative,
        }
    }
}

impl Sign {
    pub const fn is_negative(&self) -> bool {
        matches!(self, Self::Negative)
    }

    pub const fn is_positive(&self) -> bool {
        matches!(self, Self::Positive)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Cmd {
    Pow(Range),
    Ln(Range),
    Log(Range),
    Sqrt(Range),
    Ncr(Range),
    Sin(Range),
    Cos(Range),
    Tan(Range),
    Asin(Range),
    Acos(Range),
    Atan(Range),
    Gcd(Range),
}

impl Cmd {
    pub const fn range(&self) -> Range {
        match *self {
            Self::Pow(r) => r,
            Self::Ln(r) => r,
            Self::Log(r) => r,
            Self::Sqrt(r) => r,
            Self::Ncr(r) => r,
            Self::Sin(r) => r,
            Self::Cos(r) => r,
            Self::Tan(r) => r,
            Self::Asin(r) => r,
            Self::Acos(r) => r,
            Self::Atan(r) => r,
            Self::Gcd(r) => r,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Mod {
    Degree(Range),
    Factorial(Range),
}

impl Mod {
    pub const fn range(&self) -> Range {
        match *self {
            Self::Degree(r) => r,
            Self::Factorial(r) => r,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Par {
    RoundOpen(Range),
    RoundClose(Range),
    SquareOpen(Range),
    SquareClose(Range),
    CurlyOpen(Range),
    CurlyClose(Range),
}

impl Par {
    pub const fn is_opening(&self) -> bool {
        match self {
            Self::RoundOpen(_) | Self::SquareOpen(_) | Self::CurlyOpen(_) => true,
            Self::RoundClose(_) | Self::SquareClose(_) | Self::CurlyClose(_) => false,
        }
    }

    pub const fn matches(&self, other: Self) -> bool {
        match self {
            Self::RoundOpen(_) => matches!(other, Par::RoundClose(_)),
            Self::RoundClose(_) => matches!(other, Par::RoundOpen(_)),
            Self::SquareOpen(_) => matches!(other, Par::SquareClose(_)),
            Self::SquareClose(_) => matches!(other, Par::SquareOpen(_)),
            Self::CurlyOpen(_) => matches!(other, Par::CurlyClose(_)),
            Self::CurlyClose(_) => matches!(other, Par::CurlyOpen(_)),
        }
    }

    pub const fn range(&self) -> Range {
        match *self {
            Self::RoundOpen(r) => r,
            Self::RoundClose(r) => r,
            Self::SquareOpen(r) => r,
            Self::SquareClose(r) => r,
            Self::CurlyOpen(r) => r,
            Self::CurlyClose(r) => r,
        }
    }

    pub const fn par_type(&self) -> ParType {
        match self {
            Self::RoundOpen(_) | Self::RoundClose(_) => ParType::Round,
            Self::SquareOpen(_) | Self::SquareClose(_) => ParType::Square,
            Self::CurlyOpen(_) | Self::CurlyClose(_) => ParType::Curly,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ParType {
    Round,
    Square,
    Curly,
    Mixed,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Sep {
    Comma(Range),
}

impl Sep {
    pub const fn range(&self) -> Range {
        match *self {
            Self::Comma(r) => r,
        }
    }
}

/// Range of character indices
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Range {
    pub start: usize,
    pub end: usize,
}

impl Range {
    pub const fn of(start: usize, end: usize) -> Range {
        Range { start, end }
    }

    pub const fn span(a: Range, b: Range) -> Range {
        Self::of(a.start, b.end)
    }

    pub const fn between(a: Range, b: Range) -> Range {
        Self::of(a.end, b.start)
    }

    pub const fn pos(pos: usize) -> Range {
        Self::of(pos, pos + 1)
    }

    pub const fn len(&self) -> usize {
        self.end - self.start
    }

    pub const fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub const fn intersects(&self, other: &Range) -> bool {
        self.contains(other.start) || other.contains(self.start)
    }

    pub const fn contains(&self, pos: usize) -> bool {
        self.start <= pos && self.end > pos
    }
}

#[cfg(test)]
mod test {
    use crate::DummyVar;

    use super::*;

    #[test]
    fn simple_add() {
        check(
            "432.432 + 24324.543",
            vec![
                Token::Num(Num::new(Val::Float(432.432), Range::of(0, 7))),
                Token::Op(Op::Add(Range::pos(8))),
                Token::Num(Num::new(Val::Float(24324.543), Range::of(10, 19))),
            ],
        );
    }

    #[test]
    fn simple_mul() {
        check(
            "604.453 *3562.543",
            vec![
                Token::Num(Num::new(Val::Float(604.453), Range::of(0, 7))),
                Token::Op(Op::Mul(Range::pos(8))),
                Token::Num(Num::new(Val::Float(3562.543), Range::of(9, 17))),
            ],
        );
    }

    #[test]
    fn add_mul() {
        check(
            "(32+ 604.453)* 3562.543",
            vec![
                Token::Par(Par::RoundOpen(Range::pos(0))),
                Token::Num(Num::new(Val::Int(32), Range::of(1, 3))),
                Token::Op(Op::Add(Range::pos(3))),
                Token::Num(Num::new(Val::Float(604.453), Range::of(5, 12))),
                Token::Par(Par::RoundClose(Range::pos(12))),
                Token::Op(Op::Mul(Range::pos(13))),
                Token::Num(Num::new(Val::Float(3562.543), Range::of(15, 23))),
            ],
        );
    }

    fn check(input: &str, output: Vec<Token<DummyVar>>) {
        let tokens = Context::<DummyVar>::new().tokenize(input).unwrap();
        assert_eq!(tokens, output);
    }
}
