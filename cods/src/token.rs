use std::ops::{self, Deref, DerefMut};

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
                '+' => self.new_token(&mut state, Token::op(OpType::Add, range))?,
                '-' | '−' => self.new_token(&mut state, Token::op(OpType::Sub, range))?,
                '*' | '×' => self.new_token(&mut state, Token::op(OpType::Mul, range))?,
                '/' | '÷' => self.new_token(&mut state, Token::op(OpType::Div, range))?,
                '%' => self.new_token(&mut state, Token::op(OpType::Rem, range))?,
                '°' => self.new_token(&mut state, Token::mood(ModType::Degree, range))?,
                '!' => self.new_token(&mut state, Token::mood(ModType::Factorial, range))?,
                '^' => self.new_token(&mut state, Token::op(OpType::Pow, range))?,
                '(' => self.new_token(&mut state, Token::par(ParType::RoundOpen, range))?,
                '[' => self.new_token(&mut state, Token::par(ParType::SquareOpen, range))?,
                '{' => self.new_token(&mut state, Token::par(ParType::CurlyOpen, range))?,
                ')' => self.new_token(&mut state, Token::par(ParType::RoundClose, range))?,
                ']' => self.new_token(&mut state, Token::par(ParType::SquareClose, range))?,
                '}' => self.new_token(&mut state, Token::par(ParType::CurlyClose, range))?,
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
                    "pow" => Token::cmd(CmdType::Pow, range),
                    "ln" => Token::cmd(CmdType::Ln, range),
                    "log" => Token::cmd(CmdType::Log, range),
                    "sqrt" => Token::cmd(CmdType::Sqrt, range),
                    "nCr" => Token::cmd(CmdType::Ncr, range),
                    "sin" => Token::cmd(CmdType::Sin, range),
                    "cos" => Token::cmd(CmdType::Cos, range),
                    "tan" => Token::cmd(CmdType::Tan, range),
                    "asin" => Token::cmd(CmdType::Asin, range),
                    "acos" => Token::cmd(CmdType::Acos, range),
                    "atan" => Token::cmd(CmdType::Atan, range),
                    "gcd" => Token::cmd(CmdType::Gcd, range),
                    "min" => Token::cmd(CmdType::Min, range),
                    "max" => Token::cmd(CmdType::Max, range),
                    "div" => Token::op(OpType::IntDiv, range),
                    "mod" => Token::op(OpType::Rem, range),
                    "π" | "pi" => Token::num(Val::PI, range),
                    "τ" | "tau" => Token::num(Val::TAU, range),
                    "e" => Token::num(Val::E, range ),
                    _ => {
                        if literal.chars().next().unwrap().is_digit(10) {
                            let val = if let Ok(i) = literal.parse::<i128>() {
                                Val::Int(i)
                            } else if let Ok(f) = literal.parse::<f64>() {
                                 Val::Float(f)
                            } else {
                                return Err(crate::Error::InvalidNumberFormat(range));
                            };
                            Token::num(val, range)
                        } else if let Ok(v) = literal.parse::<T>() {
                            Token::num(Val::Var(v), range)
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
    pub fn num(val: Val<T>, range: Range) -> Self {
        Self::Num(Num::new(val, range))
    }

    pub fn op(typ: OpType, range: Range) -> Self {
        Self::Op(Op::new(typ, range))
    }

    pub fn cmd(typ: CmdType, range: Range) -> Self {
        Self::Cmd(Cmd::new(typ, range))
    }

    pub fn mood(typ: ModType, range: Range) -> Self {
        Self::Mod(Mod::new(typ, range))
    }

    pub fn par(typ: ParType, range: Range) -> Self {
        Self::Par(Par::new(typ, range))
    }

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

    pub fn as_num(&self) -> Option<Num<T>> {
        match self {
            Self::Num(n) => Some(*n),
            _ => None,
        }
    }

    pub fn as_op(&self) -> Option<Op> {
        match self {
            Self::Op(o) => Some(*o),
            _ => None,
        }
    }

    pub fn as_par(&self) -> Option<Par> {
        match self {
            Self::Par(p) => Some(*p),
            _ => None,
        }
    }

    pub fn range(&self) -> Range {
        match self {
            Self::Num(n) => n.range,
            Self::Op(o) => o.range,
            Self::Cmd(c) => c.range,
            Self::Mod(m) => m.range,
            Self::Par(p) => p.range,
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
pub struct Op {
    pub typ: OpType,
    pub range: Range,
}

impl Op {
    pub const fn new(typ: OpType, range: Range) -> Self {
        Op { typ, range }
    }
}

impl Deref for Op {
    type Target = OpType;

    fn deref(&self) -> &Self::Target {
        &self.typ
    }
}

impl DerefMut for Op {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.typ
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum OpType {
    Add,
    Sub,
    Mul,
    Div,
    IntDiv,
    Rem,
    Pow,
}

impl OpType {
    pub const fn priority(&self) -> usize {
        match self {
            Self::Pow => 2,
            Self::Mul | Self::Div | Self::IntDiv | Self::Rem => 1,
            Self::Add | Self::Sub => 0,
        }
    }

    pub const fn as_sign(&self) -> Option<Sign> {
        match self {
            Self::Add => Some(Sign::Positive),
            Self::Sub => Some(Sign::Negative),
            Self::Mul | Self::Div | Self::IntDiv | Self::Rem | Self::Pow => None,
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
pub struct Cmd {
    pub typ: CmdType,
    pub range: Range,
}

impl Cmd {
    pub const fn new(typ: CmdType, range: Range) -> Self {
        Self { typ, range }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CmdType {
    Pow,
    Ln,
    Log,
    Sqrt,
    Ncr,
    Sin,
    Cos,
    Tan,
    Asin,
    Acos,
    Atan,
    Gcd,
    Min,
    Max,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Mod {
    pub typ: ModType,
    pub range: Range,
}

impl Mod {
    pub const fn new(typ: ModType, range: Range) -> Self {
        Self { typ, range }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ModType {
    Degree,
    Factorial,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Par {
    pub typ: ParType,
    pub range: Range,
}

impl Deref for Par {
    type Target = ParType;

    fn deref(&self) -> &Self::Target {
        &self.typ
    }
}

impl DerefMut for Par {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.typ
    }
}

impl Par {
    pub const fn new(typ: ParType, range: Range) -> Self {
        Self { typ, range }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ParType {
    RoundOpen,
    RoundClose,
    SquareOpen,
    SquareClose,
    CurlyOpen,
    CurlyClose,
}

impl ParType {
    pub const fn is_opening(&self) -> bool {
        match self {
            Self::RoundOpen | Self::SquareOpen | Self::CurlyOpen => true,
            Self::RoundClose | Self::SquareClose | Self::CurlyClose => false,
        }
    }

    pub const fn matches(&self, other: Self) -> bool {
        match self {
            Self::RoundOpen => matches!(other, Self::RoundClose),
            Self::RoundClose => matches!(other, Self::RoundOpen),
            Self::SquareOpen => matches!(other, Self::SquareClose),
            Self::SquareClose => matches!(other, Self::SquareOpen),
            Self::CurlyOpen => matches!(other, Self::CurlyClose),
            Self::CurlyClose => matches!(other, Self::CurlyOpen),
        }
    }

    pub const fn par_type(&self) -> ParKind {
        match self {
            Self::RoundOpen | Self::RoundClose => ParKind::Round,
            Self::SquareOpen | Self::SquareClose => ParKind::Square,
            Self::CurlyOpen | Self::CurlyClose => ParKind::Curly,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ParKind {
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
                Token::num(Val::Float(432.432), Range::of(0, 7)),
                Token::op(OpType::Add, Range::pos(8)),
                Token::num(Val::Float(24324.543), Range::of(10, 19)),
            ],
        );
    }

    #[test]
    fn simple_mul() {
        check(
            "604.453 *3562.543",
            vec![
                Token::num(Val::Float(604.453), Range::of(0, 7)),
                Token::op(OpType::Mul, Range::pos(8)),
                Token::num(Val::Float(3562.543), Range::of(9, 17)),
            ],
        );
    }

    #[test]
    fn add_mul() {
        check(
            "(32+ 604.453)* 3562.543",
            vec![
                Token::par(ParType::RoundOpen, Range::pos(0)),
                Token::num(Val::Int(32), Range::of(1, 3)),
                Token::op(OpType::Add, Range::pos(3)),
                Token::num(Val::Float(604.453), Range::of(5, 12)),
                Token::par(ParType::RoundClose, Range::pos(12)),
                Token::op(OpType::Mul, Range::pos(13)),
                Token::num(Val::Float(3562.543), Range::of(15, 23)),
            ],
        );
    }

    fn check(input: &str, output: Vec<Token<DummyVar>>) {
        let tokens = Context::<DummyVar>::new().tokenize(input).unwrap();
        assert_eq!(tokens, output);
    }
}
