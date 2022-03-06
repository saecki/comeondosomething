use std::fmt::{Display, Write};
use std::ops::{self, Deref, DerefMut};

use crate::{Context, Ext, Provider};

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

struct Tokenizer<T: Ext> {
    tokens: Vec<Token<T>>,
    literal: String,
    char_index: usize,
}

impl<T: Ext> Tokenizer<T> {
    fn new() -> Self {
        Self {
            tokens: Vec::new(),
            literal: String::new(),
            char_index: 0,
        }
    }
}

impl<T: Ext, P: Provider<T>> Context<T, P> {
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
                '^' => self.new_token(&mut state, Token::op(OpType::Pow, range))?,
                '=' => self.new_token(&mut state, Token::op(OpType::Equals, range))?,
                '°' => self.new_token(&mut state, Token::mood(ModType::Degree, range))?,
                '!' => self.new_token(&mut state, Token::mood(ModType::Factorial, range))?,
                '(' => self.new_token(&mut state, Token::par(ParType::RoundOpen, range))?,
                '[' => self.new_token(&mut state, Token::par(ParType::SquareOpen, range))?,
                '{' => self.new_token(&mut state, Token::par(ParType::CurlyOpen, range))?,
                ')' => self.new_token(&mut state, Token::par(ParType::RoundClose, range))?,
                ']' => self.new_token(&mut state, Token::par(ParType::SquareClose, range))?,
                '}' => self.new_token(&mut state, Token::par(ParType::CurlyClose, range))?,
                ',' => self.new_token(&mut state, Token::sep(SepType::Comma, range))?,
                ';' => self.new_token(&mut state, Token::sep(SepType::Semi, range))?,
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
                    "clamp" => Token::cmd(CmdType::Clamp, range),
                    "div" => Token::op(OpType::IntDiv, range),
                    "mod" => Token::op(OpType::Rem, range),
                    "π" | "pi" => Token::num(Val::PI, range),
                    "τ" | "tau" => Token::num(Val::TAU, range),
                    "e" => Token::num(Val::E, range ),
                    _ => {
                        if literal.chars().next().unwrap().is_digit(10) {
                            let val = if let Ok(i) = literal.parse::<i128>() {
                                Val::int(i)
                            } else if let Ok(f) = literal.parse::<f64>() {
                                 Val::float(f)
                            } else {
                                return Err(crate::Error::InvalidNumberFormat(range));
                            };
                            Token::num(val, range)
                        } else if let Some(v) = self.provider.parse(literal) {
                            Token::num(Val::Ext(v), range)
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

                            let id = self.push_var(literal);
                            Token::num(Val::Var(id), range)
                        }
                    }
                }
            };

            state.literal.clear();
            state.tokens.push(token);
        }

        Ok(())
    }

    fn push_var(&mut self, name: &str) -> VarId {
        for (id, v) in self.vars.iter().enumerate() {
            if v.name == name {
                return id;
            }
        }

        let id = self.vars.len();
        self.vars.push(Var::new(name.to_owned(), None));
        id
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Token<T: Ext> {
    Num(Num<T>),
    Op(Op),
    Cmd(Cmd),
    Mod(Mod),
    Par(Par),
    Sep(Sep),
}

impl<T: Ext> Token<T> {
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

    pub fn sep(typ: SepType, range: Range) -> Self {
        Self::Sep(Sep::new(typ, range))
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
            Self::Sep(s) => s.range,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Num<T: Ext> {
    pub val: Val<T>,
    pub range: Range,
}

impl<T: Ext> Num<T> {
    pub fn new(val: Val<T>, range: Range) -> Self {
        Self { val, range }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Val<T: Ext> {
    Ext(T),
    Plain(PlainVal),
    Var(VarId),
}

impl<T: Ext> Val<T> {
    pub const TAU: Self = Self::Plain(PlainVal::TAU);
    pub const PI: Self = Self::Plain(PlainVal::PI);
    pub const E: Self = Self::Plain(PlainVal::E);

    pub fn int(i: i128) -> Self {
        Self::Plain(PlainVal::Int(i))
    }

    pub fn float(f: f64) -> Self {
        Self::Plain(PlainVal::Float(f))
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum PlainVal {
    Int(i128),
    Float(f64),
    TAU,
    PI,
    E,
}

pub type VarId = usize;

#[derive(Clone, Debug, PartialEq)]
pub struct Var<T: Ext> {
    pub name: String,
    pub value: Option<Val<T>>,
}

impl<T: Ext> Var<T> {
    pub fn new(name: String, value: Option<Val<T>>) -> Self {
        Self { name, value }
    }
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
    Equals,
}

impl OpType {
    pub const fn priority(&self) -> usize {
        match self {
            Self::Pow => 4,
            Self::Mul | Self::Div | Self::IntDiv | Self::Rem => 2,
            Self::Add | Self::Sub => 1,
            Self::Equals => 0,
        }
    }

    pub const fn as_sign(&self) -> Option<Sign> {
        match self {
            Self::Add => Some(Sign::Positive),
            Self::Sub => Some(Sign::Negative),
            Self::Mul | Self::Div | Self::IntDiv | Self::Rem | Self::Pow | Self::Equals => None,
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
    Clamp,
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
pub struct Sep {
    pub typ: SepType,
    pub range: Range,
}

impl Deref for Sep {
    type Target = SepType;

    fn deref(&self) -> &Self::Target {
        &self.typ
    }
}

impl Sep {
    pub const fn new(typ: SepType, range: Range) -> Self {
        Self { typ, range }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SepType {
    Comma,
    Semi,
}

impl Display for SepType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Comma => f.write_char(','),
            Self::Semi => f.write_char(';'),
        }
    }
}

impl SepType {
    pub fn is_semi(&self) -> bool {
        matches!(self, Self::Semi)
    }
}

/// Range of character indices
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Range {
    pub start: usize,
    pub end: usize,
}

impl Range {
    pub const fn of(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub const fn span(a: Self, b: Self) -> Self {
        Self::of(a.start, b.end)
    }

    pub const fn between(a: Self, b: Self) -> Self {
        Self::of(a.end, b.start)
    }

    pub const fn pos(pos: usize) -> Self {
        Self::of(pos, pos + 1)
    }

    pub const fn offset(&self, offset: isize) -> Self {
        Self::of(
            (self.start as isize + offset) as usize,
            (self.end as isize + offset) as usize,
        )
    }

    pub const fn len(&self) -> usize {
        self.end - self.start
    }

    pub const fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub const fn intersects(&self, other: &Self) -> bool {
        self.contains(other.start) || other.contains(self.start)
    }

    pub const fn contains(&self, pos: usize) -> bool {
        self.start <= pos && self.end > pos
    }
}

#[cfg(test)]
mod test {
    use crate::{DummyProvider, ExtDummy};

    use super::*;

    #[test]
    fn simple_add() {
        check(
            "432.432 + 24324.543",
            vec![
                Token::num(Val::float(432.432), Range::of(0, 7)),
                Token::op(OpType::Add, Range::pos(8)),
                Token::num(Val::float(24324.543), Range::of(10, 19)),
            ],
        );
    }

    #[test]
    fn simple_mul() {
        check(
            "604.453 *3562.543",
            vec![
                Token::num(Val::float(604.453), Range::of(0, 7)),
                Token::op(OpType::Mul, Range::pos(8)),
                Token::num(Val::float(3562.543), Range::of(9, 17)),
            ],
        );
    }

    #[test]
    fn add_mul() {
        check(
            "(32+ 604.453)* 3562.543",
            vec![
                Token::par(ParType::RoundOpen, Range::pos(0)),
                Token::num(Val::int(32), Range::of(1, 3)),
                Token::op(OpType::Add, Range::pos(3)),
                Token::num(Val::float(604.453), Range::of(5, 12)),
                Token::par(ParType::RoundClose, Range::pos(12)),
                Token::op(OpType::Mul, Range::pos(13)),
                Token::num(Val::float(3562.543), Range::of(15, 23)),
            ],
        );
    }

    #[test]
    fn vars() {
        let mut ctx = Context::new(DummyProvider);
        let tokens = ctx.tokenize("x64 = 2; Arm = 3").unwrap();

        assert_eq!(
            ctx.vars,
            [Var::new("x64".into(), None), Var::new("Arm".into(), None)]
        );

        assert_eq!(
            tokens,
            [
                Token::num(Val::Var(0), Range::of(0, 3)),
                Token::op(OpType::Equals, Range::pos(4)),
                Token::num(Val::int(2), Range::pos(6)),
                Token::sep(SepType::Semi, Range::pos(7)),
                Token::num(Val::Var(1), Range::of(9, 12)),
                Token::op(OpType::Equals, Range::pos(13)),
                Token::num(Val::int(3), Range::pos(15)),
            ],
        );
    }

    #[test]
    fn invalid_char() {
        let mut ctx = Context::new(DummyProvider);
        let error = ctx.tokenize("x6ä = 2; Arm = 3").unwrap_err();

        assert_eq!(error, crate::Error::InvalidChar(Range::pos(2)));
    }

    fn check(input: &str, output: Vec<Token<ExtDummy>>) {
        let tokens = Context::new(DummyProvider).tokenize(input).unwrap();
        assert_eq!(output, tokens);
    }
}
