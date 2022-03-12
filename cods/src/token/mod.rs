use std::f64::consts;
use std::fmt::{self, Display, Write};
use std::ops::{self, Deref, DerefMut};

use crate::Context;

#[cfg(test)]
mod test;

const LITERAL_SUFFIXES: [(&'static str, ModT); 4] = [
    ("_deg", ModT::Degree),
    ("deg", ModT::Degree),
    ("_rad", ModT::Radian),
    ("rad", ModT::Radian),
];

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

struct Tokenizer {
    tokens: Vec<Token>,
    literal: String,
    char_index: usize,
}

impl Tokenizer {
    fn new() -> Self {
        Self {
            tokens: Vec::new(),
            literal: String::new(),
            char_index: 0,
        }
    }
}

impl Context {
    pub fn tokenize(&mut self, string: &str) -> crate::Result<Vec<Token>> {
        let mut state = Tokenizer::new();

        for c in string.chars() {
            let range = Range::pos(state.char_index);
            match c {
                ' ' | '\n' | '\r' => self.complete_literal(&mut state)?,
                '+' => self.new_token(&mut state, Token::op(OpT::Add, range))?,
                '-' | '−' => self.new_token(&mut state, Token::op(OpT::Sub, range))?,
                '*' | '×' => self.new_token(&mut state, Token::op(OpT::Mul, range))?,
                '/' | '÷' => self.new_token(&mut state, Token::op(OpT::Div, range))?,
                '%' => self.new_token(&mut state, Token::op(OpT::Rem, range))?,
                '^' => self.new_token(&mut state, Token::op(OpT::Pow, range))?,
                '=' => self.new_token(&mut state, Token::op(OpT::Equals, range))?,
                '°' => self.new_token(&mut state, Token::mood(ModT::Degree, range))?,
                '!' => self.new_token(&mut state, Token::mood(ModT::Factorial, range))?,
                '(' => self.new_token(&mut state, Token::par(ParT::RoundOpen, range))?,
                '[' => self.new_token(&mut state, Token::par(ParT::SquareOpen, range))?,
                '{' => self.new_token(&mut state, Token::par(ParT::CurlyOpen, range))?,
                ')' => self.new_token(&mut state, Token::par(ParT::RoundClose, range))?,
                ']' => self.new_token(&mut state, Token::par(ParT::SquareClose, range))?,
                '}' => self.new_token(&mut state, Token::par(ParT::CurlyClose, range))?,
                ',' => self.new_token(&mut state, Token::sep(SepT::Comma, range))?,
                ';' => self.new_token(&mut state, Token::sep(SepT::Semi, range))?,
                c => state.literal.push(c),
            }
            state.char_index += 1;
        }

        self.complete_literal(&mut state)?;

        Ok(state.tokens)
    }

    fn new_token(&mut self, state: &mut Tokenizer, token: Token) -> crate::Result<()> {
        self.complete_literal(state)?;
        state.tokens.push(token);
        Ok(())
    }

    fn complete_literal(&mut self, state: &mut Tokenizer) -> crate::Result<()> {
        if !state.literal.is_empty() {
            let start = state.char_index - state.literal.chars().count();
            let range = Range::of(start, state.char_index);

            let literal = &state.literal;
            let token = match_warn_case! {
                self,
                range,
                match literal {
                    "π" | "pi" => Token::val(ExprT::PI, range),
                    "τ" | "tau" => Token::val(ExprT::TAU, range),
                    "e" => Token::val(ExprT::E, range ),
                    "true" => Token::val(ExprT::bool(true), range),
                    "false" => Token::val(ExprT::bool(false), range),
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
                    "deg" => Token::mood(ModT::Degree, range),
                    "rad" => Token::mood(ModT::Radian, range),
                    "div" => Token::op(OpT::IntDiv, range),
                    "mod" => Token::op(OpT::Rem, range),
                    _ => {
                        if literal.chars().next().unwrap().is_digit(10) {
                            let mut mood = None;
                            let mut num_lit = literal.as_str();
                            for (s, m) in LITERAL_SUFFIXES {
                                if literal.ends_with(s) {
                                    let mod_range = Range::of(range.end - s.len(), range.end);
                                    mood = Some(Token::mood(m, mod_range));

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
                            state.literal.clear();
                            state.tokens.push(Token::val(num, num_range));

                            if let Some(m) = mood {
                                state.tokens.push(m);
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

                            let id = self.push_var(literal);
                            Token::val(ExprT::Var(id), range)
                        }
                    }
                }
            };

            state.literal.clear();
            state.tokens.push(token);
        }

        Ok(())
    }

    pub fn push_var(&mut self, name: &str) -> VarId {
        for (id, v) in self.vars.iter().enumerate() {
            if v.name == name {
                return VarId(id);
            }
        }

        let id = self.vars.len();
        self.vars.push(Var::new(name.to_owned(), None));
        VarId(id)
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Token {
    Expr(Expr),
    Op(Op),
    Fun(Fun),
    Mod(Mod),
    Par(Par),
    Sep(Sep),
}

impl Token {
    pub fn val(val: ExprT, range: Range) -> Self {
        Self::Expr(Expr::new(val, range))
    }

    pub fn op(typ: OpT, range: Range) -> Self {
        Self::Op(Op::new(typ, range))
    }

    pub fn fun(typ: FunT, range: Range) -> Self {
        Self::Fun(Fun::new(typ, range))
    }

    pub fn mood(typ: ModT, range: Range) -> Self {
        Self::Mod(Mod::new(typ, range))
    }

    pub fn par(typ: ParT, range: Range) -> Self {
        Self::Par(Par::new(typ, range))
    }

    pub fn sep(typ: SepT, range: Range) -> Self {
        Self::Sep(Sep::new(typ, range))
    }

    pub fn is_val(&self) -> bool {
        matches!(self, Self::Expr(_))
    }

    pub fn is_op(&self) -> bool {
        matches!(self, Self::Op(_))
    }

    pub fn is_fun(&self) -> bool {
        matches!(self, Self::Fun(_))
    }

    pub fn is_par(&self) -> bool {
        matches!(self, Self::Par(_))
    }

    pub fn is_sep(&self) -> bool {
        matches!(self, Self::Sep(_))
    }

    pub fn as_val(&self) -> Option<Expr> {
        match self {
            Self::Expr(n) => Some(*n),
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
            Self::Expr(n) => n.range,
            Self::Op(o) => o.range,
            Self::Fun(c) => c.range,
            Self::Mod(m) => m.range,
            Self::Par(p) => p.range,
            Self::Sep(s) => s.range,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Expr {
    pub typ: ExprT,
    pub range: Range,
}

impl Expr {
    pub fn new(expr: ExprT, range: Range) -> Self {
        Self { typ: expr, range }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ExprT {
    Val(Val),
    Var(VarId),
}

impl ExprT {
    pub const TAU: Self = Self::Val(Val::Float(consts::TAU));
    pub const PI: Self = Self::Val(Val::Float(consts::PI));
    pub const E: Self = Self::Val(Val::Float(consts::E));

    pub fn int(i: i128) -> Self {
        Self::Val(Val::Int(i))
    }

    pub fn float(f: f64) -> Self {
        Self::Val(Val::Float(f))
    }

    pub fn bool(b: bool) -> Self {
        Self::Val(Val::Bool(b))
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Val {
    Int(i128),
    Float(f64),
    Bool(bool),
}

impl Display for Val {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(v) => write!(f, "{v}"),
            Self::Float(v) => write!(f, "{v}"),
            Self::Bool(v) => write!(f, "{v}"),
        }
    }
}

impl Val {
    pub const fn type_name(&self) -> &'static str {
        match self {
            Self::Int(_) => "int",
            Self::Float(_) => "float",
            Self::Bool(_) => "bool",
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct VarId(pub usize);

#[derive(Clone, Debug, PartialEq)]
pub struct Var {
    pub name: String,
    pub value: Option<Val>,
}

impl Var {
    pub fn new(name: String, value: Option<Val>) -> Self {
        Self { name, value }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Op {
    pub typ: OpT,
    pub range: Range,
}

impl Op {
    pub const fn new(typ: OpT, range: Range) -> Self {
        Op { typ, range }
    }
}

impl Deref for Op {
    type Target = OpT;

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
pub enum OpT {
    Add,
    Sub,
    Mul,
    Div,
    IntDiv,
    Rem,
    Pow,
    Equals,
}

impl OpT {
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
pub struct Fun {
    pub typ: FunT,
    pub range: Range,
}

impl Fun {
    pub const fn new(typ: FunT, range: Range) -> Self {
        Self { typ, range }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FunT {
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
    Print,
    Println,
    Spill,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Mod {
    pub typ: ModT,
    pub range: Range,
}

impl Mod {
    pub const fn new(typ: ModT, range: Range) -> Self {
        Self { typ, range }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ModT {
    Degree,
    Radian,
    Factorial,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Par {
    pub typ: ParT,
    pub range: Range,
}

impl Deref for Par {
    type Target = ParT;

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
    pub const fn new(typ: ParT, range: Range) -> Self {
        Self { typ, range }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ParT {
    RoundOpen,
    RoundClose,
    SquareOpen,
    SquareClose,
    CurlyOpen,
    CurlyClose,
}

impl ParT {
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

    pub const fn kind(&self) -> ParKind {
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
    pub typ: SepT,
    pub range: Range,
}

impl Deref for Sep {
    type Target = SepT;

    fn deref(&self) -> &Self::Target {
        &self.typ
    }
}

impl Sep {
    pub const fn new(typ: SepT, range: Range) -> Self {
        Self { typ, range }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SepT {
    Comma,
    Semi,
}

impl Display for SepT {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Comma => f.write_char(','),
            Self::Semi => f.write_char(';'),
        }
    }
}

impl SepT {
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
