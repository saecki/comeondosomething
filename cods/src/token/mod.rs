use std::f64::consts;
use std::fmt::{self, Display};
use std::iter::Peekable;
use std::ops::{Deref, DerefMut};
use std::str::Chars;

use crate::Context;

#[cfg(test)]
mod test;

const LITERAL_SUFFIXES: [(&str, OpT); 4] = [
    ("_deg", OpT::Degree),
    ("deg", OpT::Degree),
    ("_rad", OpT::Radian),
    ("rad", OpT::Radian),
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

struct Tokenizer<'a> {
    tokens: Vec<Token>,
    literal: String,
    chars: Peekable<Chars<'a>>,
    cursor: usize,
}

impl<'a> Tokenizer<'a> {
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
    pub fn tokenize(&mut self, string: &str) -> crate::Result<Vec<Token>> {
        let mut state = Tokenizer::new(string);

        while let Some(c) = state.next() {
            let range = Range::pos(state.pos());
            match c {
                '"' => self.string_literal(&mut state)?,
                ' ' | '\r' => self.end_literal(&mut state)?,
                '\n' => self.new_atom(&mut state, Token::sep(SepT::Newln, range))?,
                '+' => self.new_atom(&mut state, Token::op(OpT::Add, range))?,
                '-' | '−' => self.new_atom(&mut state, Token::op(OpT::Sub, range))?,
                '*' | '×' => self.new_atom(&mut state, Token::op(OpT::Mul, range))?,
                '/' | '÷' => self.new_atom(&mut state, Token::op(OpT::Div, range))?,
                '%' => self.new_atom(&mut state, Token::op(OpT::Rem, range))?,
                '^' => self.new_atom(&mut state, Token::op(OpT::Pow, range))?,
                '=' => self.two_char_op(&mut state, OpT::Assign, OpT::Eq, '=')?,
                '<' => self.two_char_op(&mut state, OpT::Lt, OpT::Le, '=')?,
                '>' => self.two_char_op(&mut state, OpT::Gt, OpT::Ge, '=')?,
                '|' => self.two_char_op(&mut state, OpT::BwOr, OpT::Or, '|')?,
                '&' => self.two_char_op(&mut state, OpT::BwAnd, OpT::And, '&')?,
                '!' => self.two_char_op(&mut state, OpT::Bang, OpT::Ne, '=')?,
                '°' => self.new_atom(&mut state, Token::op(OpT::Degree, range))?,
                '(' => self.new_atom(&mut state, Token::par(ParT::RoundOpen, range))?,
                '[' => self.new_atom(&mut state, Token::par(ParT::SquareOpen, range))?,
                '{' => self.new_atom(&mut state, Token::par(ParT::CurlyOpen, range))?,
                ')' => self.new_atom(&mut state, Token::par(ParT::RoundClose, range))?,
                ']' => self.new_atom(&mut state, Token::par(ParT::SquareClose, range))?,
                '}' => self.new_atom(&mut state, Token::par(ParT::CurlyClose, range))?,
                ',' => self.new_atom(&mut state, Token::sep(SepT::Comma, range))?,
                ';' => self.new_atom(&mut state, Token::sep(SepT::Semi, range))?,
                c => state.literal.push(c),
            }
        }

        self.end_literal(&mut state)?;

        Ok(state.tokens)
    }

    fn new_atom(&mut self, state: &mut Tokenizer<'_>, token: Token) -> crate::Result<()> {
        self.end_literal(state)?;
        state.tokens.push(token);
        Ok(())
    }

    fn two_char_op(
        &mut self,
        state: &mut Tokenizer<'_>,
        one: OpT,
        two: OpT,
        expected: char,
    ) -> crate::Result<()> {
        match state.next_if(expected) {
            Some(_) => {
                let r = Range::of(state.pos() - 1, state.pos() + 1);
                self.new_atom(state, Token::op(two, r))
            }
            None => {
                let r = Range::pos(state.pos());
                self.new_atom(state, Token::op(one, r))
            }
        }
    }

    fn end_literal(&mut self, state: &mut Tokenizer<'_>) -> crate::Result<()> {
        if !state.literal.is_empty() {
            let start = state.pos() - state.literal.chars().count();
            let range = Range::of(start, state.pos());

            let literal = &state.literal;
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
                            state.literal.clear();
                            state.tokens.push(Token::expr(num, num_range));

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
                            Token::expr(ExprT::Var(id), range)
                        }
                    }
                }
            };

            state.literal.clear();
            state.tokens.push(token);
        }

        Ok(())
    }

    fn string_literal(&mut self, state: &mut Tokenizer<'_>) -> crate::Result<()> {
        self.end_literal(state)?;

        let start = state.pos();
        while let Some(c) = state.next() {
            match c {
                '"' => {
                    self.end_string_literal(state, start)?;
                    return Ok(());
                }
                '\\' => match self.escape_char(state) {
                    Ok(c) => state.literal.push(c),
                    Err(e) => {
                        if e.fail {
                            if e.end_str {
                                self.end_string_literal(state, start)?;
                            }
                            return Err(e.error);
                        } else {
                            self.errors.push(e.error);
                            if e.end_str {
                                self.end_string_literal(state, start)?;
                                return Ok(());
                            }
                        }
                    }
                },
                _ => state.literal.push(c),
            }
        }

        let r = Range::pos(start);
        return Err(crate::Error::MissingClosingQuote(r));
    }

    fn end_string_literal(&mut self, state: &mut Tokenizer<'_>, start: usize) -> crate::Result<()> {
        let str = Val::Str(state.literal.clone());
        let range = Range::of(start, state.pos() + 1);
        state.tokens.push(Token::expr(ExprT::Val(str), range));
        state.literal.clear();
        Ok(())
    }

    fn escape_char(&mut self, state: &mut Tokenizer<'_>) -> Result<char, EscError> {
        let esc_start = state.pos();
        let c = match state.next() {
            Some(c) => c,
            None => {
                let r = Range::pos(state.pos());
                return Err(EscError {
                    error: crate::Error::MissingEscapeChar(r),
                    end_str: false,
                    fail: true,
                });
            }
        };

        let escaped = match c {
            'x' => unicode_escape_char(state, 2, esc_start)?,
            'u' => unicode_escape_char(state, 4, esc_start)?,
            _ => match c {
                '0' => '\0',
                'b' => '\u{8}',
                't' => '\t',
                'n' => '\n',
                'r' => '\r',
                '"' => '"',
                '\\' => '\\',
                '\n' => todo!("eat all whitespace"),
                _ => {
                    return Err(EscError {
                        error: crate::Error::InvalidEscapeChar(c, Range::pos(state.pos())),
                        end_str: false,
                        fail: false,
                    })
                }
            },
        };

        Ok(escaped)
    }
}

struct EscError {
    error: crate::Error,
    end_str: bool,
    fail: bool,
}

fn unicode_escape_char(
    state: &mut Tokenizer<'_>,
    expected: usize,
    esc_start: usize,
) -> Result<char, EscError> {
    if let Some('{') = state.peek() {
        state.next();
        return braced_unicode_escape_char(state, esc_start);
    }

    let mut cp = 0;
    let mut i = 0;
    while let Some(c) = state.next() {
        if c == ' ' || c == '"' {
            let range = Range::pos(state.pos());
            let end_str = c == '"';
            return Err(EscError {
                error: crate::Error::MissingUnicodeEscapeChar {
                    expected,
                    found: i,
                    range,
                },
                end_str,
                fail: false,
            });
        }
        
        let digit = unicode_escape_hex(state, c)?;

        cp <<= 4;
        cp += digit;

        i += 1;

        if i == expected {
            break;
        }
    }

    parse_unicode_cp(state, cp, esc_start)
}

fn braced_unicode_escape_char(
    state: &mut Tokenizer<'_>,
    esc_start: usize,
) -> Result<char, EscError> {
    let mut cp = 0;
    let mut i = 0;
    while let Some(c) = state.next() {
        if c == '}' {
            break;
        }

        if c == ' ' || c == '"' {
            let e_r = Range::pos(state.pos());
            let s_r = e_r.offset(-(i + 1));
            let end_str = c == '"';
            return Err(EscError {
                error: crate::Error::MissingClosingUnicodeEscapePar(s_r, e_r),
                end_str,
                fail: false,
            });
        }

        let digit = unicode_escape_hex(state, c)?;

        cp <<= 4;
        cp += digit;

        i += 1;
    }

    if i > 6 {
        let r = Range::of(esc_start, state.pos() + 1);
        return Err(EscError {
            error: crate::Error::OverlongUnicodeEscape(r),
            end_str: false,
            fail: true,
        });
    }

    parse_unicode_cp(state, cp, esc_start)
}

fn unicode_escape_hex(state: &Tokenizer<'_>, c: char) -> Result<u32, EscError> {
    match c {
        '0'..='9' => Ok(c as u32 - '0' as u32),
        'a'..='f' => Ok(c as u32 - 'a' as u32 + 10),
        'A'..='F' => Ok(c as u32 - 'A' as u32 + 10),
        _ => {
            let r = Range::pos(state.pos());
            return Err(EscError {
                error: crate::Error::InvalidUnicodeEscapeChar(c, r),
                end_str: false,
                fail: false,
            });
        }
    }
}

fn parse_unicode_cp(
    state: &mut Tokenizer<'_>,
    cp: u32,
    esc_start: usize,
) -> Result<char, EscError> {
    match char::from_u32(cp) {
        Some(char) => Ok(char),
        None => {
            let r = Range::of(esc_start, state.pos() + 1);
            Err(EscError {
                error: crate::Error::InvalidUnicodeScalar(cp, r),
                end_str: false,
                fail: true,
            })
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Expr(Expr),
    Fun(Fun),
    Op(Op),
    Par(Par),
    Sep(Sep),
}

impl Token {
    pub fn expr(val: ExprT, range: Range) -> Self {
        Self::Expr(Expr::new(val, range))
    }

    pub fn op(typ: OpT, range: Range) -> Self {
        Self::Op(Op::new(typ, range))
    }

    pub fn fun(typ: FunT, range: Range) -> Self {
        Self::Fun(Fun::new(typ, range))
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
            Self::Par(p) => p.range,
            Self::Sep(s) => s.range,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Expr {
    pub typ: ExprT,
    pub range: Range,
}

impl Expr {
    pub fn new(expr: ExprT, range: Range) -> Self {
        Self { typ: expr, range }
    }
}

#[derive(Clone, Debug, PartialEq)]
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

#[derive(Clone, Debug, PartialEq)]
pub enum Val {
    Int(i128),
    Float(f64),
    Bool(bool),
    Str(String),
}

impl Display for Val {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(v) => write!(f, "{v}"),
            Self::Float(v) => write!(f, "{v}"),
            Self::Bool(v) => write!(f, "{v}"),
            Self::Str(v) => write!(f, "{v}"),
        }
    }
}

impl Val {
    pub const fn type_name(&self) -> &'static str {
        match self {
            Self::Int(_) => "int",
            Self::Float(_) => "float",
            Self::Bool(_) => "bool",
            Self::Str(_) => "str",
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
    Assign,
    Add,
    Sub,
    Mul,
    Div,
    IntDiv,
    Rem,
    Pow,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    Or,
    And,
    BwOr,
    BwAnd,
    /// Not or Factorial depending on position
    Bang,
    Degree,
    Radian,
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
    Assert,
    AssertEq,
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

    pub const fn is_closing(&self) -> bool {
        !self.is_opening()
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

impl ParKind {
    pub const fn of(l: ParT, r: ParT) -> ParKind {
        if l.matches(r) {
            l.kind()
        } else {
            ParKind::Mixed
        }
    }
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
    Newln,
}

impl Display for SepT {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Comma => write!(f, ","),
            Self::Semi => write!(f, ";"),
            Self::Newln => write!(f, "\\n"),
        }
    }
}

impl SepT {
    pub fn is_semi(&self) -> bool {
        matches!(self, Self::Semi)
    }

    pub fn is_newln(&self) -> bool {
        matches!(self, Self::Newln)
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
