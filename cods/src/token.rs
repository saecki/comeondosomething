use crate::{Context, Val};

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

#[derive(Default)]
struct Tokenizer {
    tokens: Vec<Token>,
    literal: String,
    char_index: usize,
}

impl Context {
    pub fn tokenize(&mut self, string: &str) -> crate::Result<Vec<Token>> {
        let mut state = Tokenizer::default();

        for c in string.chars() {
            let range = pos(state.char_index);
            match c {
                ' ' | '\n' | '\r' => self.complete_literal(&mut state)?,
                '+' => self.new_token(&mut state, Token::Op(Op::Add(range)))?,
                '-' | '−' => self.new_token(&mut state, Token::Op(Op::Sub(range)))?,
                '*' | '×' => self.new_token(&mut state, Token::Op(Op::Mul(range)))?,
                '/' | '÷' => self.new_token(&mut state, Token::Op(Op::Div(range)))?,
                '°' => self.new_token(&mut state, Token::Mod(Mod::Degree(range)))?,
                '!' => self.new_token(&mut state, Token::Mod(Mod::Factorial(range)))?,
                '^' => self.new_token(&mut state, Token::Op(Op::Pow(range)))?,
                '(' => self.new_token(&mut state, Token::Par(Par::RoundOpen(range)))?,
                '[' => self.new_token(&mut state, Token::Par(Par::SquareOpen(range)))?,
                ')' => self.new_token(&mut state, Token::Par(Par::RoundClose(range)))?,
                ']' => self.new_token(&mut state, Token::Par(Par::SquareClose(range)))?,
                '_' | '\'' => (), // visual separator
                ',' | '.' => state.literal.push('.'),
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
            let range = range(start, state.char_index);

            let literal = &state.literal;
            let token = match_warn_case! {
                self,
                range,
                match literal {
                    "sqrt" => Token::Cmd(Cmd::Sqrt(range)),
                    "sin" => Token::Cmd(Cmd::Sin(range)),
                    "cos" => Token::Cmd(Cmd::Cos(range)),
                    "tan" => Token::Cmd(Cmd::Tan(range)),
                    "π" | "pi" => Token::Num(Num {
                        val: Val::PI,
                        range,
                    }),
                    "τ" | "tau" => Token::Num(Num {
                        val: Val::TAU,
                        range,
                    }),
                    "e" => Token::Num(Num { val: Val::E, range }),
                    _ => {
                        let i = || state.literal.parse::<i128>().ok().map(Val::Int);
                        let f = || state.literal.parse::<f64>().ok().map(Val::Float);
                        let val = i().or_else(f).ok_or_else(|| crate::Error::InvalidNumberFormat(range))?;
                        Token::Num(Num { val, range })
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
pub enum Token {
    Num(Num),
    Op(Op),
    Cmd(Cmd),
    Mod(Mod),
    Par(Par),
}

impl Token {
    pub const fn is_num(&self) -> bool {
        matches!(self, Self::Num(_))
    }

    pub const fn is_op(&self) -> bool {
        matches!(self, Self::Op(_))
    }

    pub const fn is_cmd(&self) -> bool {
        matches!(self, Self::Cmd(_))
    }

    pub const fn is_par(&self) -> bool {
        matches!(self, Self::Par(_))
    }

    pub const fn num(&self) -> Option<Num> {
        match self {
            Self::Num(n) => Some(*n),
            _ => None,
        }
    }

    pub const fn op(&self) -> Option<Op> {
        match self {
            Self::Op(o) => Some(*o),
            _ => None,
        }
    }

    pub const fn par(&self) -> Option<Par> {
        match self {
            Self::Par(p) => Some(*p),
            _ => None,
        }
    }

    pub const fn range(&self) -> Range {
        match self {
            Self::Num(n) => n.range,
            Self::Op(o) => o.range(),
            Self::Cmd(r) => r.range(),
            Self::Mod(r) => r.range(),
            Self::Par(p) => p.range(),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Num {
    pub val: Val,
    pub range: Range,
}

pub const fn num(val: Val, start: usize, end: usize) -> Num {
    Num {
        val,
        range: range(start, end),
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Op {
    Add(Range),
    Sub(Range),
    Mul(Range),
    Div(Range),
    Pow(Range),
}

impl Op {
    pub const fn priority(&self) -> usize {
        match self {
            Self::Pow(_) => 0,
            Self::Mul(_) | Self::Div(_) => 1,
            Self::Add(_) | Self::Sub(_) => 2,
        }
    }

    pub const fn range(&self) -> Range {
        match *self {
            Self::Mul(r) => r,
            Self::Div(r) => r,
            Self::Add(r) => r,
            Self::Sub(r) => r,
            Self::Pow(r) => r,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Cmd {
    Sqrt(Range),
    Sin(Range),
    Cos(Range),
    Tan(Range),
}

impl Cmd {
    pub const fn range(&self) -> Range {
        match *self {
            Self::Sqrt(r) => r,
            Self::Sin(r) => r,
            Self::Cos(r) => r,
            Self::Tan(r) => r,
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
}

impl Par {
    pub const fn is_opening(&self) -> bool {
        match self {
            Self::SquareOpen(_) | Self::RoundOpen(_) => true,
            Self::SquareClose(_) | Self::RoundClose(_) => false,
        }
    }

    pub const fn matches(&self, other: Self) -> bool {
        match self {
            Self::RoundOpen(_) => matches!(other, Par::RoundClose(_)),
            Self::RoundClose(_) => matches!(other, Par::RoundOpen(_)),
            Self::SquareOpen(_) => matches!(other, Par::SquareClose(_)),
            Self::SquareClose(_) => matches!(other, Par::SquareOpen(_)),
        }
    }

    pub const fn range(&self) -> Range {
        match *self {
            Self::RoundOpen(r) => r,
            Self::RoundClose(r) => r,
            Self::SquareOpen(r) => r,
            Self::SquareClose(r) => r,
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

pub const fn range(start: usize, end: usize) -> Range {
    Range { start, end }
}

pub const fn span(a: Range, b: Range) -> Range {
    range(a.start, b.end)
}

pub const fn between(a: Range, b: Range) -> Range {
    range(a.end, b.start)
}

pub const fn pos(pos: usize) -> Range {
    range(pos, pos + 1)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn simple_add() {
        check(
            "432,432 + 24324,543",
            vec![
                Token::Num(num(Val::Float(432.432), 0, 7)),
                Token::Op(Op::Add(pos(8))),
                Token::Num(num(Val::Float(24324.543), 10, 19)),
            ],
        );
    }

    #[test]
    fn simple_mul() {
        check(
            "604.453 *3562,543",
            vec![
                Token::Num(num(Val::Float(604.453), 0, 7)),
                Token::Op(Op::Mul(pos(8))),
                Token::Num(num(Val::Float(3562.543), 9, 17)),
            ],
        );
    }

    #[test]
    fn add_mul() {
        check(
            "(32+ 604.453)* 3562,543",
            vec![
                Token::Par(Par::RoundOpen(pos(0))),
                Token::Num(num(Val::Int(32), 1, 3)),
                Token::Op(Op::Add(pos(3))),
                Token::Num(num(Val::Float(604.453), 5, 12)),
                Token::Par(Par::RoundClose(pos(12))),
                Token::Op(Op::Mul(pos(13))),
                Token::Num(num(Val::Float(3562.543), 15, 23)),
            ],
        );
    }

    fn check(input: &str, output: Vec<Token>) {
        let tokens = Context::default().tokenize(input).unwrap();
        assert_eq!(tokens, output);
    }
}