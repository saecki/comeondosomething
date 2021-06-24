pub fn tokenize(string: &str) -> crate::Result<Vec<Token>> {
    let mut tokens = Vec::new();
    let mut literal = String::new();

    let mut i = 0;
    for c in string.chars() {
        match c {
            '0'..='9' => literal.push(c),
            ',' | '.' => literal.push('.'),
            '_' | '\'' => (), // literal separator
            _ => {
                tokens.extend(complete_num(&mut literal, i)?);

                let range = pos(i);
                match c {
                    ' ' | '\n' | '\r' => (), // visual separator
                    '+' => tokens.push(Token::Op(Op::Add(range))),
                    '-' | '−' => tokens.push(Token::Op(Op::Sub(range))),
                    '*' | '×' => tokens.push(Token::Op(Op::Mul(range))),
                    '/' | '÷' => tokens.push(Token::Op(Op::Div(range))),
                    '(' => tokens.push(Token::Par(Par::RoundOpen(range))),
                    '[' => tokens.push(Token::Par(Par::SquareOpen(range))),
                    ')' => tokens.push(Token::Par(Par::RoundClose(range))),
                    ']' => tokens.push(Token::Par(Par::SquareClose(range))),
                    _ => return Err(crate::Error::InvalidCharacter { char: c, range }),
                }
            }
        }
        i += 1;
    }

    tokens.extend(complete_num(&mut literal, i)?);

    Ok(tokens)
}

fn complete_num(literal: &mut String, end: usize) -> crate::Result<Option<Token>> {
    if !literal.is_empty() {
        let start = end - literal.len();
        let val = literal
            .parse::<f64>()
            .map_err(|_| crate::Error::InvalidNumberFormat(range(start, end)))?;

        literal.clear();
        return Ok(Some(Token::Num(num(val, start, end))));
    }
    Ok(None)
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Token {
    Num(Num),
    Op(Op),
    Par(Par),
}

impl Token {
    pub const fn is_op(&self) -> bool {
        matches!(self, Self::Op(_))
    }

    pub const fn is_num(&self) -> bool {
        matches!(self, Self::Num(_))
    }

    pub const fn is_par(&self) -> bool {
        matches!(self, Self::Par(_))
    }

    pub const fn op(&self) -> Option<Op> {
        match self {
            Self::Op(o) => Some(*o),
            _ => None,
        }
    }

    pub const fn num(&self) -> Option<Num> {
        match self {
            Self::Num(n) => Some(*n),
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
            Self::Par(p) => p.range(),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Num {
    pub val: f64,
    pub range: Range,
}

pub const fn num(val: f64, start: usize, end: usize) -> Num {
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
}

impl Op {
    pub const fn priority(&self) -> usize {
        match self {
            Self::Mul(_) | Self::Div(_) => 0,
            Self::Add(_) | Self::Sub(_) => 1,
        }
    }

    pub const fn range(&self) -> Range {
        match *self {
            Self::Mul(r) => r,
            Self::Div(r) => r,
            Self::Add(r) => r,
            Self::Sub(r) => r,
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
}

pub const fn range(start: usize, end: usize) -> Range {
    Range { start, end }
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
                Token::Num(num(432.432, 0, 7)),
                Token::Op(Op::Add(pos(8))),
                Token::Num(num(24324.543, 10, 19)),
            ],
        );
    }

    #[test]
    fn simple_mul() {
        check(
            "604.453 *3562,543",
            vec![
                Token::Num(num(604.453, 0, 7)),
                Token::Op(Op::Mul(pos(8))),
                Token::Num(num(3562.543, 9, 17)),
            ],
        );
    }

    #[test]
    fn add_mul() {
        check(
            "(32+ 604.453)* 3562,543",
            vec![
                Token::Par(Par::RoundOpen(pos(0))),
                Token::Num(num(32.0, 1, 3)),
                Token::Op(Op::Add(pos(3))),
                Token::Num(num(604.453, 5, 12)),
                Token::Par(Par::RoundClose(pos(12))),
                Token::Op(Op::Mul(pos(13))),
                Token::Num(num(3562.543, 15, 23)),
            ],
        );
    }

    fn check(input: &str, output: Vec<Token>) {
        let tokens = tokenize(input).unwrap();
        assert_eq!(tokens, output);
    }
}
