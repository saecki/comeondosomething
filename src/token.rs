#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Token {
    Num(Num),
    Op(Op),
    Par(Par),
}

impl Token {
    pub const fn is_op(&self) -> bool {
        match self {
            Self::Op(_) => true,
            _ => false,
        }
    }

    pub const fn is_num(&self) -> bool {
        match self {
            Self::Num(_) => true,
            _ => false,
        }
    }

    pub const fn is_par(&self) -> bool {
        match self {
            Self::Par(_) => true,
            _ => false,
        }
    }

    pub const fn op(&self) -> Option<Op> {
        match self {
            &Self::Op(o) => Some(o),
            _ => None,
        }
    }

    pub const fn num(&self) -> Option<Num> {
        match self {
            &Self::Num(n) => Some(n),
            _ => None,
        }
    }

    pub const fn par(&self) -> Option<Par> {
        match self {
            &Self::Par(p) => Some(p),
            _ => None,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Num {
    pub val: f64,
    /// including
    pub start: usize,
    /// excluding
    pub end: usize,
}

pub fn num(val: f64, start: usize, end: usize) -> Num {
    Num { val, start, end }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Op {
    Add(usize),
    Sub(usize),
    Mul(usize),
    Div(usize),
}

impl Op {
    pub const fn priority(&self) -> usize {
        match self {
            Self::Mul(_) | Self::Div(_) => 0,
            Self::Add(_) | Self::Sub(_) => 1,
        }
    }

    pub const fn pos(&self) -> usize {
        match self {
            &Self::Mul(p) => p,
            &Self::Div(p) => p,
            &Self::Add(p) => p,
            &Self::Sub(p) => p,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Par {
    RoundOpen(usize),
    RoundClose(usize),
    SquareOpen(usize),
    SquareClose(usize),
}

impl Par {
    pub fn is_opening(&self) -> bool {
        match self {
            Self::SquareOpen(_) | Self::RoundOpen(_) => true,
            Self::SquareClose(_) | Self::RoundClose(_) => false,
        }
    }

    pub fn matches(&self, other: Self) -> bool {
        match self {
            Self::RoundOpen(_) => matches!(other, Par::RoundClose(_)),
            Self::RoundClose(_) => matches!(other, Par::RoundOpen(_)),
            Self::SquareOpen(_) => matches!(other, Par::SquareClose(_)),
            Self::SquareClose(_) => matches!(other, Par::SquareOpen(_)),
        }
    }

    pub fn pos(&self) -> usize {
        match self {
            &Self::RoundOpen(p) => p,
            &Self::RoundClose(p) => p,
            &Self::SquareOpen(p) => p,
            &Self::SquareClose(p) => p,
        }
    }
}

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

                match c {
                    ' ' | '\n' => (), // visual separator
                    '+' => tokens.push(Token::Op(Op::Add(i))),
                    '-' | '−' => tokens.push(Token::Op(Op::Sub(i))),
                    '*' | '×' => tokens.push(Token::Op(Op::Mul(i))),
                    '/' | '÷' => tokens.push(Token::Op(Op::Div(i))),
                    '(' => tokens.push(Token::Par(Par::RoundOpen(i))),
                    '[' => tokens.push(Token::Par(Par::SquareOpen(i))),
                    ')' => tokens.push(Token::Par(Par::RoundClose(i))),
                    ']' => tokens.push(Token::Par(Par::SquareClose(i))),
                    _ => return Err(crate::Error::InvalidCharacter { char: c, pos: i }),
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
            .map_err(|_| crate::Error::NumberFormatException { start, end })?;

        literal.clear();
        return Ok(Some(Token::Num(num(val, start, end))));
    }
    Ok(None)
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
                Token::Op(Op::Add(8)),
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
                Token::Op(Op::Mul(8)),
                Token::Num(num(3562.543, 9, 17)),
            ],
        );
    }

    #[test]
    fn add_mul() {
        check(
            "(32+ 604.453)* 3562,543",
            vec![
                Token::Par(Par::RoundOpen(0)),
                Token::Num(num(32.0, 1, 3)),
                Token::Op(Op::Add(3)),
                Token::Num(num(604.453, 5, 12)),
                Token::Par(Par::RoundClose(12)),
                Token::Op(Op::Mul(13)),
                Token::Num(num(3562.543, 15, 23)),
            ],
        );
    }

    fn check(input: &str, output: Vec<Token>) {
        let tokens = tokenize(input).unwrap();
        assert_eq!(tokens, output);
    }
}
