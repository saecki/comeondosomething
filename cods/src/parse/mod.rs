use crate::{
    BuiltinConst, BuiltinFun, Context, IdentSpan, Kw, KwT, OpT, ParKind, ParT, PctT, Span, Token,
};

pub use cst::Cst;
pub use op::*;
use parser::*;

pub mod cst;
mod op;
mod parser;
#[cfg(test)]
mod test;

#[derive(Clone, Copy, PartialEq, Eq)]
enum StopOn {
    Nothing = 1,
    LCurly = 2,
    RCurly = 4,
    RRound = 8,
    Comma = 16,
}

impl Context {
    pub fn parse(&mut self, tokens: Vec<Token>) -> crate::Result<Vec<Cst>> {
        let span = tokens_span(&tokens).unwrap_or(Span::pos(0, 0));
        self.parse_tokens(tokens, span)
    }

    fn parse_tokens(&mut self, tokens: Vec<Token>, span: Span) -> crate::Result<Vec<Cst>> {
        let mut asts: Vec<Cst> = Vec::new();

        let mut parser = Parser::new(tokens, span.start);
        while parser.peek().is_some() {
            let ast = self.parse_bp(&mut parser, 0, StopOn::Nothing as u16)?;
            parser.eat_semi();

            if !ast.is_empty() {
                asts.push(ast);
            }
        }

        if asts.is_empty() {
            Ok(vec![Cst::Empty(span)])
        } else {
            Ok(asts)
        }
    }

    fn parse_bp(&mut self, parser: &mut Parser, min_bp: u8, stop: u16) -> crate::Result<Cst> {
        let mut lhs = match parser.peek() {
            Some(Token::Par(p)) => match p.typ {
                crate::ParT::LRound => todo!("parse one expr"),
                crate::ParT::RRound => todo!("error unexpected"),
                crate::ParT::LSquare => todo!("error not implemented"),
                crate::ParT::RSquare => todo!("error unexpected"),
                crate::ParT::LCurly => todo!("parse block"),
                crate::ParT::RCurly => todo!("error unexpected"),
                //
                // let g = parser.next().unwrap().into_group().unwrap();
                // match g.par_kind() {
                //     ParKind::Round => {
                //         let s = g.span();
                //         let mut group_parser = Parser::new(g.items, s.start);
                //         let cst = self.parse_bp(&mut group_parser, 0, StopOn::Nothing)?;
                //         if let Some(i) = group_parser.next() {
                //             return Err(crate::Error::UnexpectedItem(i));
                //         }
                //
                //         Cst::Par(g.l_par, Box::new(cst), g.r_par)
                //     }
                //     ParKind::Curly => Cst::Block(self.parse_block(g)?),
                //     ParKind::Square => {
                //         self.errors.push(crate::Error::NotImplemented(
                //             "Arrays are not yet implemented",
                //             vec![g.span()],
                //         ));
                //         return Ok(Cst::Error(g.span()));
                //     }
                // }
            },
            Some(Token::Val(_)) => {
                let v = parser.next().unwrap().into_val().unwrap();
                Cst::Val(v)
            }
            Some(Token::Ident(_)) => {
                let i = parser.next().unwrap().into_ident().unwrap();
                Cst::Ident(i)
            }
            Some(&Token::Op(o)) => {
                parser.next();

                let (prefix, r_bp) = match o.prefix_bp() {
                    Some(p) => p,
                    None => return Err(crate::Error::UnexpectedOperator(o)),
                };

                if parser.peek().is_none() {
                    let s = o.span.after();
                    return Err(crate::Error::MissingOperand(s));
                }

                let val = self.parse_bp(parser, r_bp, stop)?;
                let p = Prefix::new(prefix, o.span);

                Cst::Prefix(p, Box::new(val))
            }
            Some(&Token::Pct(p)) => match p.typ {
                PctT::Comma | PctT::Colon | PctT::Arrow => {
                    let i = parser.next().unwrap();
                    self.errors.push(crate::Error::UnexpectedToken(i));
                    return Ok(Cst::Error(p.span));
                }
                PctT::Semi | PctT::Newln => {
                    return Ok(Cst::Empty(p.span));
                }
            },
            Some(&Token::Kw(k)) => {
                parser.next();
                self.parse_lang_construct(parser, k, stop)?
            }
            None => return Ok(Cst::Empty(Span::from(parser.pos))),
        };

        loop {
            parser.eat_newlns();
            let newln = parser.current_newln;
            let i = match parser.peek() {
                Some(i) => i,
                None => break,
            };

            let op = match i {
                Token::Par(p) => {
                    if newln {
                        break;
                    }
                    if stop & StopOn::LCurly as u16 != 0 && p.typ == ParT::LCurly {
                        break;
                    }
                    if stop & StopOn::RCurly as u16 != 0 && p.typ == ParT::RCurly {
                        break;
                    }
                    if stop & StopOn::RRound as u16 != 0 && p.typ == ParT::RRound {
                        break;
                    }

                    if let Cst::Ident(id) = lhs {
                        match p.kind() {
                            ParKind::Round => {
                                parser.next();
                                lhs = self.parse_fun_call(id, parser)?;
                                continue;
                            }
                            ParKind::Square => {
                                todo!("find closing par");
                                //return Err(crate::Error::NotImplemented(
                                //    "Array access is not yet implemented",
                                //    vec![g.span()],
                                //));
                            }
                            _ => (),
                        }
                    }

                    todo!("find closing par");
                    //let s = Span::between(lhs.span(), g.span());
                    //return Err(crate::Error::MissingOperator(s));
                }
                Token::Val(v) => {
                    if newln {
                        break;
                    }

                    let s = Span::between(lhs.span(), v.span);
                    return Err(crate::Error::MissingOperator(s));
                }
                Token::Ident(i) => {
                    if newln {
                        break;
                    }

                    let s = Span::between(lhs.span(), i.span);
                    return Err(crate::Error::MissingOperator(s));
                }
                &Token::Op(o) => o,
                &Token::Pct(p) => {
                    if newln {
                        break;
                    }
                    if p.typ == PctT::Semi {
                        break;
                    }
                    if stop & StopOn::Comma as u16 != 0 && p.typ == PctT::Comma {
                        break;
                    }

                    let i = parser.next().unwrap();
                    return Err(crate::Error::UnexpectedToken(i));
                }
                Token::Kw(k) => {
                    if newln {
                        break;
                    }

                    let s = Span::between(lhs.span(), k.span);
                    return Err(crate::Error::MissingOperator(s));
                }
            };

            if let Some((l_bp, postfix)) = op.postfix_bp() {
                if l_bp < min_bp {
                    break;
                }
                parser.next();

                let p = Postfix::new(postfix, op.span);
                lhs = Cst::Postfix(Box::new(lhs), p);

                continue;
            }

            let (l_bp, infix, r_bp) = match op.infix_bp() {
                Some(bp) => bp,
                None => {
                    let s = Span::between(lhs.span(), op.span);
                    return Err(crate::Error::MissingOperator(s));
                }
            };
            if l_bp < min_bp {
                break;
            } else {
                parser.next();
            }

            let rhs = self.parse_bp(parser, r_bp, stop)?;

            if let Cst::Empty(s) = rhs {
                return Err(crate::Error::MissingOperand(Span::between(op.span, s)));
            }

            let i = Infix::new(infix, op.span);
            lhs = Cst::Infix(Box::new(lhs), i, Box::new(rhs));
        }

        Ok(lhs)
    }

    fn parse_fun_call(&mut self, id: IdentSpan, parser: &mut Parser) -> crate::Result<Cst> {
        let args = self.parse_fun_args(parser)?;
        let f = cst::FunCall::new(id, args);
        Ok(Cst::FunCall(f))
    }

    fn parse_fun_args(&mut self, parser: &mut Parser) -> crate::Result<cst::FunArgs> {
        let l_par = parser.expect_par(ParT::LRound)?;
        let mut args = Vec::new();
        while parser.peek().is_some() {
            let stop = StopOn::Comma as u16 | StopOn::RRound as u16;
            let arg = self.parse_bp(parser, 0, stop)?;
            args.push(arg);

            match parser.next() {
                Some(Token::Par(r_par)) if r_par.typ == ParT::RRound => {
                    return Ok(cst::FunArgs::new(l_par, r_par, args));
                }
                Some(Token::Pct(p)) if p.is_comma() => (),
                Some(i) => return Err(crate::Error::ExpectedPct(PctT::Comma, i.span())),
                None => break,
            }
        }

        Err(crate::Error::MissingClosingPar(l_par))
    }

    fn parse_lang_construct(
        &mut self,
        parser: &mut Parser,
        kw: Kw,
        stop: u16,
    ) -> crate::Result<Cst> {
        match kw.typ {
            KwT::If => {
                let if_block = {
                    let cond = self.parse_bp(parser, 0, StopOn::LCurly as u16)?;
                    let block = self.parse_block(parser)?;
                    cst::IfBlock::new(kw, Box::new(cond), block)
                };

                let mut else_if_blocks = Vec::new();
                let mut else_block = None;
                loop {
                    let else_kw = match parser.peek() {
                        Some(&Token::Kw(k)) if k.typ == KwT::Else => {
                            parser.next();
                            k
                        }
                        _ => break,
                    };

                    match parser.peek() {
                        Some(&Token::Kw(if_kw)) if if_kw.typ == KwT::If => {
                            parser.next();
                            let cond = self.parse_bp(parser, 0, StopOn::LCurly as u16)?;
                            let block = self.parse_block(parser)?;
                            else_if_blocks.push(cst::ElseIfBlock::new(else_kw, if_kw, cond, block));
                        }
                        Some(Token::Par(p)) if p.typ == ParT::LCurly => {
                            let block = self.parse_block(parser)?;
                            else_block = Some(cst::ElseBlock::new(else_kw, block));
                            break;
                        }
                        Some(_) | None => {
                            let s = kw.span.after();
                            return Err(crate::Error::ExpectedBlock(s));
                        }
                    }
                }

                let if_expr = cst::IfExpr::new(if_block, else_if_blocks, else_block);
                Ok(Cst::IfExpr(if_expr))
            }
            KwT::Else => Err(crate::Error::WrongContext(kw)),
            KwT::While => {
                let cond = self.parse_bp(parser, 0, StopOn::LCurly as u16)?;
                let block = self.parse_block(parser)?;
                let whl_loop = cst::WhileLoop::new(kw, Box::new(cond), block);
                Ok(Cst::WhileLoop(whl_loop))
            }
            KwT::For => {
                let ident = parser.expect_ident()?;
                let in_kw = parser.expect_kw(KwT::In)?;
                let iter = self.parse_bp(parser, 0, StopOn::LCurly as u16)?;
                let block = self.parse_block(parser)?;

                let for_loop = cst::ForLoop::new(kw, ident, in_kw, Box::new(iter), block);
                Ok(Cst::ForLoop(for_loop))
            }
            KwT::In => Err(crate::Error::WrongContext(kw)),
            KwT::Fun => {
                let ident = parser.expect_ident()?;

                let name = self.idents.name(ident.ident);
                if name.parse::<BuiltinFun>().is_ok() {
                    self.errors.push(crate::Error::RedefinedBuiltinFun(
                        name.to_owned(),
                        ident.span,
                    ));
                }

                let l_par = parser.expect_par(ParT::LRound)?;
                let mut params = Vec::new();
                let params = loop {
                    if let Some(t) = parser.next() {
                        let s = t.span();

                        let ident = match t {
                            Token::Ident(id) => id,
                            _ => return Err(crate::Error::ExpectedIdent(s)),
                        };

                        let colon = parser.expect_pct(PctT::Colon)?;
                        let data_type = parser.expect_ident()?;

                        params.push(cst::FunParam::new(ident, colon, data_type));

                        match parser.next() {
                            Some(Token::Par(r_par)) if r_par.typ == ParT::RRound => {
                                break cst::FunParams::new(l_par, r_par, params);
                            }
                            Some(Token::Pct(p)) if p.is_comma() => (),
                            Some(t) => {
                                let s = t.span().before();
                                return Err(crate::Error::ExpectedPct(PctT::Comma, s));
                            }
                            None => return Err(crate::Error::MissingClosingPar(l_par)),
                        }
                    } else {
                        return Err(crate::Error::MissingClosingPar(l_par));
                    }
                };

                let mut return_type = None;
                if let Some(&Token::Pct(p)) = parser.peek() {
                    if let PctT::Arrow = p.typ {
                        parser.next();
                        let t = parser.expect_ident()?;
                        return_type = Some(cst::ReturnType::new(p, t));
                    }
                }

                let block = self.parse_block(parser)?;

                let fun = cst::FunDef::new(kw, ident, params, return_type, block);
                Ok(Cst::FunDef(fun))
            }
            KwT::Return => {
                parser.eat_newlns();
                let val = if parser.current_newln {
                    None
                } else {
                    let v = self.parse_bp(parser, 0, stop)?;
                    Some(Box::new(v))
                };
                let r = cst::Return::new(kw, val);
                Ok(Cst::Return(r))
            }
            KwT::Val | KwT::Var => {
                let ident = parser.expect_ident()?;

                let name = self.idents.name(ident.ident);
                if name.parse::<BuiltinConst>().is_ok() {
                    self.errors.push(crate::Error::RedefinedBuiltinConst(
                        name.to_owned(),
                        ident.span,
                    ));
                }

                let mut type_hint = None;
                if let Some(&Token::Pct(p)) = parser.peek() {
                    if let PctT::Colon = p.typ {
                        parser.next();
                        let t = parser.expect_ident()?;
                        type_hint = Some((p, t));
                    }
                }

                let value = {
                    let op = parser.expect_op(OpT::Assign)?;
                    let val = self.parse_bp(parser, 0, stop)?;
                    (op, Box::new(val))
                };

                let v = cst::VarDef::new(kw, ident, type_hint, value);
                Ok(Cst::VarDef(v))
            }
        }
    }

    fn parse_block(&mut self, parser: &mut Parser) -> crate::Result<cst::Block> {
        let l_par = parser.expect_par(ParT::LCurly)?;
        let mut csts = Vec::new();
        while let Some(t) = parser.next() {
            match t {
                Token::Par(r_par) if r_par.is_closing() => {
                    if l_par.matches(r_par.typ) {
                        let r_par = parser.expect_par(ParT::RCurly)?;
                        return Ok(cst::Block::new(l_par, r_par, csts));
                    } else {
                        todo!("error");
                    }
                }
                t => {
                    let cst = match self.parse_bp(parser, 0, StopOn::RCurly as u16) {
                        Ok(cst) => cst,
                        Err(e) => {
                            self.errors.push(e);
                            Cst::Error(t.span())
                        }
                    };
                    csts.push(cst);
                }
            }
        }

        Err(crate::Error::MissingClosingPar(l_par))
    }
}

fn tokens_span(tokens: &[Token]) -> Option<Span> {
    let first = tokens.first();
    let last = tokens.last();

    match (first, last) {
        (Some(f), Some(l)) => Some(Span::across(f.span(), l.span())),
        _ => None,
    }
}
