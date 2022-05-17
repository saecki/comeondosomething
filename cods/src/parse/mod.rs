use crate::{Context, Group, IdentSpan, Item, Kw, KwT, OpT, ParKind, PctT, Span};

pub use builtin::*;
pub use cst::Cst;
pub use op::*;
use parser::*;

mod builtin;
pub mod cst;
mod op;
mod parser;
#[cfg(test)]
mod test;

#[derive(Clone, Copy, PartialEq, Eq)]
enum StopOn {
    Nothing,
    LCurly,
    Comma,
}

impl Context {
    pub fn parse(&mut self, items: Vec<Item>) -> crate::Result<Vec<Cst>> {
        let span = items_span(&items).unwrap_or(Span::pos(0, 0));
        self.parse_items(items, span)
    }

    fn parse_items(&mut self, items: Vec<Item>, span: Span) -> crate::Result<Vec<Cst>> {
        let mut asts: Vec<Cst> = Vec::new();

        let mut parser = Parser::new(items, span.start);
        while parser.peek().is_some() {
            let ast = self.parse_bp(&mut parser, 0, StopOn::Nothing)?;
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

    fn parse_bp(&mut self, parser: &mut Parser, min_bp: u8, stop: StopOn) -> crate::Result<Cst> {
        let mut lhs = match parser.peek() {
            Some(Item::Group(_)) => {
                let g = parser.next().unwrap().into_group().unwrap();
                match g.par_kind() {
                    ParKind::Round => {
                        let s = g.span();
                        let mut group_parser = Parser::new(g.items, s.start);
                        let cst = self.parse_bp(&mut group_parser, 0, StopOn::Nothing)?;
                        if let Some(i) = group_parser.next() {
                            return Err(crate::Error::UnexpectedItem(i));
                        }

                        Cst::Par(g.l_par, Box::new(cst), g.r_par)
                    }
                    ParKind::Curly => Cst::Block(self.parse_block(g)?),
                    ParKind::Square => {
                        self.errors.push(crate::Error::NotImplemented(
                            "Arrays are not yet implemented",
                            g.span(),
                        ));
                        return Ok(Cst::Error(g.span()));
                    }
                }
            }
            Some(Item::Val(_)) => {
                let v = parser.next().unwrap().into_val().unwrap();
                Cst::Val(v)
            }
            Some(Item::Ident(_)) => {
                let i = parser.next().unwrap().into_ident().unwrap();
                Cst::Ident(i)
            }
            Some(&Item::Op(o)) => {
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
            Some(&Item::Pct(p)) => match p.typ {
                PctT::Comma | PctT::Colon | PctT::Arrow => {
                    let i = parser.next().unwrap();
                    self.errors.push(crate::Error::UnexpectedItem(i));
                    return Ok(Cst::Error(p.span));
                }
                PctT::Semi | PctT::Newln => {
                    return Ok(Cst::Empty(p.span));
                }
            },
            Some(&Item::Kw(k)) => {
                parser.next();
                self.parse_lang_construct(parser, k)?
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
                Item::Group(g) => {
                    if newln {
                        break;
                    }
                    if stop == StopOn::LCurly && g.par_kind().is_curly() {
                        break;
                    }

                    if let Cst::Ident(id) = lhs {
                        match g.par_kind() {
                            ParKind::Round => {
                                let g = parser.next().unwrap().into_group().unwrap();
                                lhs = self.parse_fun_call(id, g)?;
                                continue;
                            }
                            ParKind::Square => {
                                return Err(crate::Error::NotImplemented(
                                    "Array access is not yet implemented",
                                    g.span(),
                                ));
                            }
                            _ => (),
                        }
                    }

                    let s = Span::between(lhs.span(), g.span());
                    return Err(crate::Error::MissingOperator(s));
                }
                Item::Val(v) => {
                    if newln {
                        break;
                    }

                    let s = Span::between(lhs.span(), v.span);
                    return Err(crate::Error::MissingOperator(s));
                }
                Item::Ident(i) => {
                    if newln {
                        break;
                    }

                    let s = Span::between(lhs.span(), i.span);
                    return Err(crate::Error::MissingOperator(s));
                }
                &Item::Op(o) => o,
                &Item::Pct(p) => {
                    if newln {
                        break;
                    }
                    if p.typ == PctT::Semi {
                        break;
                    }
                    if stop == StopOn::Comma && p.typ == PctT::Comma {
                        break;
                    }

                    let i = parser.next().unwrap();
                    return Err(crate::Error::UnexpectedItem(i));
                }
                Item::Kw(k) => {
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

    fn parse_block(&mut self, g: Group) -> crate::Result<cst::Block> {
        let s = g.span();
        match self.parse_items(g.items, s) {
            Ok(csts) => Ok(cst::Block::new(g.l_par, g.r_par, csts)),
            Err(e) => {
                self.errors.push(e);
                Ok(cst::Block::new(g.l_par, g.r_par, vec![Cst::Error(s)]))
            }
        }
    }

    fn parse_fun_call(&mut self, id: IdentSpan, g: Group) -> crate::Result<Cst> {
        let args = self.parse_fun_args(g)?;
        let f = cst::FunCall::new(id, args);
        Ok(Cst::FunCall(f))
    }

    fn parse_fun_args(&mut self, group: Group) -> crate::Result<cst::FunArgs> {
        let arg_count = group.items.iter().filter(|i| i.is_comma()).count() + 1;
        let mut args = Vec::with_capacity(arg_count);
        let start = group.inner_span().start;
        let mut parser = Parser::new(group.items, start);

        while parser.peek().is_some() {
            let arg = self.parse_bp(&mut parser, 0, StopOn::Comma)?;
            args.push(arg);

            match parser.next() {
                Some(Item::Pct(p)) if p.is_comma() => (),
                Some(i) => return Err(crate::Error::ExpectedPct(PctT::Comma, i.span())),
                None => break,
            }
        }

        Ok(cst::FunArgs::new(group.l_par, group.r_par, args))
    }

    fn parse_lang_construct(&mut self, parser: &mut Parser, kw: Kw) -> crate::Result<Cst> {
        match kw.typ {
            KwT::If => {
                let if_block = {
                    let (cond, block) = self.parse_cond_block(parser)?;
                    cst::IfBlock::new(kw, Box::new(cond), block)
                };

                let mut else_if_blocks = Vec::new();
                let mut else_block = None;
                loop {
                    let else_kw = match parser.peek() {
                        Some(&Item::Kw(k)) if k.typ == KwT::Else => {
                            parser.next();
                            k
                        }
                        _ => break,
                    };

                    match parser.next() {
                        Some(Item::Kw(if_kw)) if if_kw.typ == KwT::If => {
                            let (cond, block) = self.parse_cond_block(parser)?;
                            else_if_blocks.push(cst::ElseIfBlock::new(else_kw, if_kw, cond, block));
                        }
                        Some(Item::Group(g)) if g.par_kind().is_curly() => {
                            let block = self.parse_block(g)?;
                            else_block = Some(cst::ElseBlock::new(else_kw, block));
                            break;
                        }
                        Some(i) => return Err(crate::Error::ExpectedBlock(i.span())),
                        None => {
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
                let (cond, block) = self.parse_cond_block(parser)?;
                let whl_loop = cst::WhileLoop::new(kw, Box::new(cond), block);
                Ok(Cst::WhileLoop(whl_loop))
            }
            KwT::For => {
                let ident = parser.expect_ident()?;
                let in_kw = parser.expect_kw(KwT::In)?;
                let iter = self.parse_bp(parser, 0, StopOn::LCurly)?;
                let group = parser.expect_block()?;
                let block = self.parse_block(group)?;

                let for_loop = cst::ForLoop::new(kw, ident, in_kw, Box::new(iter), block);
                Ok(Cst::ForLoop(for_loop))
            }
            KwT::In => Err(crate::Error::WrongContext(kw)),
            KwT::Fun => {
                let ident = parser.expect_ident()?;

                let param_group = parser.expect_fun_pars()?;
                let params = {
                    let s = param_group.span();
                    let mut group_parser = Parser::new(param_group.items, s.start);
                    let mut params = Vec::new();
                    while let Some(i) = group_parser.next() {
                        let s = i.span();

                        let ident = match i {
                            Item::Ident(id) => id,
                            _ => return Err(crate::Error::ExpectedIdent(s)),
                        };

                        let colon = group_parser.expect_pct(PctT::Colon)?;
                        let typ = group_parser.expect_ident()?;

                        params.push(cst::FunParam::new(ident, colon, typ));

                        match group_parser.next() {
                            Some(i) if i.is_comma() => (),
                            Some(i) => {
                                let s = i.span().before();
                                return Err(crate::Error::ExpectedPct(PctT::Comma, s));
                            }
                            None => break,
                        }
                    }
                    cst::FunParams::new(param_group.l_par, param_group.r_par, params)
                };

                let mut return_type = None;
                if let Some(&Item::Pct(p)) = parser.peek() {
                    if let PctT::Arrow = p.typ {
                        parser.next();
                        let t = parser.expect_ident()?;
                        return_type = Some(cst::ReturnType::new(p, t));
                    }
                }

                let block_group = parser.expect_block()?;
                let block = self.parse_block(block_group)?;

                let fun = cst::FunDef::new(kw, ident, params, return_type, block);
                Ok(Cst::FunDef(fun))
            }
            KwT::Return => {
                parser.eat_newlns();
                let val = if parser.current_newln {
                    None
                } else {
                    let v = self.parse_bp(parser, 0, StopOn::Nothing)?;
                    Some(Box::new(v))
                };
                let r = cst::Return::new(kw, val);
                Ok(Cst::Return(r))
            }
            KwT::Val | KwT::Var => {
                let ident = parser.expect_ident()?;

                let name = self.idents.name(ident.ident);
                if name.parse::<BuiltinConst>().is_ok() {
                    return Err(crate::Error::RedefinedBuiltinConst(
                        name.to_owned(),
                        ident.span,
                    ));
                }

                let mut type_hint = None;
                if let Some(&Item::Pct(p)) = parser.peek() {
                    if let PctT::Colon = p.typ {
                        parser.next();
                        let t = parser.expect_ident()?;
                        type_hint = Some((p, t));
                    }
                }

                let value = {
                    let op = parser.expect_op(OpT::Assign)?;
                    let val = self.parse_bp(parser, 0, StopOn::Nothing)?;
                    (op, Box::new(val))
                };

                let v = cst::VarDef::new(kw, ident, type_hint, value);
                Ok(Cst::VarDef(v))
            }
        }
    }

    fn parse_cond_block(&mut self, parser: &mut Parser) -> crate::Result<(Cst, cst::Block)> {
        let cond = self.parse_bp(parser, 0, StopOn::LCurly)?;
        let group = parser.expect_block()?;
        let block = self.parse_block(group)?;
        Ok((cond, block))
    }
}

fn items_span(items: &[Item]) -> Option<Span> {
    let first = items.first();
    let last = items.last();

    match (first, last) {
        (Some(f), Some(l)) => Some(Span::across(f.span(), l.span())),
        _ => None,
    }
}
