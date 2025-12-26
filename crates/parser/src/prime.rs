use crate::{Parse, ParseResult, Parser, util};
use ::token::{T, token};

pub fn parse_prime<'src>(parser: &mut Parser<'src, '_, '_>) -> ParseResult<'src, ast::Expr> {
    if parser.peek::<T![if]>() {
        let expr = parser.parse_push()?;
        return Ok(ast::Expr::If(expr));
    }
    if parser.peek::<T![while]>() {
        let expr = parser.parse_push()?;
        return Ok(ast::Expr::While(expr));
    }
    if parser.peek::<T![loop]>() {
        let expr = parser.parse_push()?;
        return Ok(ast::Expr::Loop(expr));
    }
    if parser.peek::<T![become]>() {
        let expr = parser.parse_push()?;
        return Ok(ast::Expr::Become(expr));
    }
    if parser.peek::<T![let]>() {
        let expr = parser.parse_push()?;
        return Ok(ast::Expr::Let(expr));
    }
    if parser.peek::<T![return]>() {
        let expr = parser.parse_push()?;
        return Ok(ast::Expr::Return(expr));
    }
    if parser.peek::<T![break]>() {
        let expr = parser.parse_push()?;
        return Ok(ast::Expr::Break(expr));
    }
    if let Some(lit) = parser.eat::<token::Lit>() {
        let lit = parser.push(lit)?;
        return Ok(ast::Expr::Literal(lit));
    }
    if parser.peek::<token::Paren>() {
        return parser.parse_parenthesized(|parser, span| {
            if parser.is_empty() {
                let lit = parser.push(token::Lit::Nil(span))?;
                return Ok(ast::Expr::Literal(lit));
            }

            let res = parser.parse_push()?;
            if !parser.is_empty() {
                return Err(parser.error("expected expression to end"));
            }
            Ok(ast::Expr::Covered(res))
        });
    }
    let parse = parser.parse_push()?;
    Ok(ast::Expr::Symbol(parse))
}

impl<'src> Parse<'src> for ast::If {
    fn parse(parser: &mut Parser<'src, '_, '_>) -> ParseResult<'src, Self> {
        let span = parser.expect::<T![if]>()?.0;
        let condition = parser.parse_push()?;
        let then = parser.parse_push()?;
        let otherwise = if let Some(_) = parser.eat::<T![else]>() {
            Some(parser.parse_push()?)
        } else {
            None
        };

        Ok(ast::If {
            span,
            condition,
            then,
            otherwise,
        })
    }
}

impl<'src> Parse<'src> for ast::While {
    fn parse(parser: &mut Parser<'src, '_, '_>) -> ParseResult<'src, Self> {
        let span = parser.expect::<T![while]>()?.0;
        let condition = parser.parse_push()?;
        let then = parser.parse_push()?;

        Ok(ast::While {
            span,
            condition,
            then,
        })
    }
}

impl<'src> Parse<'src> for ast::Loop {
    fn parse(parser: &mut Parser<'src, '_, '_>) -> ParseResult<'src, Self> {
        let span = parser.expect::<T![loop]>()?.0;
        let body = parser.parse_push()?;

        let span = parser.span_since(span);

        Ok(ast::Loop { span, body })
    }
}

impl<'src> Parse<'src> for ast::Become {
    fn parse(parser: &mut Parser<'src, '_, '_>) -> ParseResult<'src, Self> {
        let span = parser.span();
        parser.expect::<T![become]>()?;
        let callee = parser.expect()?;
        let callee = parser.push_set(callee)?;
        let args = parser.parse_parenthesized(|parser, _| parser.parse_terminated::<_, T![,]>())?;
        Ok(Self { callee, args, span })
    }
}

impl<'src> Parse<'src> for ast::Let {
    fn parse(parser: &mut Parser<'src, '_, '_>) -> ParseResult<'src, Self> {
        let span = parser.expect::<T![let]>()?.0;

        let mutable = parser.eat::<T![mut]>().is_some();

        let ty = if let Some(_) = parser.eat::<T![:]>() {
            Some(parser.parse_push()?)
        } else {
            None
        };

        let sym = parser.parse_push()?;
        parser.expect::<T![=]>()?;
        let expr = parser.parse_push()?;

        Ok(Self {
            sym,
            mutable,
            ty,
            expr,
            span,
        })
    }
}

impl<'src> Parse<'src> for ast::Return {
    fn parse(parser: &mut Parser<'src, '_, '_>) -> ParseResult<'src, Self> {
        let span = parser.expect::<T![return]>()?.0;
        if parser.peek::<T![;]>() || parser.is_empty() {
            return Ok(ast::Return { expr: None, span });
        }

        let expr = parser.parse_push()?;
        Ok(ast::Return {
            expr: Some(expr),
            span,
        })
    }
}

impl<'src> Parse<'src> for ast::Break {
    fn parse(parser: &mut Parser<'src, '_, '_>) -> ParseResult<'src, Self> {
        let span = parser.expect::<T![break]>()?.0;
        if parser.peek::<T![;]>() || parser.is_empty() {
            return Ok(ast::Break { expr: None, span });
        }

        let expr = parser.parse_push()?;
        Ok(ast::Break {
            expr: Some(expr),
            span,
        })
    }
}

impl<'src> Parse<'src> for ast::Symbol {
    fn parse(parser: &mut Parser<'src, '_, '_>) -> ParseResult<'src, Self> {
        let ident: token::Ident = parser.expect()?;
        let span = ident.span().into();
        let ident = parser.push_set(ident)?;
        Ok(ast::Symbol { name: ident, span })
    }
}

impl<'src> Parse<'src> for ast::Block {
    fn parse(parser: &mut Parser<'src, '_, '_>) -> ParseResult<'src, Self> {
        let span = parser.span();

        let mut returns_last = false;

        let body = parser.parse_braced(|parser, _| {
            let mut head = None;
            let mut current = None;
            loop {
                if parser.is_empty() {
                    break;
                }

                let expr = parser.parse_push()?;
                parser.push_list(&mut head, &mut current, expr)?;

                returns_last = true;

                if util::expr_needs_semicolon(parser, expr) {
                    if !parser.is_empty() {
                        parser.expect::<T![;]>()?;
                    }
                } else {
                    returns_last = parser.eat::<T![;]>().is_some();
                }
            }
            Ok(head)
        })?;

        let span = parser.span_since(span);

        Ok(ast::Block {
            body,
            span,
            returns_last,
        })
    }
}
