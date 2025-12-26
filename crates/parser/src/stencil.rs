use ast::Variant;
use token::T;

use crate::{Parse, ParseResult, Parser};

impl<'src> Parse<'src> for ast::Stencil {
    fn parse(parser: &mut Parser<'src, '_, '_>) -> ParseResult<'src, Self> {
        let span = parser.expect::<T![stencil]>()?.0;
        let sym = parser.parse_push()?;

        let parameters =
            parser.parse_parenthesized(|parser, _| parser.parse_terminated::<_, T![,]>())?;

        let output = if let Some(_) = parser.eat::<T![->]>() {
            Some(parser.parse_push()?)
        } else {
            None
        };

        let mut head = None;
        let mut current = None;
        while parser.peek::<T![variant]>() {
            let item = parser.parse_push::<Variant>()?;
            parser.push_list(&mut head, &mut current, item)?;
        }
        let variants = head;

        let body = parser.parse_push()?;
        let body = parser.push(ast::Expr::Block(body))?;
        let span = parser.span_since(span);

        Ok(ast::Stencil {
            sym,
            parameters,
            span,
            variants,
            output,
            body,
        })
    }
}

impl<'src> Parse<'src> for ast::Function {
    fn parse(parser: &mut Parser<'src, '_, '_>) -> ParseResult<'src, Self> {
        let span = parser.expect::<T![fn]>()?.0;
        let sym = parser.parse_push()?;

        let parameters =
            parser.parse_parenthesized(|parser, _| parser.parse_terminated::<_, T![,]>())?;

        let output = if let Some(_) = parser.eat::<T![->]>() {
            Some(parser.parse_push()?)
        } else {
            None
        };

        let body = parser.parse_push()?;
        let body = parser.push(ast::Expr::Block(body))?;
        let span = parser.span_since(span);

        Ok(ast::Function {
            sym,
            parameters,
            span,
            output,
            body,
        })
    }
}

impl<'src> Parse<'src> for ast::Parameter {
    fn parse(parser: &mut Parser<'src, '_, '_>) -> ParseResult<'src, Self> {
        let span = parser.span();
        let sym = parser.parse_push()?;
        parser.expect::<T![:]>()?;
        let ty = parser.parse_push()?;

        Ok(Self { sym, ty, span })
    }
}
