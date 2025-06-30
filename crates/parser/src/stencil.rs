use ast::{NodeId, Variant};
use token::T;

use crate::{Parse, Parser, Result};

impl Parse for ast::Stencil {
    fn parse(parser: &mut Parser) -> Result<Self> {
        let span = parser.expect::<T![stencil]>()?.0;
        let sym = parser.parse_push()?;

        let parameters =
            parser.parse_parenthesized(|parser| parser.parse_terminated::<_, T![,]>())?;

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

impl Parse for ast::Parameter {
    fn parse(parser: &mut Parser) -> Result<Self> {
        let span = parser.span();
        let sym = parser.parse_push()?;
        parser.expect::<T![:]>()?;
        let ty = parser.parse_push()?;

        Ok(Self { sym, ty, span })
    }
}
