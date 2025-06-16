use ast::{NodeId, Spanned as _, Variant};

use crate::{ParsePush, Parser, Result, T};

impl ParsePush for ast::Stencil {
    fn parse_push(parser: &mut Parser) -> Result<NodeId<Self>> {
        let span = parser.parse::<T![stencil]>()?.0;
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

        parser.push(ast::Stencil {
            sym,
            parameters,
            span,
            variants,
            output,
            body,
        })
    }
}

impl ParsePush for ast::Parameter {
    fn parse_push(parser: &mut Parser) -> Result<NodeId<Self>> {
        let span = parser.span();
        let sym = parser.parse_push()?;
        parser.parse::<T![:]>()?;
        let ty = parser.parse_push()?;

        parser.push(Self { sym, ty, span })
    }
}
