use ast::{NodeId, Spanned as _, Variant};
use syn::{Result, Token};

use crate::{kw, Parse, Parser};

impl Parse for ast::Stencil {
    fn parse(parser: &mut Parser) -> Result<NodeId<Self>> {
        let span = parser.parse_syn::<kw::stencil>()?.span();
        let sym = parser.parse()?;

        let parameters =
            parser.parse_parenthesized(|parser| parser.parse_terminated::<_, Token![,]>())?;

        let output = if parser.peek(Token![->]) {
            parser.parse_syn::<Token![->]>()?;
            Some(parser.parse()?)
        } else {
            None
        };

        let mut head = None;
        let mut current = None;
        while parser.peek(kw::variant) {
            let item = parser.parse::<Variant>()?;
            parser.push_list(&mut head, &mut current, item)?;
        }
        let variants = head;

        let body = parser.parse()?;
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

impl Parse for ast::Parameter {
    fn parse(parser: &mut Parser) -> Result<NodeId<Self>> {
        let span = parser.span();
        let sym = parser.parse()?;
        parser.parse_syn::<Token![:]>()?;
        let ty = parser.parse()?;

        parser.push(Self { sym, ty, span })
    }
}
