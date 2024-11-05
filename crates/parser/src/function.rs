use ast::{NodeId, Spanned as _};
use syn::{Ident, Result, Token};

use crate::{kw, Parse, Parser};

impl Parse for ast::StencilFunction {
    fn parse(parser: &mut Parser) -> Result<NodeId<Self>> {
        let entry = if parser.peek(Token![#]) {
            parser.parse_syn::<Token![#]>()?;
            parser.parse_bracketed(|parser| parser.parse_syn::<kw::entry>())?;
            true
        } else {
            false
        };

        let span = parser.parse_syn::<Token![fn]>()?.span();
        let sym = parser.parse()?;

        let mut head = None;
        let mut current = None;
        if parser.peek(Token![<]) {
            parser.parse_syn::<Token![<]>()?;
            loop {
                if parser.peek(Token![>]) {
                    break;
                }

                let variant = parser.parse::<ast::Variant>()?;
                parser.push_list(&mut head, &mut current, variant)?;

                if parser.peek(Token![>]) {
                    parser.parse_syn::<Token![>]>()?;
                    break;
                }

                parser.parse_syn::<Token![,]>()?;
            }
        }

        let parameters =
            parser.parse_parenthesized(|parser| parser.parse_terminated::<_, Token![,]>())?;

        let output = if parser.peek(Token![->]) {
            parser.parse_syn::<Token![->]>()?;
            Some(parser.parse()?)
        } else {
            None
        };

        let body = parser.parse_braced(|parser| {
            let mut head = None;
            let mut current = None;
            loop {
                if parser.is_empty() {
                    break;
                }

                let expr = parser.parse()?;
                parser.push_list(&mut head, &mut current, expr)?;

                // TODO: Handle semicolons better
                if parser.peek(Token![;]) {
                    parser.parse_syn::<Token![;]>()?;
                }
            }
            Ok(head)
        })?;

        parser.push(ast::StencilFunction {
            sym,
            entry,
            parameters,
            span,
            variants: current,
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
