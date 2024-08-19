use ast::{NodeId, Spanned as _};
use syn::{token, Result, Token};

use crate::{Parse, Parser};

impl Parse for ast::Type {
    fn parse(parser: &mut Parser) -> Result<NodeId<Self>> {
        if parser.peek(Token![*]) {
            let v = ast::Type::Ptr(parser.parse()?);
            return parser.push(v);
        }

        if parser.peek(Token![fn]) {
            let v = ast::Type::Fn(parser.parse()?);
            return parser.push(v);
        }

        if parser.peek(Token![&]) {
            let v = ast::Type::Reference(parser.parse()?);
            return parser.push(v);
        }

        if parser.peek(token::Paren) {
            let v = ast::Type::Tuple(parser.parse()?);
            return parser.push(v);
        }

        if parser.peek(token::Bracket) {
            let v = ast::Type::Array(parser.parse()?);
            return parser.push(v);
        }

        let name = parser.parse_syn_push()?;
        parser.push(ast::Type::Direct(name))
    }
}

impl Parse for ast::TypeFn {
    fn parse(parser: &mut Parser) -> Result<NodeId<Self>> {
        let span = parser.parse_syn::<Token![fn]>()?.span();
        let params =
            parser.parse_parenthesized(|parser| parser.parse_terminated::<_, Token![,]>())?;

        let output = if parser.peek(Token![->]) {
            parser.parse_syn::<Token![->]>()?;
            Some(parser.parse()?)
        } else {
            None
        };

        parser.push(ast::TypeFn {
            params,
            output,
            span,
        })
    }
}

impl Parse for ast::TypePtr {
    fn parse(parser: &mut Parser) -> Result<NodeId<Self>> {
        let span = parser.parse_syn::<Token![*]>()?.span();
        let mutable = if parser.peek(Token![mut]) {
            parser.parse_syn::<Token![mut]>()?;
            true
        } else if parser.peek(Token![const]) {
            parser.parse_syn::<Token![const]>()?;
            false
        } else {
            return Err(parser.error("expected either `mut` or `const`"));
        };
        let ty = parser.parse()?;

        parser.push(ast::TypePtr { mutable, span, ty })
    }
}

impl Parse for ast::TypeReference {
    fn parse(parser: &mut Parser) -> Result<NodeId<Self>> {
        let span = parser.parse_syn::<Token![*]>()?.span();
        let mutable = if parser.peek(Token![mut]) {
            parser.parse_syn::<Token![mut]>()?;
            true
        } else {
            false
        };
        let ty = parser.parse()?;
        parser.push(ast::TypeReference { mutable, span, ty })
    }
}

impl Parse for ast::TypeArray {
    fn parse(parser: &mut Parser) -> Result<NodeId<Self>> {
        let span = parser.span();
        parser.parse_bracketed(|parser| {
            let ty = parser.parse()?;
            parser.parse_syn::<Token![;]>()?;
            let len = parser.parse()?;
            parser.push(ast::TypeArray {
                span,
                elem: ty,
                len,
            })
        })
    }
}

impl Parse for ast::TypeTuple {
    fn parse(parser: &mut Parser) -> Result<NodeId<Self>> {
        let span = parser.span();
        let fields =
            parser.parse_parenthesized(|parser| parser.parse_terminated::<_, Token![,]>())?;
        parser.push(ast::TypeTuple { fields, span })
    }
}
