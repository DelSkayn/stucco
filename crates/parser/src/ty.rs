use crate::{ParsePush, Parser, Result, T, token};
use ast::{NodeId, Spanned as _};

impl ParsePush for ast::Type {
    fn parse_push(parser: &mut Parser) -> Result<NodeId<Self>> {
        if parser.peek::<T![*]>() {
            let v = ast::Type::Ptr(parser.parse_push()?);
            return parser.push(v);
        }

        if parser.peek::<T![fn]>() {
            let v = ast::Type::Fn(parser.parse_push()?);
            return parser.push(v);
        }

        if parser.peek::<T![&]>() {
            let v = ast::Type::Reference(parser.parse_push()?);
            return parser.push(v);
        }

        if parser.peek::<token::Paren>() {
            let v = ast::Type::Tuple(parser.parse_push()?);
            return parser.push(v);
        }

        if parser.peek::<token::Bracket>() {
            let v = ast::Type::Array(parser.parse_push()?);
            return parser.push(v);
        }

        let name = parser.parse_push()?;
        parser.push(ast::Type::Direct(name))
    }
}

impl ParsePush for ast::TypeFn {
    fn parse_push(parser: &mut Parser) -> Result<NodeId<Self>> {
        let span = parser.parse::<T![fn]>()?.0;
        let params = parser.parse_parenthesized(|parser| parser.parse_terminated::<_, T![,]>())?;

        let output = if let Some(_) = parser.eat::<T![->]>() {
            Some(parser.parse_push()?)
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

impl ParsePush for ast::TypePtr {
    fn parse_push(parser: &mut Parser) -> Result<NodeId<Self>> {
        let span = parser.parse::<T![*]>()?.0;
        let mutable = if let Some(_) = parser.eat::<T![mut]>() {
            true
        } else if let Some(_) = parser.eat::<T![const]>() {
            false
        } else {
            return Err(parser.error("expected either `mut` or `const`"));
        };
        let ty = parser.parse_push()?;

        parser.push(ast::TypePtr { mutable, span, ty })
    }
}

impl ParsePush for ast::TypeReference {
    fn parse_push(parser: &mut Parser) -> Result<NodeId<Self>> {
        let span = parser.parse::<T![*]>()?.0;
        let mutable = parser.eat::<T![mut]>().is_some();
        let ty = parser.parse_push()?;
        parser.push(ast::TypeReference { mutable, span, ty })
    }
}

impl ParsePush for ast::TypeArray {
    fn parse_push(parser: &mut Parser) -> Result<NodeId<Self>> {
        let span = parser.span();
        parser.parse_bracketed(|parser| {
            let ty = parser.parse_push()?;
            parser.parse::<T![;]>()?;
            let len = parser.parse_push()?;
            parser.push(ast::TypeArray {
                span,
                elem: ty,
                len,
            })
        })
    }
}

impl ParsePush for ast::TypeTuple {
    fn parse_push(parser: &mut Parser) -> Result<NodeId<Self>> {
        let span = parser.span();
        let fields = parser.parse_parenthesized(|parser| parser.parse_terminated::<_, T![,]>())?;
        parser.push(ast::TypeTuple { fields, span })
    }
}
