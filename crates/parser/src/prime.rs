use ast::{NodeId, Spanned};
use syn::{token::Paren, Ident, Lit, Result, Token};

use crate::{Parse, Parser};

pub fn parse_prime(parser: &mut Parser) -> Result<NodeId<ast::Expr>> {
    if parser.peek(Token![if]) {
        let expr = parser.parse()?;
        return parser.push(ast::Expr::If(expr));
    }
    if parser.peek(Token![while]) {
        let expr = parser.parse()?;
        return parser.push(ast::Expr::While(expr));
    }
    if parser.peek(Token![loop]) {
        parser.parse_syn::<Token![loop]>()?;
        let expr = parser.parse()?;
        return parser.push(ast::Expr::Loop(expr));
    }
    if parser.peek(Token![become]) {
        let expr = parser.parse()?;
        return parser.push(ast::Expr::Become(expr));
    }
    if parser.peek(Token![let]) {
        let expr = parser.parse()?;
        return parser.push(ast::Expr::Let(expr));
    }
    if parser.peek(Token![return]) {
        let expr = parser.parse()?;
        return parser.push(ast::Expr::Return(expr));
    }
    if parser.peek(Token![break]) {
        let expr = parser.parse()?;
        return parser.push(ast::Expr::Break(expr));
    }
    if parser.peek(Lit) {
        let expr = parser.parse()?;
        return parser.push(ast::Expr::Literal(expr));
    }
    if parser.peek(Paren) {
        let expr = parser.parse_parenthesized(|parser| {
            let res = parser.parse()?;
            if !parser.is_empty() {
                return Err(parser.error("expected expression to end"));
            }
            Ok(res)
        })?;
        return parser.push(ast::Expr::Covered(expr));
    }
    let parse = parser.parse()?;
    parser.push(ast::Expr::Symbol(parse))
}

impl Parse for ast::If {
    fn parse(parser: &mut Parser) -> Result<NodeId<Self>> {
        let span = parser.parse_syn::<Token![if]>()?.span();
        let condition = parser.parse()?;
        let then = parser.parse()?;
        let otherwise = if parser.peek(Token![else]) {
            parser.parse_syn::<Token![else]>()?;
            Some(parser.parse()?)
        } else {
            None
        };

        parser.push(ast::If {
            span,
            condition,
            then,
            otherwise,
        })
    }
}

impl Parse for ast::While {
    fn parse(parser: &mut Parser) -> Result<NodeId<Self>> {
        let span = parser.parse_syn::<Token![while]>()?.span();
        let condition = parser.parse()?;
        let then = parser.parse()?;

        parser.push(ast::While {
            span,
            condition,
            then,
        })
    }
}

impl Parse for ast::Become {
    fn parse(parser: &mut Parser) -> Result<NodeId<Self>> {
        let span = parser.span();
        parser.parse_syn::<Token![become]>()?;
        let callee = parser.parse_syn_push()?;
        let args =
            parser.parse_parenthesized(|parser| parser.parse_terminated::<_, Token![,]>())?;
        parser.push(Self { callee, args, span })
    }
}

impl Parse for ast::Let {
    fn parse(parser: &mut Parser) -> Result<NodeId<Self>> {
        let span = parser.parse_syn::<Token![let]>()?.span();

        let mutable = if parser.peek(Token![mut]) {
            parser.parse_syn::<Token![mut]>()?;
            true
        } else {
            false
        };

        let ty = if parser.peek(Token![:]) {
            parser.parse_syn::<Token![:]>()?;
            Some(parser.parse()?)
        } else {
            None
        };

        let sym = parser.parse()?;
        parser.parse_syn::<Token![=]>()?;
        let expr = parser.parse()?;

        parser.push(Self {
            sym,
            mutable,
            ty,
            expr,
            span,
        })
    }
}

impl Parse for ast::Return {
    fn parse(parser: &mut Parser) -> Result<NodeId<Self>> {
        let span = parser.parse_syn::<Token![return]>()?.span();
        if parser.peek(Token![;]) || parser.is_empty() {
            return parser.push(ast::Return { expr: None, span });
        }

        let expr = parser.parse()?;
        parser.push(ast::Return {
            expr: Some(expr),
            span,
        })
    }
}

impl Parse for ast::Break {
    fn parse(parser: &mut Parser) -> Result<NodeId<Self>> {
        let span = parser.parse_syn::<Token![break]>()?.span();
        if parser.peek(Token![;]) || parser.is_empty() {
            return parser.push(ast::Break { expr: None, span });
        }

        let expr = parser.parse()?;
        parser.push(ast::Break {
            expr: Some(expr),
            span,
        })
    }
}

impl Parse for ast::Symbol {
    fn parse(parser: &mut Parser) -> Result<NodeId<Self>> {
        let ident: Ident = parser.parse_syn()?;
        let span = Spanned::span(&ident);
        let ident = parser.push(ident)?;
        parser.push(ast::Symbol { name: ident, span })
    }
}

impl Parse for ast::Block {
    fn parse(parser: &mut Parser) -> Result<NodeId<Self>> {
        let span = parser.span();
        let mut returns_last = false;
        let body = parser.parse_braced(|parser| {
            let mut head = None;
            let mut current = None;
            loop {
                if parser.is_empty() {
                    break;
                }

                let expr = parser.parse()?;
                parser.push_list(&mut head, &mut current, expr)?;

                returns_last = true;

                // TODO: Handle semicolons better
                if parser.peek(Token![;]) {
                    returns_last = false;
                    parser.parse_syn::<Token![;]>()?;
                }
            }
            Ok(head)
        })?;

        parser.push(ast::Block {
            body,
            span,
            returns_last,
        })
    }
}

impl Parse for Lit {
    fn parse(parser: &mut Parser) -> Result<NodeId<Self>> {
        let p = parser.parse_syn::<Lit>()?;
        match p {
            Lit::Str(_) | Lit::ByteStr(_) | Lit::CStr(_) | Lit::Byte(_) | Lit::Char(_) => {
                return Err(syn::Error::new(p.span(), "Literal type not yet supported"))
            }
            Lit::Int(ref x) => match x.suffix() {
                "i64" | "u64" | "" => {}
                "isize" | "usize" | "i32" | "u32" | "i16" | "u16" | "i8" | "u8" => {
                    return Err(syn::Error::new(
                        p.span(),
                        format_args!("Invalid integer suffix, {} not yet supported", x.suffix()),
                    ))
                }
                "i128" | "u128" => {
                    return Err(syn::Error::new(
                        p.span(),
                        "Invalid integer suffix, 128 bit integers are not supported",
                    ))
                }
                _ => return Err(syn::Error::new(p.span(), "Invalid integer suffix")),
            },
            Lit::Float(ref f) => match f.suffix() {
                "f64" | "" => {}
                "f32" => {
                    return Err(syn::Error::new(
                        p.span(),
                        format_args!("Invalid integer suffix, {} not yet supported", f.suffix()),
                    ))
                }
                _ => return Err(syn::Error::new(p.span(), "Invalid floating point suffix")),
            },
            Lit::Bool(_) => {}
            Lit::Verbatim(_) => return Err(syn::Error::new(p.span(), "Invalid token")),
            _ => unreachable!(),
        }
        parser.push(p)
    }
}
