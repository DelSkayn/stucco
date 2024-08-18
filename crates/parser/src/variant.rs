use ast::{NodeId, Spanned as _};
use syn::{Ident, Result, Token};

use crate::{Parse, Parser};

impl Parse for ast::Variant {
    fn parse(parser: &mut Parser) -> Result<NodeId<Self>> {
        if parser.peek(Token![const]) {
            let v = parser.parse()?;
            return parser.push(ast::Variant::Constant(v));
        }

        Err(parser.error("expected a variant"))
    }
}

impl Parse for ast::VariantConstant {
    fn parse(parser: &mut Parser) -> Result<NodeId<Self>> {
        let span = parser.parse_syn::<Token![const]>()?.span();
        let name = parser.parse_syn_push::<Ident>()?;
        parser.parse_syn::<Token![:]>()?;
        let ty = parser.parse::<ast::Type>()?;

        parser.push(ast::VariantConstant { name, ty, span })
    }
}
