use ast::{NodeId, Spanned as _, Variant, Variation, VariationImmediate, VariationSlot};
use syn::{Result, Token};

use crate::{Parse, Parser, kw};

impl Parse for ast::Variant {
    fn parse(parser: &mut Parser) -> Result<NodeId<Self>> {
        let span = parser.parse_syn::<kw::variant>()?.span();
        let mut head = None;
        let mut current = None;
        loop {
            let v = if parser.peek(kw::slot) {
                let v = parser.parse()?;
                Variation::Slot(v)
            } else if parser.peek(Token![const]) {
                let v = parser.parse()?;
                Variation::Immediate(v)
            } else {
                break;
            };

            let n = parser.push(v)?;
            parser.push_list(&mut head, &mut current, n)?;

            if !parser.peek(Token![,]) {
                break;
            }
            parser.parse_syn::<Token![,]>()?;
        }

        parser.push(Variant {
            span,
            variations: head,
        })
    }
}

impl Parse for ast::VariationSlot {
    fn parse(parser: &mut Parser) -> Result<NodeId<Self>> {
        let span = parser.parse_syn::<kw::slot>()?.span();
        let sym = parser.parse()?;
        parser.push(VariationSlot { span, sym })
    }
}

impl Parse for ast::VariationImmediate {
    fn parse(parser: &mut Parser) -> Result<NodeId<Self>> {
        let span = parser.parse_syn::<kw::imm>()?.span();
        let sym = parser.parse()?;
        parser.push(VariationImmediate { span, sym })
    }
}
