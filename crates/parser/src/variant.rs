use crate::{Parse, Parser, Result};
use ast::{Variant, Variation, VariationImmediate, VariationSlot};
use token::T;

impl Parse for ast::Variant {
    fn parse(parser: &mut Parser) -> Result<Self> {
        let span = parser.expect::<T![variant]>()?.0;
        let mut head = None;
        let mut current = None;
        loop {
            let v = if parser.peek::<T![slot]>() {
                let v = parser.parse_push()?;
                Variation::Slot(v)
            } else if parser.peek::<T![imm]>() {
                let v = parser.parse_push()?;
                Variation::Immediate(v)
            } else {
                break;
            };

            let n = parser.push(v)?;
            parser.push_list(&mut head, &mut current, n)?;

            let Some(_) = parser.eat::<T![,]>() else {
                break;
            };
        }

        Ok(Variant {
            span,
            variations: head,
        })
    }
}

impl Parse for ast::VariationSlot {
    fn parse(parser: &mut Parser) -> Result<Self> {
        let span = parser.expect::<T![slot]>()?.0;
        let sym = parser.parse_push()?;
        Ok(VariationSlot { span, sym })
    }
}

impl Parse for ast::VariationImmediate {
    fn parse(parser: &mut Parser) -> Result<Self> {
        let span = parser.expect::<T![imm]>()?.0;
        let sym = parser.parse_push()?;
        Ok(VariationImmediate { span, sym })
    }
}
