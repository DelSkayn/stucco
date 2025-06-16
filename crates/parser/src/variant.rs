use crate::{ParsePush, Parser, Result, T};
use ast::{NodeId, Variant, Variation, VariationImmediate, VariationSlot};

impl ParsePush for ast::Variant {
    fn parse_push(parser: &mut Parser) -> Result<NodeId<Self>> {
        let span = parser.parse::<T![variant]>()?.0;
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

        parser.push(Variant {
            span,
            variations: head,
        })
    }
}

impl ParsePush for ast::VariationSlot {
    fn parse_push(parser: &mut Parser) -> Result<NodeId<Self>> {
        let span = parser.parse::<T![slot]>()?.0;
        let sym = parser.parse_push()?;
        parser.push(VariationSlot { span, sym })
    }
}

impl ParsePush for ast::VariationImmediate {
    fn parse_push(parser: &mut Parser) -> Result<NodeId<Self>> {
        let span = parser.parse::<T![imm]>()?.0;
        let sym = parser.parse_push()?;
        parser.push(VariationImmediate { span, sym })
    }
}
