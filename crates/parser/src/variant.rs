use crate::{Parse, ParseResult, Parser};
use ast::{Variant, Variation, VariationConst, VariationImmediate, VariationSlot};
use token::T;

impl<'src> Parse<'src> for ast::Variant {
    fn parse(parser: &mut Parser<'src, '_, '_>) -> ParseResult<'src, Self> {
        let span = parser.expect::<T![variant]>()?.0;

        let name = parser.expect()?;
        let name = parser.push_set(name)?;

        parser.expect::<T![:]>()?;

        let mut head = None;
        let mut current = None;
        loop {
            let v = if parser.peek::<T![slot]>() {
                let v = parser.parse_push()?;
                Variation::Slot(v)
            } else if parser.peek::<T![imm]>() {
                let v = parser.parse_push()?;
                Variation::Immediate(v)
            } else if parser.peek::<T![const]>() {
                let v = parser.parse_push()?;
                Variation::Const(v)
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
            name,
            span,
            variations: head,
        })
    }
}

impl<'src> Parse<'src> for ast::VariationSlot {
    fn parse(parser: &mut Parser<'src, '_, '_>) -> ParseResult<'src, Self> {
        let span = parser.expect::<T![slot]>()?.0;
        let sym = parser.parse_push()?;
        Ok(VariationSlot { span, sym })
    }
}

impl<'src> Parse<'src> for ast::VariationImmediate {
    fn parse(parser: &mut Parser<'src, '_, '_>) -> ParseResult<'src, Self> {
        let span = parser.expect::<T![imm]>()?.0;
        let sym = parser.parse_push()?;
        Ok(VariationImmediate { span, sym })
    }
}

impl<'src> Parse<'src> for ast::VariationConst {
    fn parse(parser: &mut Parser<'src, '_, '_>) -> ParseResult<'src, Self> {
        let span = parser.expect::<T![const]>()?.0;
        let sym = parser.parse_push()?;
        parser.expect::<T![=]>()?;
        let expr = parser.parse_push()?;
        Ok(VariationConst { span, sym, expr })
    }
}
