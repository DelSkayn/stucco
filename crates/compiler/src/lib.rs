#![allow(dead_code)]

use ast::{Ast, NodeId};
use std::{error, fmt};
use token::Span;
use type_check::TypeError;

pub mod resolve;
pub mod type_check;

#[derive(Debug)]
pub enum Error {
    UndeclaredSymbol { symbol: Span },
    RedeclaredFunction(Span),
    RedeclaredVariant(Span),
    RedeclaredParameter { redecl: Span, original: Span },
    RedeclaredVariation(Span),
    VariationMissingParameter { variation: Span, parameter: Span },
    Type(TypeError),
    PushNode,
}

impl From<TypeError> for Error {
    fn from(value: TypeError) -> Self {
        Error::Type(value)
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::UndeclaredSymbol { .. } => {
                write!(f, "Undeclared symbol")
            }
            Error::RedeclaredFunction(_) => {
                write!(f, "Function redeclared")
            }
            Error::RedeclaredVariant(_) => {
                write!(f, "Variant redeclared")
            }
            Error::RedeclaredParameter { .. } => {
                write!(f, "Function parameter redeclared")
            }
            Error::Type(e) => {
                write!(f, "Type error: {:?}", e)
            }
            Error::PushNode => {
                write!(f, "Exceeded node limits")
            }
            Error::VariationMissingParameter { .. } => {
                write!(f, "Variation did not contain all parameters")
            }
            Error::RedeclaredVariation(_) => {
                write!(f, "Variation declared twice")
            }
        }
    }
}
impl error::Error for Error {}

impl Error {
    pub fn render(&self, source: &str) -> String {
        match self {
            Error::UndeclaredSymbol { symbol } => {
                common::error::render_block(source, symbol.byte_range(), "Use of undeclared symbol")
            }
            Error::RedeclaredFunction(x) => {
                common::error::render_block(source, x.byte_range(), "Function declared twice")
            }
            Error::RedeclaredVariant(x) => {
                common::error::render_block(source, x.byte_range(), "Variant declared twice")
            }
            Error::RedeclaredParameter { redecl, original } => {
                let a = common::error::render_block(
                    source,
                    redecl.byte_range(),
                    "Parameter declared twice",
                );
                let b = common::error::render_block(
                    source,
                    original.byte_range(),
                    "First declared here",
                );
                format!("{a}\n{b}")
            }
            Error::VariationMissingParameter {
                variation,
                parameter,
            } => {
                let a = common::error::render_block(
                    source,
                    variation.byte_range(),
                    "Variation did not contain all parameters",
                );
                let b = common::error::render_block(
                    source,
                    parameter.byte_range(),
                    "This parameter was not contained in the variation",
                );
                format!("{a}\n{b}")
            }
            Error::RedeclaredVariation(s) => common::error::render_block(
                source,
                s.byte_range(),
                "Variation for parameter declared more then once",
            ),
            Error::Type(_) => {
                format!("Type error")
            }
            Error::PushNode => "Exceeded node limits".to_string(),
        }
    }
}

pub fn compile(_ast: Ast, _root: NodeId<ast::Module>) -> Result<(), Error> {
    todo!()
}
