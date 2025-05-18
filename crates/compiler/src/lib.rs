use ast::{Ast, NodeId, Span};
use infer::TypeError;
use std::{error, fmt};

pub mod infer;
pub mod resolve;

#[derive(Debug)]
pub enum Error {
    UndeclaredSymbol { symbol: Span },
    RedeclaredFunction(Span),
    RedeclaredVariant(Span),
    RedeclaredParameter { redecl: Span, original: Span },
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
            Error::Type(_) => {
                format!("Type error")
            }
            Error::PushNode => "Exceeded node limits".to_string(),
        }
    }
}

pub fn compile(ast: Ast, root: NodeId<ast::Module>) -> Result<(), Error> {
    let _symbols = resolve::resolve(root, &ast)?;
    todo!()
}
