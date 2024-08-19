use ast::{PushNodeError, Span};

pub mod resolve;

#[derive(Debug)]
pub enum Error {
    UndeclaredSymbol(Span),
    PushNode(PushNodeError),
}

impl From<PushNodeError> for Error {
    fn from(v: PushNodeError) -> Self {
        Self::PushNode(v)
    }
}
