use ast::{PushNodeError, Span};
pub use imp::render;

#[derive(Debug)]
pub struct Error {
    pub span: Span,
    pub message: String,
}

impl From<PushNodeError> for Error {
    fn from(_: PushNodeError) -> Self {
        Error {
            span: Span::call_site(),
            message: "Too many AST nodes, source file too long".to_string(),
        }
    }
}

#[cfg(not(feature = "span-locations"))]
mod imp {
    use super::Error;
    pub fn render(source: &str, err: Error) -> String {
        let _ = source;
        format!("{}", err)
    }
}

#[cfg(feature = "span-locations")]
mod imp {
    use super::Error;
    pub fn render(source: &str, err: Error) -> String {
        common::error::render_block(source, err.span.byte_range(), &err.message)
    }
}
