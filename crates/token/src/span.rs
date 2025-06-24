use std::hash::Hasher;

use proc_macro2::Ident;

#[derive(Clone, Copy, Debug)]
pub struct Span(proc_macro2::Span);

impl std::hash::Hash for Span {
    fn hash<H: Hasher>(&self, _state: &mut H) {}
}
impl std::cmp::PartialEq for Span {
    fn eq(&self, _other: &Self) -> bool {
        true
    }
}
impl std::cmp::Eq for Span {}

impl Span {
    /// Returns a span which covers the proc-macro call site.
    pub fn call_site() -> Self {
        Self(proc_macro2::Span::call_site())
    }

    /// Joints two span if the underlying implementation supports it.
    pub fn try_join(&self, other: Self) -> Self {
        self.0
            .join(other.0)
            .map(Span)
            .unwrap_or_else(|| self.clone())
    }

    /// Returns the byte in the source code for the range.
    #[cfg(feature = "span-locations")]
    pub fn byte_range(&self) -> std::ops::Range<usize> {
        self.0.byte_range()
    }
}

impl From<proc_macro2::Span> for Span {
    fn from(value: proc_macro2::Span) -> Self {
        Self(value)
    }
}

impl From<Span> for proc_macro2::Span {
    fn from(value: Span) -> Self {
        value.0
    }
}

impl Spanned for Span {
    fn span(&self) -> Span {
        self.clone()
    }
}

/// Trait for types which contain a span
pub trait Spanned {
    fn span(&self) -> Span;
}

impl Spanned for Ident {
    fn span(&self) -> Span {
        self.span().into()
    }
}
