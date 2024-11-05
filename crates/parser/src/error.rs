#[cfg(not(feature = "span-locations"))]
mod imp {
    pub fn render(source: &str, err: syn::Error) -> String {
        let _ = source;
        format!("{}", err)
    }
}

#[cfg(feature = "span-locations")]
mod imp {
    pub fn render(source: &str, err: syn::Error) -> String {
        common::error::render_block(source, err.span().byte_range(), &err.to_string())
    }
}
pub use imp::render;
