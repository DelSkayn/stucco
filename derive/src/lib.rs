use proc_macro::TokenStream;

#[proc_macro_attribute]
pub fn template(attrs: TokenStream, input: TokenStream) -> TokenStream {
    stucco_derive_impl::template(attrs.into(), input.into()).into()
}
