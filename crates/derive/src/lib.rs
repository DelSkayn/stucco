use proc_macro::TokenStream;

#[proc_macro]
pub fn file(items: TokenStream) -> TokenStream {
    expand::file(items.into()).into()
}
