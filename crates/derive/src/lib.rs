use proc_macro::TokenStream;

#[proc_macro]
pub fn file(items: TokenStream) -> TokenStream {
    runner::file(items.into()).into()
}

#[proc_macro]
pub fn module(input: TokenStream) -> TokenStream {
    runner::module(input.into()).into()
}
