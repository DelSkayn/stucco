use proc_macro2::{Delimiter, Group, Ident, Literal, Punct, Spacing, Span, TokenStream, TokenTree};

use crate::{AnnotationKind, Diagnostic};

pub fn render(g: &Diagnostic) -> TokenStream {
    let mut stream = TokenStream::new();
    for g in g.groups.iter() {
        let span = g
            .elements
            .first()
            .unwrap()
            .annotations
            .iter()
            .find(|x| x.kind == AnnotationKind::Primary)
            .unwrap()
            .span;

        let message = g.title.as_ref();
        extend_error(&mut stream, message, span.into());
    }
    stream
}

fn extend_error(s: &mut TokenStream, error: &str, span: Span) {
    s.extend([
        TokenTree::Ident(Ident::new("compile_error", span)),
        TokenTree::Punct(Punct::new('!', Spacing::Alone)),
        TokenTree::Group(Group::new(
            Delimiter::Parenthesis,
            [TokenTree::Literal(Literal::string(error))]
                .into_iter()
                .collect(),
        )),
    ]);
}
