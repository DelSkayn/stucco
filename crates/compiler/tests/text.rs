#![cfg(feature = "dev")]

use ast::visit::Visit;
use common::{
    render::IndentFormatter,
    test::{current_file_path, string_test_runner},
};
use parser::{Parser, parse_external_module};

use stucco_compiler::{
    resolve::{self, ResolveInfo, print::ResolvePrinter},
    type_check::Types,
};

#[test]
fn infer_text_tests() {
    string_test_runner(&current_file_path().join("infer"), |src| {
        let (node, ast) = match Parser::parse_str_func(&src, parse_external_module) {
            Ok(x) => x,
            Err(e) => return format!("PARSE ERROR: {}", e.render(src)),
        };

        let mut info = ResolveInfo::new();
        match resolve::resolve(node, &ast, &mut info) {
            Ok(x) => x,
            Err(e) => return format!("RESOLVE ERROR: {}", e.render(src)),
        };

        let mut types = Types::new();
        types.infer(&ast, &info.symbols, node).unwrap();

        let mut s = String::new();
        let mut fmt = IndentFormatter::new(&mut s, 2);
        stucco_compiler::type_check::print::TypePrinter::new(
            false,
            &mut fmt,
            &src,
            &info.symbols,
            &types,
        )
        .visit_module(&ast, node)
        .unwrap();
        s
    })
}

#[test]
fn resolve_text_tests() {
    string_test_runner(&current_file_path().join("resolve"), |src| {
        let (node, ast) = match Parser::parse_str_func(&src, parse_external_module) {
            Ok(x) => x,
            Err(e) => return format!("PARSE ERROR: {}", e.render(src)),
        };
        let mut info = ResolveInfo::new();
        match resolve::resolve(node, &ast, &mut info) {
            Ok(x) => x,
            Err(e) => return format!("RESOLVE ERROR: {}", e.render(src)),
        };

        let mut s = String::new();
        let mut fmt = IndentFormatter::new(&mut s, 4);
        ResolvePrinter::new(&mut fmt, src, &info.symbols, false)
            .visit_module(&ast, node)
            .unwrap();

        s
    })
}
