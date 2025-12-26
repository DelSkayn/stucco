#![cfg(feature = "dev")]

use ast::visit::Visit;
use common::{
    render::IndentFormatter,
    test::{current_file_path, string_test_runner},
};
use parser::{Parser, parse_external_module};

use stucco_compiler::{
    resolve::{
        self, ResolveInfo, SymbolResolvePass, SymbolTable, symbols::print::format_symbol_table,
    },
    type_check::Types,
};

#[test]
fn infer_text_tests() {
    string_test_runner(&current_file_path().join("infer"), |src| {
        let (node, ast) = match Parser::parse_str_func(&src, parse_external_module) {
            Ok(x) => x,
            Err(e) => return format!("PARSE ERROR: {}", e.render_string()),
        };
        let mut info = ResolveInfo::new();
        match resolve::resolve(src, node, &ast, &mut info) {
            Ok(x) => x,
            Err(e) => return format!("RESOLVE ERROR: {}", e.render_string()),
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
fn resolve_symbol_text_tests() {
    string_test_runner(&current_file_path().join("resolve_symbol"), |src| {
        let (root_node, ast) = match Parser::parse_str_func(&src, parse_external_module) {
            Ok(x) => x,
            Err(e) => return format!("PARSE ERROR: {}", e.render_string()),
        };
        let mut symbol_table = SymbolTable::new();
        let root_scope = match SymbolResolvePass::new(src, &mut symbol_table).pass(&ast, root_node)
        {
            Ok(x) => x,
            Err(e) => return format!("RESOLVE ERROR: {}", e.render_string()),
        };

        format_symbol_table(src, &ast, &symbol_table, root_node, root_scope, false)
    })
}
