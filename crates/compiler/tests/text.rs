#![cfg(feature = "dev")]

use ast::visit::Visit;
use common::{
    render::IndentFormatter,
    test::{current_file_path, string_test_runner},
};
use parser::{Parser, parse_external_module};

use stucco_compiler::{
    resolve::{
        self, ResolveInfo, SymbolResolvePass, SymbolTable, TypeResolvePass, TypeTable,
        symbols::print::format_symbol_table,
    },
    type_check::check,
};

#[test]
fn check_text_tests() {
    string_test_runner(&current_file_path().join("check"), |src| {
        let (node, ast) = match Parser::parse_str_func(&src, parse_external_module) {
            Ok(x) => x,
            Err(e) => return format!("PARSE ERROR: {}", e.render_string()),
        };
        let mut info = ResolveInfo::new();
        match resolve::resolve(src, node, &ast, &mut info) {
            Ok(x) => x,
            Err(e) => return format!("RESOLVE ERROR: {}", e.render_string()),
        };

        match check(src, node, &ast, &mut info) {
            Ok(_) => {}
            Err(e) => return format!("TYPE ERROR: {}", e.render_string()),
        }

        // TODO: Format infered types
        String::new()
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

#[test]
fn resolve_type_text_tests() {
    string_test_runner(&current_file_path().join("resolve_type"), |src| {
        let (root_node, ast) = match Parser::parse_str_func(&src, parse_external_module) {
            Ok(x) => x,
            Err(e) => return format!("PARSE ERROR: {}", e.render_string()),
        };

        let mut type_table = TypeTable::new();
        match TypeResolvePass::new(src, &mut type_table).pass(&ast, root_node) {
            Ok(x) => x,
            Err(e) => return format!("RESOLVE ERROR: {}", e.render_string()),
        };

        // TODO: Actually format types
        String::new()
    })
}
