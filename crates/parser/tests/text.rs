use ast::AstRender;
use common::test::{current_file_path, string_test_runner};
use stucco_parser::{Parser, parse_external_module};

#[test]
fn ast_test() {
    string_test_runner(
        &current_file_path().join("ast"),
        |src| match Parser::parse_str_func(&src, parse_external_module) {
            Ok((node, ast)) => {
                format!("{}", AstRender::new(&ast, node))
            }
            Err(e) => e.render_string(),
        },
    )
}
