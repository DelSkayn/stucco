use common::{
    render::IndentFormatter,
    test::{current_file_path, string_test_runner},
};
use parser::{Parser, parse_external_module};

use stucco_compiler::{infer::Types, resolve};

#[cfg(feature = "dev")]
#[test]
fn infer_text_tests() {
    string_test_runner(&current_file_path().join("infer"), |src| {
        use ast::visit::Visit;

        let (node, ast) = Parser::parse_str_func(&src, parse_external_module).unwrap();
        let symbols = resolve::resolve(node, &ast).unwrap();
        let mut types = Types::new();
        types.infer(&ast, &symbols, node).unwrap();
        let mut s = String::new();
        let mut fmt = IndentFormatter::new(&mut s, 2);
        stucco_compiler::infer::print::TypePrinter::new(false, &mut fmt, &src, &symbols, &types)
            .visit_module(&ast, node)
            .unwrap();
        s
    })
}
