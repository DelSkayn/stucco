use ast::visit::Visit;
use common::render::{self, IndentFormatter};
use parser::{Parser, parse_external_module};
use std::{
    env,
    error::Error,
    io::{Read, Write as _},
};
use stucco_compiler::resolve::{
    ResolveInfo, resolve,
    symbols::print::{ResolvePrinter, format_symbol_table},
};

fn main() -> Result<(), Box<dyn Error>> {
    let src = if let Some(arg) = env::args().skip(1).next() {
        std::fs::read_to_string(arg)?
    } else {
        let mut buf = String::new();
        std::io::stdin().read_to_string(&mut buf)?;
        buf
    };

    let (root_node, ast) = match Parser::parse_str_func(&src, parse_external_module) {
        Ok((node, ast)) => (node, ast),
        Err(e) => {
            let mut w = std::io::stderr().lock();
            e.render_char_buffer().write_styled(&mut w)?;
            writeln!(&mut w)?;
            return Ok(());
        }
    };

    let mut info = ResolveInfo::new();
    match resolve(&src, root_node, &ast, &mut info) {
        Ok(root_scope) => println!(
            "{}",
            format_symbol_table(&src, &ast, &info.symbols, root_node, root_scope, true)
        ),
        Err(e) => {
            let mut w = std::io::stderr().lock();
            e.render_char_buffer().write_styled(&mut w)?;
            writeln!(&mut w)?;
        }
    }

    Ok(())
}
