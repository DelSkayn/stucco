use ast::visit::Visit;
use common::render::{self, IndentFormatter};
use parser::{Parser, parse_external_module};
use std::{env, error::Error, io::Read};
use stucco_compiler::resolve::{ResolveInfo, print::ResolvePrinter, resolve};

fn main() -> Result<(), Box<dyn Error>> {
    let src = if let Some(arg) = env::args().skip(1).next() {
        std::fs::read_to_string(arg)?
    } else {
        let mut buf = String::new();
        std::io::stdin().read_to_string(&mut buf)?;
        buf
    };

    let (node, ast) = match Parser::parse_str_func(&src, parse_external_module) {
        Ok((node, ast)) => (node, ast),
        Err(e) => {
            eprintln!("ERROR: {}", parser::error::render(&src, e));
            return Ok(());
        }
    };

    let mut info = ResolveInfo::new();
    match resolve(node, &ast, &mut info) {
        Ok(_) => println!(
            "{}",
            render::render(|fmt| {
                let mut fmt = IndentFormatter::new(fmt, 2);
                ResolvePrinter::new(&mut fmt, &src, &info.symbols, true).visit_module(&ast, node)
            })
        ),
        Err(e) => eprintln!("ERROR: {}", e.render(&src)),
    }

    Ok(())
}
