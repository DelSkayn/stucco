use ast::{AstSpanned as _, visit::Visit};
use common::render::{self, IndentFormatter};
use parser::{Parser, parse_external_module};
use std::{
    env,
    error::Error,
    io::{Read, Write},
};
use stucco_compiler::{
    resolve::{ResolveInfo, resolve},
    type_check::{TypeError, Types, print::TypePrinter},
};
use token::Spanned as _;

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
            let mut w = std::io::stderr().lock();
            e.render_char_buffer().write_styled(&mut w)?;
            writeln!(&mut w)?;
            return Ok(());
        }
    };

    let mut info = ResolveInfo::new();
    match resolve(&src, node, &ast, &mut info) {
        Ok(s) => s,
        Err(e) => {
            let mut w = std::io::stderr().lock();
            e.render_char_buffer().write_styled(&mut w)?;
            writeln!(&mut w)?;
            return Ok(());
        }
    };

    let mut types = Types::new();
    match types.infer(&ast, &info.symbols, node) {
        Ok(()) => println!(
            "{}",
            render::render(|fmt| {
                let mut fmt = IndentFormatter::new(fmt, 2);
                TypePrinter::new(true, &mut fmt, &src, &info.symbols, &types)
                    .visit_module(&ast, node)
            })
        ),
        Err(e) => {
            match e {
                TypeError::Mismatch(a, b) => {
                    eprintln!(
                        "Unexpected type {}, expected {}",
                        types.type_to_string(&ast, a),
                        types.type_to_string(&ast, b)
                    )
                }
                TypeError::LiteralOverflow(lit, ty) => {
                    eprintln!(
                        "Can't fit '{:?}' in type {}",
                        ast[lit].span().source_text(),
                        types.type_to_string(&ast, ty)
                    )
                }
                TypeError::UnknownMethod(lit, ty) => {
                    eprintln!(
                        "unknown method '{:?}' for type {}",
                        ast[lit].span().source_text(),
                        types.type_to_string(&ast, ty)
                    )
                }
                TypeError::UnknownType(ty, _) => {
                    eprintln!("unknown type '{:?}'", ast[ty].ast_span(&ast).source_text(),)
                }
                _ => {}
            }
            eprintln!("ERROR: {:?}", e);
        }
    }

    Ok(())
}
