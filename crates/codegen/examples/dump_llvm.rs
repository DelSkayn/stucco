use ast::{
    Ast, AstSpanned as _,
    visit::{self, Visit},
};
use common::{
    error,
    render::{self, IndentFormatter},
};
use compiler::{
    infer::{self, TypeError, Types},
    resolve::{Symbols, resolve},
};
use core::fmt;
use parser::{Parser, parse_external_module};
use std::{env, error::Error, fmt::Write as _, io::Read};
use stucco_codegen::CodeGen;

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

    let symbols = match resolve(node, &ast) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("ERROR: {}", e.render(&src));
            return Ok(());
        }
    };

    let mut types = Types::new();
    match types.infer(&ast, &symbols, node) {
        Ok(()) => {}
        Err(e) => {
            match e {
                TypeError::Mismatch(a, b) => {
                    eprintln!(
                        "Unexpected type {}, expected {}",
                        types.type_to_string(a),
                        types.type_to_string(b)
                    )
                }
                _ => {}
            }
            eprintln!("ERROR: {:?}", e);
        }
    }

    let code_gen = CodeGen::new(&ast, &symbols, &types, Default::default());

    for stencil in ast.iter_list_node(ast[node].stencils) {
        for var in ast.iter_list_node(ast[stencil].variants) {
            let module = code_gen.generate_variation(stencil, var).into_module();
            let str = module.print_to_string().to_string_lossy().into_owned();
            println!("{str}")
        }
    }

    Ok(())
}
