use compiler::{
    resolve::{ResolveInfo, resolve},
    type_check::{TypeError, Types},
};
use parser::{Parser, parse_external_module};
use std::{env, error::Error, io::Read};
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

    let mut info = ResolveInfo::new();
    match resolve(node, &ast, &mut info) {
        Ok(_) => {}
        Err(e) => {
            eprintln!("ERROR: {}", e.render(&src));
            return Ok(());
        }
    };

    let mut types = Types::new();
    match types.infer(&ast, &info.symbols, node) {
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

    let code_gen = CodeGen::new(ast, info.symbols, types, Default::default());

    let entry = code_gen
        .generate_entry()
        .into_module()
        .print_to_string()
        .to_string_lossy()
        .into_owned();
    println!("=== ENTRY ===");
    println!("{entry}");

    for stencil in code_gen.ast.iter_list_node(code_gen.ast[node].stmts) {
        for var in code_gen.ast.iter_list_node(code_gen.ast[stencil].variants) {
            println!(
                "=== {} ===",
                code_gen.ast[stencil]
                    .sym
                    .index(&code_gen.ast)
                    .name
                    .index(&code_gen.ast)
            );

            let ir = code_gen
                .generate_variant(stencil, var)
                .into_module()
                .print_to_string()
                .to_string_lossy()
                .into_owned();
            println!("{ir}")
        }
    }

    Ok(())
}
