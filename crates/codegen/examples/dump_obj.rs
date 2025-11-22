use compiler::{
    resolve::{ResolveInfo, resolve},
    type_check::{TypeError, Types},
};
use parser::{Parser, parse_external_module};
use std::{env, error::Error, path::Path};
use stucco_codegen::{CodeGen, Target};

fn main() -> Result<(), Box<dyn Error>> {
    let Some(arg) = env::args().skip(1).next() else {
        return Err("dumping to object requires a file".into());
    };
    let src = std::fs::read_to_string(&arg)?;

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

    let path = format!("{arg}_obj");
    let path = Path::new(&path);
    if let Ok(x) = std::fs::metadata(path) {
        if !x.is_dir() {
            return Err("Output path already exists".into());
        }
    } else {
        std::fs::create_dir(path)?;
    }

    let obj = code_gen.generate_entry().into_object(Target::X86_64);
    std::fs::write(path.join(format!("__entry__.o")), &obj)?;

    for stencil in code_gen.ast.iter_list_node(code_gen.ast[node].stmts) {
        for var in code_gen.ast.iter_list_node(code_gen.ast[stencil].variants) {
            let obj = code_gen
                .generate_variant(stencil, var)
                .into_object(Target::X86_64);

            let mut name = code_gen.ast[stencil]
                .sym
                .index(&code_gen.ast)
                .name
                .index(&code_gen.ast)
                .to_string();
            for v in code_gen.ast.iter_list(code_gen.ast[var].variations) {
                match v {
                    ast::Variation::Immediate(n) => {
                        name.push_str("_");
                        name.push_str(
                            &code_gen.ast[*n]
                                .sym
                                .index(&code_gen.ast)
                                .name
                                .index(&code_gen.ast)
                                .to_string(),
                        );
                        name.push_str("_imm");
                    }
                    ast::Variation::Slot(n) => {
                        name.push_str("_");
                        name.push_str(
                            &code_gen.ast[*n]
                                .sym
                                .index(&code_gen.ast)
                                .name
                                .index(&code_gen.ast)
                                .to_string(),
                        );
                        name.push_str("_slot");
                    }
                    ast::Variation::Const(_) => todo!(),
                }
            }

            std::fs::write(path.join(format!("{name}.o")), &obj)?;
        }
    }

    Ok(())
}
