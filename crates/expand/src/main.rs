use std::{
    io::Write,
    path::Path,
    process::{Command, Stdio},
};

use compiler::type_check::TypeError;
use proc_macro2::{Ident, Span};
use stucco_expand::compile::CompilationError;

fn rustfmt(input: String) -> String {
    let mut command = Command::new("rustfmt")
        .stdin(Stdio::piped())
        .stderr(Stdio::inherit())
        .stdout(Stdio::piped())
        .spawn()
        .unwrap();

    Write::write_all(&mut command.stdin.take().unwrap(), input.as_bytes()).unwrap();
    let output = command.wait_with_output().unwrap();
    if !output.status.success() {
        panic!("rustfmt returned unsuccessful exit code");
    }

    String::from_utf8(output.stdout).unwrap()
}

fn main() {
    let file = std::env::args()
        .skip(1)
        .next()
        .expect("Expected name of stucco file");

    let module_name = Path::new(&file)
        .file_stem()
        .expect("Stucco file path did not have a file name");
    let module_name = module_name.display().to_string();
    let name = Ident::new(&module_name, Span::call_site());

    let source = std::fs::read_to_string(file).expect("Failed to read file");

    let tokens = source
        .parse::<proc_macro2::TokenStream>()
        .expect("Could not lex source file");

    let compile = match stucco_expand::compile::compile(tokens, name) {
        Ok(x) => x,
        Err(CompilationError::Parse(x)) => {
            eprintln!("Failed to parse source: {}", x.render(&source));
            return;
        }
        Err(CompilationError::Resolve(x)) => {
            eprintln!("Failed to resolve symbols: {}", x.render(&source));
            return;
        }
        Err(CompilationError::Types(types, e)) => {
            match e {
                TypeError::Mismatch(a, b) => {
                    eprintln!(
                        "Unexpected type {}, expected {}",
                        types.type_to_string(a),
                        types.type_to_string(b)
                    );
                }
                _ => {}
            }
            eprintln!("TYPE ERROR: {:?}", e);
            return;
        }
    };

    let expand = stucco_expand::expand::expand(compile);
    println!("{}", rustfmt(expand.to_string()));
}
