use ast::AstRender;
use std::{env, error::Error, io::Read};
use stucco_parser::Parser;

fn main() -> Result<(), Box<dyn Error>> {
    let src = if let Some(arg) = env::args().skip(1).next() {
        std::fs::read_to_string(arg)?
    } else {
        let mut buf = String::new();
        std::io::stdin().read_to_string(&mut buf)?;
        buf
    };

    match Parser::parse_str::<ast::Module>(&src) {
        Ok((node, ast)) => {
            println!("{}", AstRender::new(&ast, node));
        }
        Err(e) => {
            eprintln!("ERROR: {}", stucco_parser::error::render(&src, e))
        }
    }

    Ok(())
}
