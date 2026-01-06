use parser::{Parser, parse_external_module};
use std::{
    env,
    error::Error,
    io::{Read, Write},
};
use stucco_compiler::{
    resolve::{ResolveInfo, resolve},
    type_check::check,
};

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

    match check(&src, node, &ast, &mut info) {
        Ok(_) => {
            todo!()
        }
        Err(e) => {
            let mut w = std::io::stderr().lock();
            e.render_char_buffer().write_styled(&mut w)?;
            writeln!(&mut w)?;
        }
    }
    Ok(())
}
