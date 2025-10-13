use ast::{
    Ast, AstSpanned as _,
    visit::{self, Visit},
};
use common::{
    error,
    id::Id,
    render::{self, IndentFormatter},
};
use core::fmt;
use parser::{Parser, parse_external_module};
use std::{env, error::Error, fmt::Write as _, io::Read};
use stucco_compiler::resolve::{Symbols, resolve};

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

    match resolve(node, &ast) {
        Ok(s) => println!(
            "{}",
            render::render(|fmt| {
                let mut fmt = IndentFormatter::new(fmt, 2);
                ResolvePrinter::new(&mut fmt, &src, &s).visit_module(&ast, node)
            })
        ),
        Err(e) => eprintln!("ERROR: {}", e.render(&src)),
    }

    Ok(())
}

struct ResolvePrinter<'a, W> {
    source: &'a str,
    fmt: &'a mut IndentFormatter<W>,
    symbols: &'a Symbols,
}

impl<'a, W> ResolvePrinter<'a, W>
where
    W: fmt::Write,
{
    pub fn new(fmt: &'a mut IndentFormatter<W>, source: &'a str, symbols: &'a Symbols) -> Self {
        ResolvePrinter {
            fmt,
            source,
            symbols,
        }
    }

    pub fn indent<F>(&mut self, f: F) -> fmt::Result
    where
        F: FnOnce(&mut ResolvePrinter<W>) -> fmt::Result,
    {
        self.fmt.indent(|fmt| {
            let mut this = ResolvePrinter {
                fmt,
                source: self.source,
                symbols: self.symbols,
            };
            f(&mut this)
        })
    }
}

impl<W> Visit for ResolvePrinter<'_, W>
where
    W: fmt::Write,
{
    type Error = fmt::Error;

    fn visit_module(&mut self, ast: &Ast, m: ast::NodeId<ast::Module>) -> Result<(), Self::Error> {
        writeln!(self.fmt, "module {{")?;
        self.indent(|this| visit::visit_module(this, ast, m))?;
        writeln!(self.fmt, "}}")
    }

    fn visit_stencil(
        &mut self,
        ast: &Ast,
        f: ast::NodeId<ast::Stencil>,
    ) -> Result<(), Self::Error> {
        writeln!(
            self.fmt,
            "stencil {} {{",
            f.index(ast).sym.index(ast).name.index(ast)
        )?;
        self.indent(|this| visit::visit_stencil(this, ast, f))?;
        writeln!(self.fmt, "}}")
    }

    fn visit_symbol(&mut self, ast: &Ast, f: ast::NodeId<ast::Symbol>) -> Result<(), Self::Error> {
        let line = error::render_line(self.source, f.ast_span(ast).byte_range(), true);
        write!(self.fmt, "{} @ '{}'", ast[f].name.index(&ast), line.trim())?;
        if let Some(x) = self.symbols.ast_to_symbol.get(f).copied().and_then(|x| x) {
            writeln!(self.fmt, " = [{:?}]", x.idx())
        } else {
            writeln!(self.fmt, " = NOT RESOLVED")
        }
    }
}
