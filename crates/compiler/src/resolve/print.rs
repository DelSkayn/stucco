use core::fmt::{self, Write};

use ast::{
    Ast, AstSpanned,
    visit::{self, Visit},
};
use common::{error, id::Id, render::IndentFormatter};

use crate::resolve::SymbolTable;

pub struct ResolvePrinter<'a, W> {
    source: &'a str,
    fmt: &'a mut IndentFormatter<W>,
    symbols: &'a SymbolTable,
    pretty: bool,
}

impl<'a, W> ResolvePrinter<'a, W>
where
    W: fmt::Write,
{
    pub fn new(
        fmt: &'a mut IndentFormatter<W>,
        source: &'a str,
        symbols: &'a SymbolTable,
        pretty: bool,
    ) -> Self {
        ResolvePrinter {
            fmt,
            source,
            symbols,
            pretty,
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
                pretty: self.pretty,
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

    fn visit_function(
        &mut self,
        ast: &Ast,
        f: ast::NodeId<ast::Function>,
    ) -> Result<(), Self::Error> {
        writeln!(
            self.fmt,
            "function {} {{",
            f.index(ast).sym.index(ast).name.index(ast)
        )?;
        self.indent(|this| visit::visit_function(this, ast, f))?;
        writeln!(self.fmt, "}}")
    }

    fn visit_symbol(&mut self, ast: &Ast, f: ast::NodeId<ast::Symbol>) -> Result<(), Self::Error> {
        let line = error::render_line(self.source, f.ast_span(ast).byte_range(), self.pretty);
        write!(self.fmt, "{} @ '{}'", ast[f].name.index(&ast), line.trim())?;
        if let Some(x) = self.symbols.ast_to_symbol.get(f).copied() {
            writeln!(self.fmt, " = [{:?}]", x.idx())
        } else {
            writeln!(self.fmt, " = NOT RESOLVED")
        }
    }
}
