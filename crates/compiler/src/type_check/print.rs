use std::fmt::{self, Write};

use ast::{
    Ast, AstSpanned,
    visit::{self, Visit},
};
use common::{error, render::IndentFormatter};

use crate::{type_check::Types, resolve::SymbolTable};

pub struct TypePrinter<'a, W> {
    terminal: bool,
    source: &'a str,
    fmt: &'a mut IndentFormatter<W>,
    symbols: &'a SymbolTable,
    types: &'a Types,
}

impl<'a, W> TypePrinter<'a, W>
where
    W: fmt::Write,
{
    pub fn new(
        terminal: bool,
        fmt: &'a mut IndentFormatter<W>,
        source: &'a str,
        symbols: &'a SymbolTable,
        types: &'a Types,
    ) -> Self {
        TypePrinter {
            terminal,
            fmt,
            source,
            symbols,
            types,
        }
    }

    pub fn indent<F>(&mut self, f: F) -> fmt::Result
    where
        F: FnOnce(&mut TypePrinter<W>) -> fmt::Result,
    {
        self.fmt.indent(|fmt| {
            let mut this = TypePrinter {
                terminal: self.terminal,
                fmt,
                source: self.source,
                symbols: self.symbols,
                types: self.types,
            };
            f(&mut this)
        })
    }
}

impl<W> Visit for TypePrinter<'_, W>
where
    W: fmt::Write,
{
    type Error = fmt::Error;

    fn visit_module(&mut self, ast: &Ast, m: ast::NodeId<ast::Module>) -> Result<(), Self::Error> {
        writeln!(self.fmt, "module {{")?;
        self.indent(|this| visit::visit_module(this, ast, m))?;
        writeln!(self.fmt, "}}")
    }

    fn visit_block(
        &mut self,
        ast: &ast::Ast,
        b: ast::NodeId<ast::Block>,
    ) -> Result<(), Self::Error> {
        let mut byte_range = b.ast_span(ast).byte_range();
        byte_range.end = byte_range.start + 1;
        let line = error::render_line(self.source, byte_range, self.terminal);
        writeln!(self.fmt, "'{}'", line.trim())?;
        self.indent(|this| visit::visit_inner_block(this, ast, ast[b].body))?;
        if let Some(x) = self.types.block_type[b] {
            let ty = self.types.find_type(x);
            writeln!(self.fmt, "}} = {:?}", &self.types.type_to_string(ty))
        } else {
            writeln!(self.fmt, "}} = NOT INFERED")
        }
    }

    fn visit_expr(&mut self, ast: &ast::Ast, e: ast::NodeId<ast::Expr>) -> Result<(), Self::Error> {
        if !matches!(ast[e], ast::Expr::Block(_)) {
            let byte_range = e.ast_span(ast).byte_range();
            let line = error::render_line(self.source, byte_range, self.terminal);
            write!(self.fmt, "'{}'", line.trim())?;
            if let Some(x) = self.types.expr_to_type.get(e).copied().flatten() {
                let ty = self.types.find_type(x);
                writeln!(self.fmt, " = {:?}", &self.types.type_to_string(ty))?;
            } else {
                writeln!(self.fmt, " = NOT INFERED")?;
            }
        }

        self.indent(|this| visit::visit_expr(this, ast, e))
    }

    fn visit_symbol(
        &mut self,
        ast: &ast::Ast,
        e: ast::NodeId<ast::Symbol>,
    ) -> Result<(), Self::Error> {
        let byte_range = e.ast_span(ast).byte_range();
        let line = error::render_line(self.source, byte_range, self.terminal);
        write!(self.fmt, "'{}'", line.trim())?;
        let symbol = self.symbols.ast_to_symbol[e];
        if let Some(x) = self.types.symbol_to_type.get(symbol).copied().flatten() {
            let ty = self.types.find_type(x);
            writeln!(self.fmt, " = {:?}", &self.types.type_to_string(ty))?;
        } else {
            writeln!(self.fmt, " = NOT INFERED")?;
        }
        Ok(())
    }
}
