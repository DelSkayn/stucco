use core::fmt::{self, Write};

use ast::{
    Ast, AstSpanned,
    visit::{self, Visit},
};
use common::{
    id::Id,
    render::{self, IndentFormatter},
};

use crate::resolve::{ScopeId, SymbolTable};

use super::ScopeDeclaration;

pub fn format_symbol_table(
    src: &str,
    ast: &Ast,
    table: &SymbolTable,
    root: ast::NodeId<ast::Module>,
    scope: ScopeId,
    pretty: bool,
) -> String {
    let mut res = String::new();
    let mut fmt = IndentFormatter::new(&mut res, 4);
    let _ = writeln!(&mut fmt, "DECLARED:");
    let _ = fmt.indent(|fmt| {
        format_symbol_table_rec(src, ast, table, scope, fmt, pretty);
        Ok(())
    });
    let _ = writeln!(&mut fmt, "USES:");
    let _ = fmt.indent(|fmt| ResolvePrinter::new(fmt, src, table, pretty).visit_module(ast, root));

    res
}

fn format_symbol_table_rec(
    src: &str,
    ast: &Ast,
    table: &SymbolTable,
    scope: ScopeId,
    res: &mut IndentFormatter<&mut String>,
    pretty: bool,
) {
    match table.scopes[scope].declared {
        ScopeDeclaration::Root => {
            let _ = write!(res, "module");
        }
        ScopeDeclaration::Stencil(_) => {
            let _ = write!(res, "stencil");
        }
        ScopeDeclaration::Function(_) => {
            let _ = write!(res, "stencil");
        }
        ScopeDeclaration::Block(_) => {
            let _ = write!(res, "block");
        }
    }

    let _ = writeln!(res, "{{");
    let _ = res.indent(|res| {
        if let Some(x) = table.scopes[scope].symbols {
            let mut syms = table.scope_symbols[x].iter().copied().collect::<Vec<_>>();
            syms.sort_unstable_by(|a, b| {
                table.symbols[*a]
                    .declared
                    .index(ast)
                    .span
                    .byte_range()
                    .start
                    .cmp(
                        &table.symbols[*b]
                            .declared
                            .index(ast)
                            .span
                            .byte_range()
                            .start,
                    )
            });

            for sym in syms {
                let declared = table.symbols[sym].declared;
                let line = render::render_line(src, declared.ast_span(ast).byte_range(), pretty);
                let _ = writeln!(
                    res,
                    "[{}] = {} @ '{}'",
                    sym.idx(),
                    declared.index(ast).name.index(ast),
                    line.trim(),
                );
            }
        };

        if let Some(range) = table.scopes[scope].children {
            for id in range.iter() {
                format_symbol_table_rec(src, ast, table, id, res, pretty);
            }
        }
        Ok(())
    });

    let _ = writeln!(res, "}}");
}

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
        if let Some(x) = self.symbols.ast_to_symbol.get(f).copied() {
            write!(self.fmt, "[{}] = ", x.idx())?;
        } else {
            write!(self.fmt, "NOT RESOLVED = ")?;
        }
        let line = render::render_line(self.source, f.ast_span(ast).byte_range(), self.pretty);
        writeln!(self.fmt, "{} @ '{}'", ast[f].name.index(&ast), line.trim())
    }

    fn visit_parameter(
        &mut self,
        _: &Ast,
        _: ast::NodeId<ast::Parameter>,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_let(&mut self, ast: &Ast, id: ast::NodeId<ast::Let>) -> Result<(), Self::Error> {
        self.visit_expr(ast, ast[id].expr)
    }
}
