use ast::{
    Ast, AstSpanned as _,
    visit::{self, Visit},
};
use common::{
    error,
    render::{self, IndentFormatter},
};
use core::fmt;
use parser::{Parser, parse_external_module};
use std::{env, error::Error, fmt::Write as _, io::Read};
use stucco_compiler::{
    infer::{self, TypeError, Types},
    resolve::{Symbols, resolve},
};
use token::Spanned as _;

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
        Ok(()) => println!(
            "{}",
            render::render(|fmt| {
                let mut fmt = IndentFormatter::new(fmt, 2);
                TypePrinter::new(&mut fmt, &src, &symbols, &types).visit_module(&ast, node)
            })
        ),
        Err(e) => {
            match e {
                TypeError::Mismatch(a, b) => {
                    eprintln!(
                        "Unexpected type {}, expected {}",
                        types.type_to_string(a),
                        types.type_to_string(b)
                    )
                }
                TypeError::LiteralOverflow(lit, ty) => {
                    eprintln!(
                        "Can't fit '{:?}' in type {}",
                        ast[lit].span().source_text(),
                        types.type_to_string(ty)
                    )
                }
                _ => {}
            }
            eprintln!("ERROR: {:?}", e);
        }
    }

    Ok(())
}

struct TypePrinter<'a, W> {
    source: &'a str,
    fmt: &'a mut IndentFormatter<W>,
    symbols: &'a Symbols,
    types: &'a Types,
}

impl<'a, W> TypePrinter<'a, W>
where
    W: fmt::Write,
{
    pub fn new(
        fmt: &'a mut IndentFormatter<W>,
        source: &'a str,
        symbols: &'a Symbols,
        types: &'a Types,
    ) -> Self {
        TypePrinter {
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
        let line = error::render_line(self.source, byte_range);
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
            let line = error::render_line(self.source, byte_range);
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
}
