use super::{Ast, NodeId, NodeLibrary, NodeListId, Span};
use std::fmt;
use syn::{Ident, Lit};

pub struct AstRender<'a, L, N> {
    ast: &'a Ast<L>,
    node: N,
}

impl<'a, L, N> AstRender<'a, L, N> {
    pub fn new(ast: &'a Ast<L>, node: N) -> Self {
        AstRender { ast, node }
    }
}

impl<'a, L, N> fmt::Display for AstRender<'a, L, N>
where
    L: NodeLibrary,
    N: for<'f> AstDisplay<L, fmt::Formatter<'f>>,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut buffer = String::new();
        let mut formatter = AstFormatter {
            buffer: &mut buffer,
            ast: self.ast,
            w: f,
            depth: 0,
        };
        self.node.fmt(&mut formatter)?;
        formatter.flush()
    }
}

pub struct AstFormatter<'a, L, W> {
    pub ast: &'a Ast<L>,
    buffer: &'a mut String,
    w: &'a mut W,
    depth: usize,
}

impl<'a, L, W> AstFormatter<'a, L, W>
where
    L: NodeLibrary,
    W: fmt::Write,
{
    fn flush(&mut self) -> fmt::Result {
        self.w.write_str(&self.buffer)?;
        self.buffer.clear();
        Ok(())
    }

    pub fn indent<F>(&mut self, f: F) -> fmt::Result
    where
        F: FnOnce(&mut AstFormatter<L, W>) -> fmt::Result,
    {
        self.depth += 1;
        let res = f(self);
        self.depth -= 1;
        res
    }

    pub fn scope<N, F>(&mut self, n: NodeId<N>, f: F) -> fmt::Result
    where
        F: for<'b> FnOnce(&'b N, &'b mut AstFormatter<L, W>) -> fmt::Result,
        N: 'static,
    {
        let borrow = &self.ast[n];
        let res = {
            let mut formatter = AstFormatter {
                ast: self.ast,
                buffer: self.buffer,
                w: self.w,
                depth: self.depth,
            };
            f(borrow, &mut formatter)
        };
        res
    }

    pub fn write_fmt(&mut self, args: fmt::Arguments) -> fmt::Result {
        <Self as fmt::Write>::write_fmt(self, args)
    }
}

#[cfg(feature = "print")]
impl<'a, L, W> fmt::Write for AstFormatter<'a, L, W>
where
    L: NodeLibrary,
    W: fmt::Write,
{
    fn write_str(&mut self, s: &str) -> fmt::Result {
        let flush_last = s.ends_with('\n');
        let mut last = None;
        for line in s.lines() {
            if let Some(x) = last.replace(line) {
                if !self.buffer.is_empty() {
                    self.w.write_str(&self.buffer)?;
                    self.buffer.clear();
                } else {
                    for _ in 0..self.depth {
                        self.w.write_char(' ')?;
                        self.w.write_char(' ')?;
                    }
                }
                self.w.write_str(x)?;
                self.w.write_char('\n')?;
            }
        }
        if let Some(x) = last {
            if flush_last {
                if self.buffer.is_empty() {
                    for _ in 0..self.depth {
                        self.w.write_char(' ')?;
                        self.w.write_char(' ')?;
                    }
                } else {
                    self.w.write_str(self.buffer)?;
                    self.buffer.clear();
                }
                self.w.write_str(x)?;
                self.w.write_char('\n')?;
            } else {
                if self.buffer.is_empty() {
                    for _ in 0..self.depth {
                        self.buffer.push(' ');
                        self.buffer.push(' ');
                    }
                }
                self.buffer.push_str(x);
            }
        }
        Ok(())
    }
}

#[cfg(feature = "print")]
pub trait AstDisplay<L, W>
where
    L: NodeLibrary,
    W: fmt::Write,
{
    fn fmt(&self, fmt: &mut AstFormatter<L, W>) -> fmt::Result;
}

impl<L, W, T> AstDisplay<L, W> for Option<T>
where
    L: NodeLibrary,
    W: fmt::Write,
    T: AstDisplay<L, W>,
{
    fn fmt(&self, fmt: &mut AstFormatter<L, W>) -> fmt::Result {
        if let Some(x) = self.as_ref() {
            write!(fmt, "Some(")?;
            x.fmt(fmt)?;
            write!(fmt, ")")
        } else {
            write!(fmt, "Some")
        }
    }
}

impl<L, W, T> AstDisplay<L, W> for NodeListId<T>
where
    L: NodeLibrary,
    W: fmt::Write,
    NodeId<T>: AstDisplay<L, W>,
    T: 'static,
{
    fn fmt(&self, fmt: &mut AstFormatter<L, W>) -> fmt::Result {
        writeln!(fmt, "[")?;
        fmt.indent(|fmt| {
            let mut cur = *self;
            loop {
                fmt.ast[cur].value.fmt(fmt)?;
                writeln!(fmt, ",")?;
                let Some(next) = fmt.ast[cur].next else { break };
                cur = next;
            }
            Ok(())
        })?;
        write!(fmt, "]")
    }
}

impl<L, W, T> AstDisplay<L, W> for NodeId<T>
where
    L: NodeLibrary,
    W: fmt::Write,
    T: AstDisplay<L, W> + 'static,
{
    fn fmt(&self, fmt: &mut AstFormatter<L, W>) -> fmt::Result {
        write!(fmt, "[{}]", self.into_u32())?;
        fmt.scope::<T, _>(*self, |n, fmt| n.fmt(fmt))
    }
}

impl<L, W> AstDisplay<L, W> for Ident
where
    L: NodeLibrary,
    W: fmt::Write,
{
    fn fmt(&self, fmt: &mut AstFormatter<L, W>) -> fmt::Result {
        write!(fmt, "{:?}", self)
    }
}

impl<L, W> AstDisplay<L, W> for Lit
where
    L: NodeLibrary,
    W: fmt::Write,
{
    fn fmt(&self, fmt: &mut AstFormatter<L, W>) -> fmt::Result {
        write!(fmt, "{:?}", self)
    }
}

impl<L, W> AstDisplay<L, W> for Span
where
    L: NodeLibrary,
    W: fmt::Write,
{
    fn fmt(&self, fmt: &mut AstFormatter<L, W>) -> fmt::Result {
        write!(fmt, "Span")
    }
}

macro_rules! impl_display {
    ($($t:ty),*$(,)?) => {
        $(
        impl<L, W> AstDisplay<L, W> for $t
        where
            L: NodeLibrary,
            W: fmt::Write,
        {
            fn fmt(&self, fmt: &mut AstFormatter<L, W>) -> fmt::Result {
                fmt.write_fmt(format_args!("{}", self))
            }
        }
        )*
    };
}
impl_display!(bool);
