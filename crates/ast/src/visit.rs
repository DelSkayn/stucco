use crate::{
    Ast, BinaryExpr, Block, Break, Call, Cast, Expr, Field, If, Index, Let, Method, Module, NodeId,
    NodeListId, Parameter, Return, Span, StencilFunction, Symbol, Tail, Type, UnaryExpr, Variant,
    VariantConstant, While,
};
use syn::{Ident, Lit};

pub trait Visit: Sized {
    type Error;

    fn visit_module(&mut self, ast: &Ast, m: NodeId<Module>) -> Result<(), Self::Error> {
        visit_module(self, ast, m)
    }

    fn visit_stencil_function(
        &mut self,
        ast: &Ast,
        f: NodeId<StencilFunction>,
    ) -> Result<(), Self::Error> {
        visit_stencil_function(self, ast, f)
    }

    fn visit_variant(&mut self, ast: &Ast, f: NodeId<Variant>) -> Result<(), Self::Error> {
        visit_variant(self, ast, f)
    }

    fn visit_variant_constant(
        &mut self,
        ast: &Ast,
        f: NodeId<VariantConstant>,
    ) -> Result<(), Self::Error> {
        visit_variant_constant(self, ast, f)
    }

    fn visit_type(&mut self, ast: &Ast, f: NodeId<Type>) -> Result<(), Self::Error> {
        let _ = (ast, f);
        Ok(())
    }

    fn visit_parameter(&mut self, ast: &Ast, f: NodeId<Parameter>) -> Result<(), Self::Error> {
        visit_parameter(self, ast, f)
    }

    fn visit_expr(&mut self, ast: &Ast, f: NodeId<Expr>) -> Result<(), Self::Error> {
        visit_expr(self, ast, f)
    }

    fn visit_block(&mut self, ast: &Ast, f: NodeId<Block>) -> Result<(), Self::Error> {
        visit_block(self, ast, f)
    }

    fn visit_inner_block(
        &mut self,
        ast: &Ast,
        f: Option<NodeListId<Expr>>,
    ) -> Result<(), Self::Error> {
        visit_inner_block(self, ast, f)
    }

    fn visit_if(&mut self, ast: &Ast, f: NodeId<If>) -> Result<(), Self::Error> {
        visit_if(self, ast, f)
    }

    fn visit_loop(&mut self, ast: &Ast, f: NodeId<Block>) -> Result<(), Self::Error> {
        visit_loop(self, ast, f)
    }

    fn visit_while(&mut self, ast: &Ast, f: NodeId<While>) -> Result<(), Self::Error> {
        visit_while(self, ast, f)
    }

    fn visit_tail(&mut self, ast: &Ast, f: NodeId<Tail>) -> Result<(), Self::Error> {
        visit_tail(self, ast, f)
    }

    fn visit_return(&mut self, ast: &Ast, f: NodeId<Return>) -> Result<(), Self::Error> {
        visit_return(self, ast, f)
    }

    fn visit_continue(&mut self, ast: &Ast, f: Span) -> Result<(), Self::Error> {
        let _ = (ast, f);
        Ok(())
    }

    fn visit_break(&mut self, ast: &Ast, f: NodeId<Break>) -> Result<(), Self::Error> {
        visit_break(self, ast, f)
    }

    fn visit_let(&mut self, ast: &Ast, f: NodeId<Let>) -> Result<(), Self::Error> {
        visit_let(self, ast, f)
    }

    fn visit_cast(&mut self, ast: &Ast, f: NodeId<Cast>) -> Result<(), Self::Error> {
        visit_cast(self, ast, f)
    }

    fn visit_binary(&mut self, ast: &Ast, f: NodeId<BinaryExpr>) -> Result<(), Self::Error> {
        visit_binary(self, ast, f)
    }

    fn visit_unary(&mut self, ast: &Ast, f: NodeId<UnaryExpr>) -> Result<(), Self::Error> {
        visit_unary(self, ast, f)
    }

    fn visit_field(&mut self, ast: &Ast, f: NodeId<Field>) -> Result<(), Self::Error> {
        visit_field(self, ast, f)
    }

    fn visit_index(&mut self, ast: &Ast, f: NodeId<Index>) -> Result<(), Self::Error> {
        visit_index(self, ast, f)
    }

    fn visit_call(&mut self, ast: &Ast, f: NodeId<Call>) -> Result<(), Self::Error> {
        visit_call(self, ast, f)
    }

    fn visit_method(&mut self, ast: &Ast, f: NodeId<Method>) -> Result<(), Self::Error> {
        visit_method(self, ast, f)
    }

    fn visit_literal(&mut self, ast: &Ast, f: NodeId<Lit>) -> Result<(), Self::Error> {
        let _ = (ast, f);
        Ok(())
    }

    fn visit_symbol(&mut self, ast: &Ast, f: NodeId<Symbol>) -> Result<(), Self::Error> {
        visit_symbol(self, ast, f)
    }

    fn visit_ident(&mut self, ast: &Ast, f: NodeId<Ident>) -> Result<(), Self::Error> {
        let _ = (ast, f);
        Ok(())
    }
}

pub fn visit_module<V>(visit: &mut V, ast: &Ast, m: NodeId<Module>) -> Result<(), V::Error>
where
    V: Visit,
{
    let mut cur = ast[m].functions;
    while let Some(f) = ast.next_list(&mut cur) {
        visit.visit_stencil_function(ast, f)?
    }
    Ok(())
}

pub fn visit_stencil_function<V>(
    visit: &mut V,
    ast: &Ast,
    m: NodeId<StencilFunction>,
) -> Result<(), V::Error>
where
    V: Visit,
{
    visit.visit_symbol(ast, ast[m].sym)?;

    let mut cur = ast[m].variants;
    while let Some(f) = ast.next_list(&mut cur) {
        visit.visit_variant(ast, f)?
    }

    let mut cur = ast[m].parameters;
    while let Some(f) = ast.next_list(&mut cur) {
        visit.visit_parameter(ast, f)?
    }

    if let Some(ty) = ast[m].output {
        visit.visit_type(ast, ty)?;
    }

    visit.visit_inner_block(ast, ast[m].body)?;

    Ok(())
}

pub fn visit_variant<V>(visit: &mut V, ast: &Ast, m: NodeId<Variant>) -> Result<(), V::Error>
where
    V: Visit,
{
    match ast[m] {
        Variant::Constant(x) => visit.visit_variant_constant(ast, x),
    }
}

pub fn visit_variant_constant<V>(
    visit: &mut V,
    ast: &Ast,
    m: NodeId<VariantConstant>,
) -> Result<(), V::Error>
where
    V: Visit,
{
    visit.visit_symbol(ast, ast[m].sym)?;
    visit.visit_type(ast, ast[m].ty)?;

    Ok(())
}

pub fn visit_parameter<V>(visit: &mut V, ast: &Ast, m: NodeId<Parameter>) -> Result<(), V::Error>
where
    V: Visit,
{
    visit.visit_symbol(ast, ast[m].sym)?;
    visit.visit_type(ast, ast[m].ty)?;

    Ok(())
}

pub fn visit_expr<V>(visit: &mut V, ast: &Ast, e: NodeId<Expr>) -> Result<(), V::Error>
where
    V: Visit,
{
    match ast[e] {
        Expr::If(x) => visit.visit_if(ast, x),
        Expr::Binary(x) => visit.visit_binary(ast, x),
        Expr::Unary(x) => visit.visit_unary(ast, x),
        Expr::Block(x) => visit.visit_block(ast, x),
        Expr::Cast(x) => visit.visit_cast(ast, x),
        Expr::Loop(x) => visit.visit_loop(ast, x),
        Expr::While(x) => visit.visit_while(ast, x),
        Expr::Let(x) => visit.visit_let(ast, x),
        Expr::Continue(x) => visit.visit_continue(ast, x),
        Expr::Break(x) => visit.visit_break(ast, x),
        Expr::Return(x) => visit.visit_return(ast, x),
        Expr::Tail(x) => visit.visit_tail(ast, x),
        Expr::Call(x) => visit.visit_call(ast, x),
        Expr::Method(x) => visit.visit_method(ast, x),
        Expr::Field(x) => visit.visit_field(ast, x),
        Expr::Index(x) => visit.visit_index(ast, x),
        Expr::Literal(x) => visit.visit_literal(ast, x),
        Expr::Symbol(x) => visit.visit_symbol(ast, x),
        Expr::Covered(x) => visit.visit_expr(ast, x),
    }
}

pub fn visit_block<V>(visit: &mut V, ast: &Ast, b: NodeId<Block>) -> Result<(), V::Error>
where
    V: Visit,
{
    visit.visit_inner_block(ast, ast[b].body)
}

pub fn visit_inner_block<V>(
    visit: &mut V,
    ast: &Ast,
    mut b: Option<NodeListId<Expr>>,
) -> Result<(), V::Error>
where
    V: Visit,
{
    while let Some(f) = ast.next_list(&mut b) {
        visit.visit_expr(ast, f)?
    }

    Ok(())
}

pub fn visit_if<V>(visit: &mut V, ast: &Ast, e: NodeId<If>) -> Result<(), V::Error>
where
    V: Visit,
{
    visit.visit_expr(ast, ast[e].condition)?;

    visit.visit_inner_block(ast, ast[e].then)?;

    visit.visit_inner_block(ast, ast[e].otherwise)?;

    Ok(())
}

pub fn visit_loop<V>(visit: &mut V, ast: &Ast, e: NodeId<Block>) -> Result<(), V::Error>
where
    V: Visit,
{
    visit.visit_block(ast, e)
}

pub fn visit_while<V>(visit: &mut V, ast: &Ast, e: NodeId<While>) -> Result<(), V::Error>
where
    V: Visit,
{
    visit.visit_expr(ast, ast[e].condition)?;
    visit.visit_inner_block(ast, ast[e].then)?;
    Ok(())
}

pub fn visit_return<V>(visit: &mut V, ast: &Ast, e: NodeId<Return>) -> Result<(), V::Error>
where
    V: Visit,
{
    if let Some(x) = ast[e].expr {
        visit.visit_expr(ast, x)?;
    }
    Ok(())
}

pub fn visit_tail<V>(visit: &mut V, ast: &Ast, e: NodeId<Tail>) -> Result<(), V::Error>
where
    V: Visit,
{
    visit.visit_ident(ast, ast[e].callee)?;

    let mut cur = ast[e].args;
    while let Some(x) = ast.next_list(&mut cur) {
        visit.visit_expr(ast, x)?;
    }
    Ok(())
}

pub fn visit_break<V>(visit: &mut V, ast: &Ast, e: NodeId<Break>) -> Result<(), V::Error>
where
    V: Visit,
{
    if let Some(x) = ast[e].expr {
        visit.visit_expr(ast, x)?;
    }
    Ok(())
}

pub fn visit_let<V>(visit: &mut V, ast: &Ast, e: NodeId<Let>) -> Result<(), V::Error>
where
    V: Visit,
{
    visit.visit_symbol(ast, ast[e].sym)?;
    visit.visit_expr(ast, ast[e].expr)?;
    Ok(())
}

pub fn visit_cast<V>(visit: &mut V, ast: &Ast, e: NodeId<Cast>) -> Result<(), V::Error>
where
    V: Visit,
{
    visit.visit_expr(ast, ast[e].expr)?;
    visit.visit_type(ast, ast[e].ty)?;
    Ok(())
}

pub fn visit_binary<V>(visit: &mut V, ast: &Ast, e: NodeId<BinaryExpr>) -> Result<(), V::Error>
where
    V: Visit,
{
    visit.visit_expr(ast, ast[e].left)?;
    visit.visit_expr(ast, ast[e].right)?;
    Ok(())
}

pub fn visit_unary<V>(visit: &mut V, ast: &Ast, e: NodeId<UnaryExpr>) -> Result<(), V::Error>
where
    V: Visit,
{
    visit.visit_expr(ast, ast[e].expr)?;
    Ok(())
}

pub fn visit_field<V>(visit: &mut V, ast: &Ast, e: NodeId<Field>) -> Result<(), V::Error>
where
    V: Visit,
{
    visit.visit_expr(ast, ast[e].base)?;
    visit.visit_ident(ast, ast[e].field)?;
    Ok(())
}

pub fn visit_index<V>(visit: &mut V, ast: &Ast, e: NodeId<Index>) -> Result<(), V::Error>
where
    V: Visit,
{
    visit.visit_expr(ast, ast[e].base)?;
    visit.visit_expr(ast, ast[e].index)?;
    Ok(())
}

pub fn visit_call<V>(visit: &mut V, ast: &Ast, e: NodeId<Call>) -> Result<(), V::Error>
where
    V: Visit,
{
    visit.visit_expr(ast, ast[e].func)?;

    let mut cur = ast[e].args;
    while let Some(x) = ast.next_list(&mut cur) {
        visit.visit_expr(ast, x)?;
    }

    Ok(())
}

pub fn visit_method<V>(visit: &mut V, ast: &Ast, e: NodeId<Method>) -> Result<(), V::Error>
where
    V: Visit,
{
    visit.visit_expr(ast, ast[e].receiver)?;
    visit.visit_ident(ast, ast[e].name)?;
    let mut cur = ast[e].args;
    while let Some(x) = ast.next_list(&mut cur) {
        visit.visit_expr(ast, x)?;
    }

    Ok(())
}

pub fn visit_symbol<V>(visit: &mut V, ast: &Ast, s: NodeId<Symbol>) -> Result<(), V::Error>
where
    V: Visit,
{
    visit.visit_ident(ast, ast[s].name)
}
