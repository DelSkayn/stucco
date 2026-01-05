use crate::{
    Become, BinaryExpr, Block, Break, Call, Cast, Expr, Field, FieldExpr, Function, If, Index, Let,
    Loop, Method, Module, ModuleDefinition, NodeId, NodeListId, Parameter, Return, Span, Stencil,
    Stmt, Struct, Symbol, Type, TypeArray, TypeFn, TypeName, TypePtr, TypeReference, TypeTuple,
    UnaryExpr, Variant, Variation, VariationConst, VariationImmediate, VariationSlot, While,
};
use token::token::{Ident, Lit};

macro_rules! implement_visitor{
	($(fn $name:ident($this:ident, $ast:ident, $node:ident: $node_ty:ty){
	    $($t:tt)*
	})*) => {
		pub trait Visit: Sized {
			type Error;

			$(
				fn $name(&mut self,$ast: &$crate::Ast, $node: $node_ty) -> Result<(), Self::Error>{
					let $this = self;
					implement_visitor!(@body, $name,$this,$ast,$node,$($t)*)
				}
			)*
		}

		$(
			implement_visitor!(@visitor,$name,$this,$ast,$node,$node_ty, $($t)*);
		)*

	};

	(@body, $name:ident, $this:ident, $ast:ident, $node:ident, ) => {
		Ok(())
	};

	(@body, $name:ident, $this:ident,$ast:ident, $node:ident, $($t:tt)+) => {
		$name($this,$ast,$node)
	};


	(@visitor, $name:ident, $this:ident,$ast:ident, $node:ident,$node_ty:ty, $($t:tt)+) => {
		pub fn $name<V: Visit>($this: &mut V, $ast: &$crate::Ast, $node: $node_ty) -> Result<(), V::Error>{
			$($t)*
		}
	};

	(@visitor, $name:ident, $this:ident,$ast:ident, $node:ident,$node_ty:ty,) => {};
}

implement_visitor! {
    fn visit_module(visit, ast, m: NodeId<Module>) {
        let mut cur = ast[m].stmts;
        while let Some(f) = ast.next_list(&mut cur) {
            visit.visit_stmt(ast, f)?
        }
        Ok(())
    }

    fn visit_module_definition(visit, ast, m: NodeId<ModuleDefinition>){

        visit.visit_type_fn(ast, ast[m].ty)?;

        Ok(())
    }

    fn visit_stmt(visit, ast, m: NodeId<Stmt>) {
        match ast[m]{
            Stmt::Stencil(x) =>visit.visit_stencil(ast,x),
            Stmt::Struct(s) => visit.visit_struct(ast, s),
            Stmt::Function(f) => visit.visit_function(ast,f),
            Stmt::Definition(f) => visit.visit_module_definition(ast,f),
        }
    }

    fn visit_function(visit, ast, m: NodeId<Function>) {
        visit.visit_symbol(ast, ast[m].sym)?;


        let mut cur = ast[m].parameters;
        while let Some(f) = ast.next_list(&mut cur) {
            visit.visit_parameter(ast, f)?
        }


        if let Some(ty) = ast[m].output {
            visit.visit_type(ast, ty)?;
        }

        visit.visit_expr(ast, ast[m].body)?;
        Ok(())
    }

    fn visit_stencil(visit, ast, m: NodeId<Stencil>) {
        visit.visit_symbol(ast, ast[m].sym)?;


        let mut cur = ast[m].parameters;
        while let Some(f) = ast.next_list(&mut cur) {
            visit.visit_parameter(ast, f)?
        }

        let mut cur = ast[m].variants;
        while let Some(f) = ast.next_list(&mut cur) {
            visit.visit_variant(ast, f)?
        }

        if let Some(ty) = ast[m].output {
            visit.visit_type(ast, ty)?;
        }

        visit.visit_expr(ast, ast[m].body)?;

        Ok(())
    }

    fn visit_variant(visit, ast, m: NodeId<Variant>) {
        let mut cur = ast[m].variations;
        while let Some(f) = ast.next_list(&mut cur) {
            visit.visit_variation(ast, f)?
        }
        Ok(())
    }

    fn visit_variation(visit, ast, m: NodeId<Variation>) {
        match ast[m] {
            Variation::Immediate(node_id) => visit.visit_variation_immediate(ast,node_id),
            Variation::Slot(node_id) => visit.visit_variation_slot(ast,node_id),
            Variation::Const(node_id) => visit.visit_variation_const(ast,node_id),
        }
    }

    fn visit_variation_immediate(visit,ast,m: NodeId<VariationImmediate>) {
        visit.visit_symbol(ast, ast[m].sym)
    }

    fn visit_variation_slot(visit,ast,m: NodeId<VariationSlot>) {
        visit.visit_symbol(ast, ast[m].sym)
    }

    fn visit_variation_const(visit,ast,m: NodeId<VariationConst>) {
        visit.visit_symbol(ast, ast[m].sym)?;
        visit.visit_expr(ast, ast[m].expr)
    }

    fn visit_parameter(visit, ast, m: NodeId<Parameter>) {
        visit.visit_symbol(ast, ast[m].sym)?;
        visit.visit_type(ast, ast[m].ty)?;

        Ok(())
    }

    fn visit_expr(visit, ast, e: NodeId<Expr>) {
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
            Expr::Become(x) => visit.visit_become(ast, x),
            Expr::Call(x) => visit.visit_call(ast, x),
            Expr::Method(x) => visit.visit_method(ast, x),
            Expr::Field(x) => visit.visit_field_expr(ast, x),
            Expr::Index(x) => visit.visit_index(ast, x),
            Expr::Literal(x) => visit.visit_literal(ast, x),
            Expr::Symbol(x) => visit.visit_symbol(ast, x),
            Expr::Covered(x) => visit.visit_expr(ast, x),
        }
    }

    fn visit_block(visit, ast, b: NodeId<Block>)  {
        visit.visit_inner_block(ast, ast[b].body)
    }

    fn visit_inner_block( visit, ast, b: Option<NodeListId<Expr>>)  {
        let mut b = b;
        while let Some(f) = ast.next_list(&mut b) {
            visit.visit_expr(ast, f)?
        }

        Ok(())
    }

    fn visit_if(visit, ast, e: NodeId<If>)  {
        visit.visit_expr(ast, ast[e].condition)?;

        visit.visit_block(ast, ast[e].then)?;

        if let Some(x) = ast[e].otherwise {
            visit.visit_block(ast, x)?;
        }

        Ok(())
    }

    fn visit_loop(visit, ast, e: NodeId<Loop>)  {
        visit.visit_block(ast, ast[e].body)
    }

    fn visit_while(visit, ast, e: NodeId<While>)  {
        visit.visit_expr(ast, ast[e].condition)?;
        visit.visit_block(ast, ast[e].then)?;
        Ok(())
    }

    fn visit_return(visit, ast, e: NodeId<Return>)  {
        if let Some(x) = ast[e].expr {
            visit.visit_expr(ast, x)?;
        }
        Ok(())
    }

    fn visit_become(visit, ast, e: NodeId<Become>)  {
        visit.visit_ident(ast, ast[e].callee)?;

        let mut cur = ast[e].args;
        while let Some(x) = ast.next_list(&mut cur) {
            visit.visit_expr(ast, x)?;
        }
        Ok(())
    }

    fn visit_break(visit, ast, e: NodeId<Break>)  {
        if let Some(x) = ast[e].expr {
            visit.visit_expr(ast, x)?;
        }
        Ok(())
    }

    fn visit_let(visit, ast, e: NodeId<Let>)  {
        visit.visit_symbol(ast, ast[e].sym)?;
        visit.visit_expr(ast, ast[e].expr)?;
        Ok(())
    }

    fn visit_cast(visit, ast, e: NodeId<Cast>)  {
        visit.visit_expr(ast, ast[e].expr)?;
        visit.visit_type(ast, ast[e].ty)?;
        Ok(())
    }

    fn visit_binary(visit, ast, e: NodeId<BinaryExpr>)  {
        visit.visit_expr(ast, ast[e].left)?;
        visit.visit_expr(ast, ast[e].right)?;
        Ok(())
    }

    fn visit_unary(visit, ast, e: NodeId<UnaryExpr>)  {
        visit.visit_expr(ast, ast[e].expr)?;
        Ok(())
    }

    fn visit_field_expr(visit, ast, e: NodeId<FieldExpr>)  {
        visit.visit_expr(ast, ast[e].base)?;
        visit.visit_ident(ast, ast[e].field)?;
        Ok(())
    }

    fn visit_index(visit, ast, e: NodeId<Index>)  {
        visit.visit_expr(ast, ast[e].base)?;
        visit.visit_expr(ast, ast[e].index)?;
        Ok(())
    }

    fn visit_call(visit, ast, e: NodeId<Call>)  {
        visit.visit_expr(ast, ast[e].func)?;

        let mut cur = ast[e].args;
        while let Some(x) = ast.next_list(&mut cur) {
            visit.visit_expr(ast, x)?;
        }

        Ok(())
    }

    fn visit_method(visit, ast, e: NodeId<Method>)  {
        visit.visit_expr(ast, ast[e].receiver)?;
        visit.visit_ident(ast, ast[e].name)?;
        let mut cur = ast[e].args;
        while let Some(x) = ast.next_list(&mut cur) {
            visit.visit_expr(ast, x)?;
        }

        Ok(())
    }

    fn visit_symbol(visit, ast, s: NodeId<Symbol>)  {
        visit.visit_ident(ast, ast[s].name)
    }

    fn visit_type(visit, ast, s: NodeId<Type>)  {
        match ast[s]{
            //Type::Array(x) => visit.visit_type_array(ast,x),
            Type::Fn(x) => visit.visit_type_fn(ast,x),
            //Type::Tuple(x) => visit.visit_type_tuple(ast,x),
            Type::Ptr(x) => visit.visit_type_ptr(ast,x),
            //Type::Reference(x) => visit.visit_type_reference(ast,x),
            Type::Name(x) => visit.visit_type_name(ast,x),
        }
    }

    fn visit_type_array(visit, ast, s: NodeId<TypeArray>)  {
        visit.visit_type(ast,ast[s].elem)?;
        visit.visit_expr(ast,ast[s].len)
    }

    fn visit_type_fn(visit, ast, s: NodeId<TypeFn>)  {
        ast.iter_list_node(ast[s].params).try_for_each(|x|{
            visit.visit_type(ast,x)
        })?;
        if let Some(x) = ast[s].output {
            visit.visit_type(ast,x)?
        }
        Ok(())
    }

    fn visit_type_tuple(visit, ast, s: NodeId<TypeTuple>)  {
        ast.iter_list_node(ast[s].fields).try_for_each(|x|{
            visit.visit_type(ast,x)
        })?;
        Ok(())
    }

    fn visit_type_ptr(visit, ast, s: NodeId<TypePtr>)  {
        visit.visit_type(ast,ast[s].ty)?;
        Ok(())
    }

    fn visit_type_reference(visit, ast, s: NodeId<TypeReference>)  {
        visit.visit_type(ast,ast[s].ty)?;
        Ok(())
    }

    fn visit_type_name(visit, ast, s: NodeId<TypeName>){
        visit.visit_ident(ast, ast[s].name)?;
        Ok(())
    }

    fn visit_struct(visit, ast, s: NodeId<Struct>){
        visit.visit_type_name(ast, ast[s].name)?;
        ast.iter_list_node(ast[s].fields).try_for_each(|field|{
            visit.visit_field(ast, field)
        })?;
        Ok(())
    }

    fn visit_field(visit, ast, s: NodeId<Field>){
        visit.visit_ident(ast, ast[s].name)?;
        visit.visit_type(ast, ast[s].ty)?;
        Ok(())
    }

    fn visit_continue(_visit, _ast, _s: Span)  { }
    fn visit_literal(_visit, _ast, _s: NodeId<Lit>)  { }
    fn visit_ident(_visit, _ast, _s: NodeId<Ident>)  {}
}
