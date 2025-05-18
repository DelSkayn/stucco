use crate::{
    Become, BinaryExpr, Block, Break, Call, Cast, Expr, Field, If, Index, Let, Method, Module,
    NodeId, NodeListId, Parameter, Return, Span, Stencil, Symbol, Type, TypeArray, TypeFn, TypePtr,
    TypeReference, TypeTuple, UnaryExpr, Variant, Variation, VariationConstant, VariationSlot,
    While,
};
use syn::{Ident, Lit};

macro_rules! implement_visitor{
	($(fn $name:ident($this:ident, $ast:ident, $node:ident: $node_ty:ty) -> $res:ident{
	    $($t:tt)*
	})*) => {
		pub trait Visit: Sized {
			type Error;

			$(
				fn $name(&mut self,$ast: &$crate::Ast, $node: $node_ty) -> $res<(), Self::Error>{
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
    fn visit_module(visit, ast, m: NodeId<Module>) -> Result{
        let mut cur = ast[m].functions;
        while let Some(f) = ast.next_list(&mut cur) {
            visit.visit_stencil(ast, f)?
        }
        Ok(())
    }


    fn visit_stencil(visit, ast, m: NodeId<Stencil>) -> Result{
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

    fn visit_variant(visit, ast, m: NodeId<Variant>) -> Result{
        let mut cur = ast[m].variations;
        while let Some(f) = ast.next_list(&mut cur) {
            visit.visit_variation(ast, f)?
        }
        Ok(())
    }

    fn visit_variation(visit, ast, m: NodeId<Variation>) -> Result{
        match ast[m] {
            Variation::Constant(node_id) => visit.visit_variation_constant(ast,node_id),
            Variation::Slot(node_id) => visit.visit_variation_slot(ast,node_id),
        }
    }

    fn visit_variation_constant(visit,ast,m: NodeId<VariationConstant>) -> Result{
        visit.visit_symbol(ast, ast[m].sym)
    }

    fn visit_variation_slot(visit,ast,m: NodeId<VariationSlot>) -> Result{
        visit.visit_symbol(ast, ast[m].sym)
    }

    fn visit_parameter(visit, ast, m: NodeId<Parameter>) -> Result{
        visit.visit_symbol(ast, ast[m].sym)?;
        visit.visit_type(ast, ast[m].ty)?;

        Ok(())
    }

    fn visit_expr(visit, ast, e: NodeId<Expr>) -> Result{
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
            Expr::Field(x) => visit.visit_field(ast, x),
            Expr::Index(x) => visit.visit_index(ast, x),
            Expr::Literal(x) => visit.visit_literal(ast, x),
            Expr::Symbol(x) => visit.visit_symbol(ast, x),
            Expr::Covered(x) => visit.visit_expr(ast, x),
        }
    }

    fn visit_block(visit, ast, b: NodeId<Block>) -> Result {
        visit.visit_inner_block(ast, ast[b].body)
    }

    fn visit_inner_block( visit, ast, b: Option<NodeListId<Expr>>) -> Result {
        let mut b = b;
        while let Some(f) = ast.next_list(&mut b) {
            visit.visit_expr(ast, f)?
        }

        Ok(())
    }

    fn visit_if(visit, ast, e: NodeId<If>) -> Result {
        visit.visit_expr(ast, ast[e].condition)?;

        visit.visit_block(ast, ast[e].then)?;

        if let Some(x) = ast[e].otherwise {
            visit.visit_block(ast, x)?;
        }

        Ok(())
    }

    fn visit_loop(visit, ast, e: NodeId<Block>) -> Result {
        visit.visit_block(ast, e)
    }

    fn visit_while(visit, ast, e: NodeId<While>) -> Result {
        visit.visit_expr(ast, ast[e].condition)?;
        visit.visit_block(ast, ast[e].then)?;
        Ok(())
    }

    fn visit_return(visit, ast, e: NodeId<Return>) -> Result {
        if let Some(x) = ast[e].expr {
            visit.visit_expr(ast, x)?;
        }
        Ok(())
    }

    fn visit_become(visit, ast, e: NodeId<Become>) -> Result {
        visit.visit_ident(ast, ast[e].callee)?;

        let mut cur = ast[e].args;
        while let Some(x) = ast.next_list(&mut cur) {
            visit.visit_expr(ast, x)?;
        }
        Ok(())
    }

    fn visit_break(visit, ast, e: NodeId<Break>) -> Result {
        if let Some(x) = ast[e].expr {
            visit.visit_expr(ast, x)?;
        }
        Ok(())
    }

    fn visit_let(visit, ast, e: NodeId<Let>) -> Result {
        visit.visit_symbol(ast, ast[e].sym)?;
        visit.visit_expr(ast, ast[e].expr)?;
        Ok(())
    }

    fn visit_cast(visit, ast, e: NodeId<Cast>) -> Result {
        visit.visit_expr(ast, ast[e].expr)?;
        visit.visit_type(ast, ast[e].ty)?;
        Ok(())
    }

    fn visit_binary(visit, ast, e: NodeId<BinaryExpr>) -> Result {
        visit.visit_expr(ast, ast[e].left)?;
        visit.visit_expr(ast, ast[e].right)?;
        Ok(())
    }

    fn visit_unary(visit, ast, e: NodeId<UnaryExpr>) -> Result {
        visit.visit_expr(ast, ast[e].expr)?;
        Ok(())
    }

    fn visit_field(visit, ast, e: NodeId<Field>) -> Result {
        visit.visit_expr(ast, ast[e].base)?;
        visit.visit_ident(ast, ast[e].field)?;
        Ok(())
    }

    fn visit_index(visit, ast, e: NodeId<Index>) -> Result {
        visit.visit_expr(ast, ast[e].base)?;
        visit.visit_expr(ast, ast[e].index)?;
        Ok(())
    }

    fn visit_call(visit, ast, e: NodeId<Call>) -> Result {
        visit.visit_expr(ast, ast[e].func)?;

        let mut cur = ast[e].args;
        while let Some(x) = ast.next_list(&mut cur) {
            visit.visit_expr(ast, x)?;
        }

        Ok(())
    }

    fn visit_method(visit, ast, e: NodeId<Method>) -> Result {
        visit.visit_expr(ast, ast[e].receiver)?;
        visit.visit_ident(ast, ast[e].name)?;
        let mut cur = ast[e].args;
        while let Some(x) = ast.next_list(&mut cur) {
            visit.visit_expr(ast, x)?;
        }

        Ok(())
    }

    fn visit_symbol(visit, ast, s: NodeId<Symbol>) -> Result {
        visit.visit_ident(ast, ast[s].name)
    }

    fn visit_type(visit, ast, s: NodeId<Type>) -> Result {
        match ast[s]{
            Type::Array(x) => visit.visit_type_array(ast,x),
            Type::Fn(x) => visit.visit_type_fn(ast,x),
            Type::Tuple(x) => visit.visit_type_tuple(ast,x),
            Type::Ptr(x) => visit.visit_type_ptr(ast,x),
            Type::Reference(x) => visit.visit_type_reference(ast,x),
            Type::Direct(x) => visit.visit_ident(ast,x),
        }
    }

    fn visit_type_array(visit, ast, s: NodeId<TypeArray>) -> Result {
        visit.visit_type(ast,ast[s].elem)?;
        visit.visit_expr(ast,ast[s].len)
    }

    fn visit_type_fn(visit, ast, s: NodeId<TypeFn>) -> Result {
        ast.iter_list_node(ast[s].params).try_for_each(|x|{
            visit.visit_type(ast,x)
        })?;
        if let Some(x) = ast[s].output {
            visit.visit_type(ast,x)?
        }
        Ok(())
    }

    fn visit_type_tuple(visit, ast, s: NodeId<TypeTuple>) -> Result {
        ast.iter_list_node(ast[s].fields).try_for_each(|x|{
            visit.visit_type(ast,x)
        })?;
        Ok(())
    }

    fn visit_type_ptr(visit, ast, s: NodeId<TypePtr>) -> Result {
        visit.visit_type(ast,ast[s].ty)?;
        Ok(())
    }

    fn visit_type_reference(visit, ast, s: NodeId<TypeReference>) -> Result {
        visit.visit_type(ast,ast[s].ty)?;
        Ok(())
    }

    fn visit_continue(_visit, _ast, _s: Span) -> Result { }
    fn visit_literal(_visit, _ast, _s: NodeId<Lit>) -> Result { }
    fn visit_ident(_visit, _ast, _s: NodeId<Ident>) -> Result {}
}
