use std::cell::Cell;

use ast::{
    Ast, AstSpanned, Block, Expr, Method, NodeId, TypeName,
    visit::{self, Visit},
};
use common::{id::IndexMap, iter::IterExt};
use token::token::{Lit, LitIntSuffix};

use crate::{
    resolve::SymbolTable,
    type_check::{Narrowing, PrimTy, Ty, TyId, TypeError, Types},
};

pub struct DeclarePass<'a> {
    ty: &'a mut Types,
    names: IndexMap<NodeId<TypeName>, Option<TyId>>,
}

impl Visit for DeclarePass<'_> {
    type Error = ();

    fn visit_stmt(&mut self, ast: &ast::Ast, s: NodeId<ast::Stmt>) -> Result<(), Self::Error> {
        let s = match ast[s] {
            ast::Stmt::Struct(s) => s,
            ast::Stmt::Stencil(_) | ast::Stmt::Function(_) => return Ok(()),
        };
        self.names
            .insert_fill_default(ast[s].name, Some(self.ty.new_type_var(Narrowing::None)));

        Ok(())
    }
}

pub struct ResolveNamePass<'a> {
    ty: &'a mut Types,
}

pub struct InferUpPass<'a> {
    symbols: &'a SymbolTable,
    ty: &'a mut Types,
    function_stack: Vec<NodeId<Expr>>,
    loop_block_stack: Vec<NodeId<Block>>,
    stencil_type: TyId,
}

impl<'a> InferUpPass<'a> {
    pub fn new(symbols: &'a SymbolTable, types: &'a mut Types) -> Self {
        let stencil_type = types
            .type_graph
            .push(Ty::Var(Narrowing::None, Cell::new(None)))
            .unwrap();

        InferUpPass {
            symbols,
            ty: types,
            function_stack: Vec::new(),
            loop_block_stack: Vec::new(),
            stencil_type,
        }
    }

    fn can_cast(&mut self, from: TyId, to: TyId) -> Result<bool, TypeError> {
        let from = self.ty.find_type(from);
        let to = self.ty.find_type(to);

        if matches!(self.ty.type_graph[from], Ty::Var(..)) {
            return Err(TypeError::TypeAnnotationRequired(from));
        }

        if matches!(self.ty.type_graph[to], Ty::Var(..)) {
            return Err(TypeError::TypeAnnotationRequired(from));
        }

        if self.ty.same_type(from, to).unwrap() {
            return Ok(true);
        }

        match self.ty.type_graph[from] {
            Ty::Prim(prim_ty) => {
                match prim_ty {
                    // Nil can only cast to itself and we already found that that we don't cast to
                    // the same type.
                    PrimTy::Nil => Ok(false),
                    PrimTy::Bool => match self.ty.type_graph[to] {
                        Ty::Prim(prim_ty) => match prim_ty {
                            PrimTy::Bool
                            | PrimTy::Usize
                            | PrimTy::Isize
                            | PrimTy::U64
                            | PrimTy::I64
                            | PrimTy::U32
                            | PrimTy::I32
                            | PrimTy::U16
                            | PrimTy::I16
                            | PrimTy::U8
                            | PrimTy::I8
                            | PrimTy::F32
                            | PrimTy::F64 => Ok(true),
                            _ => Ok(false),
                        },
                        _ => Ok(false),
                    },
                    PrimTy::Usize => match self.ty.type_graph[to] {
                        Ty::Prim(prim_ty) => match prim_ty {
                            PrimTy::Usize
                            | PrimTy::Isize
                            | PrimTy::U64
                            | PrimTy::I64
                            | PrimTy::U32
                            | PrimTy::I32
                            | PrimTy::U16
                            | PrimTy::I16
                            | PrimTy::U8
                            | PrimTy::I8
                            | PrimTy::F32
                            | PrimTy::F64 => Ok(true),
                            _ => Ok(false),
                        },
                        Ty::Ptr(_) | Ty::PtrMut(_) => Ok(true),
                        _ => Ok(false),
                    },
                    PrimTy::Isize
                    | PrimTy::U64
                    | PrimTy::I64
                    | PrimTy::U32
                    | PrimTy::I32
                    | PrimTy::U16
                    | PrimTy::I16
                    | PrimTy::U8
                    | PrimTy::I8
                    | PrimTy::F32
                    | PrimTy::F64 => match self.ty.type_graph[to] {
                        Ty::Prim(prim_ty) => match prim_ty {
                            PrimTy::Usize
                            | PrimTy::Isize
                            | PrimTy::U64
                            | PrimTy::I64
                            | PrimTy::U32
                            | PrimTy::I32
                            | PrimTy::U16
                            | PrimTy::I16
                            | PrimTy::U8
                            | PrimTy::I8
                            | PrimTy::F32
                            | PrimTy::F64 => Ok(true),
                            _ => Ok(false),
                        },
                        _ => Ok(false),
                    },
                    PrimTy::Diverges => todo!(),
                }
            }
            Ty::Ptr(_) | Ty::PtrMut(_) => match self.ty.type_graph[to] {
                Ty::Prim(prim_ty) => match prim_ty {
                    PrimTy::Usize => Ok(true),
                    _ => Ok(false),
                },
                Ty::Ptr(_) | Ty::PtrMut(_) => Ok(true),
                _ => Ok(false),
            },
            _ => Ok(false),
        }
    }

    fn type_from_ast(&mut self, ast: &ast::Ast, ty: NodeId<ast::Type>) -> Result<TyId, TypeError> {
        match ast[ty] {
            ast::Type::Array(_) => todo!(),
            ast::Type::Fn(n) => {
                let args = ast
                    .iter_list_node(ast[n].params)
                    .map(|x| self.type_from_ast(ast, x))
                    .collect::<Result<Vec<TyId>, TypeError>>()?;
                let output = ast[n]
                    .output
                    .map(|x| self.type_from_ast(ast, x))
                    .transpose()?
                    .unwrap_or(Types::NIL_ID);
                return Ok(self.ty.type_graph.push(Ty::Fn(args, output)).unwrap());
            }
            ast::Type::Tuple(_) => todo!(),
            ast::Type::Ptr(node_id) => {
                let inner = self.type_from_ast(ast, ast[node_id].ty)?;
                if ast[node_id].mutable {
                    return Ok(self.ty.type_graph.push(Ty::PtrMut(inner)).unwrap());
                } else {
                    return Ok(self.ty.type_graph.push(Ty::Ptr(inner)).unwrap());
                }
            }
            ast::Type::Reference(_) => todo!(),
            ast::Type::Name(node_id) => {
                let ident = node_id.index(ast).name.index(ast);
                if ident == "f32" {
                    return Ok(Types::F32_ID);
                }
                if ident == "f64" {
                    return Ok(Types::F64_ID);
                }
                if ident == "usize" {
                    return Ok(Types::USIZE_ID);
                }
                if ident == "isize" {
                    return Ok(Types::ISIZE_ID);
                }
            }
        }
        let span = ast[ty].ast_span(ast);
        Err(TypeError::UnknownType(ty, span))
    }

    // Quick and dirty way to resolve intrinisic methods.
    fn resolve_method(
        &mut self,
        ast: &Ast,
        method: NodeId<Method>,
        receiver_type: TyId,
    ) -> Result<TyId, TypeError> {
        let name = method.index(ast).name.index(ast);
        let ty = self.ty.find_type(receiver_type);

        match self.ty.type_graph[ty] {
            Ty::Prim(_) => {}
            Ty::Ptr(ty_id) => {
                if name == "add" || name == "sub" {
                    let args = ast[method].args;
                    let [count] = ast
                        .iter_list_node(args)
                        .gather()
                        .ok_or(TypeError::InvalidArity(method, 1))?;
                    let ty = self.ty.expr_to_type[count].unwrap();
                    self.ty.unify(ty, Types::USIZE_ID)?;

                    return Ok(receiver_type);
                }
                if name == "read" {
                    let args = ast[method].args;
                    let [] = ast
                        .iter_list_node(args)
                        .gather()
                        .ok_or(TypeError::InvalidArity(method, 0))?;

                    return Ok(ty_id);
                }
            }
            Ty::PtrMut(ty_id) => {
                if name == "add" || name == "sub" {
                    let args = ast[method].args;
                    let [count] = ast
                        .iter_list_node(args)
                        .gather()
                        .ok_or(TypeError::InvalidArity(method, 1))?;
                    let ty = self.ty.expr_to_type[count].unwrap();
                    self.ty.unify(ty, Types::USIZE_ID)?;

                    return Ok(receiver_type);
                }
                if name == "read" {
                    let args = ast[method].args;
                    let [] = ast
                        .iter_list_node(args)
                        .gather()
                        .ok_or(TypeError::InvalidArity(method, 0))?;

                    return Ok(ty_id);
                }

                if name == "write" {
                    let args = ast[method].args;
                    let [value] = ast
                        .iter_list_node(args)
                        .gather()
                        .ok_or(TypeError::InvalidArity(method, 1))?;
                    let ty = self.ty.expr_to_type[value].unwrap();
                    self.ty.unify(ty, ty_id)?;
                    return Ok(Types::NIL_ID);
                }
            }
            Ty::Ref(x) => return self.resolve_method(ast, method, x),
            Ty::RefMut(x) => return self.resolve_method(ast, method, x),
            Ty::Tuple(_) => {}
            Ty::Fn(_, _) => {}
            Ty::Array(_, _) | Ty::Struct(_) => todo!(),
            Ty::Var(_, _) => return Err(TypeError::TypeMustBeKnown(method)),
        }
        return Err(TypeError::UnknownMethod(
            method.index(ast).name,
            receiver_type,
        ));
    }
}

impl<'a> Visit for InferUpPass<'a> {
    type Error = TypeError;

    fn visit_block(&mut self, ast: &ast::Ast, b: NodeId<ast::Block>) -> Result<(), Self::Error> {
        let mut last_ty = None;
        let mut next = ast[b].body;
        let mut diverges = false;

        while let Some(f) = ast.next_list(&mut next) {
            self.visit_expr(ast, f)?;
            let t = self.ty.expr_to_type[f].unwrap();
            if self.ty.find_type(t) == Types::DIVERGES_ID {
                diverges = true;
            }
            last_ty = Some(t);
        }

        let block_ty = if diverges {
            Types::DIVERGES_ID
        } else if ast[b].returns_last {
            last_ty.unwrap_or(Types::NIL_ID)
        } else {
            Types::NIL_ID
        };

        self.ty.block_type.insert_fill_default(b, Some(block_ty));

        Ok(())
    }

    fn visit_stencil(
        &mut self,
        ast: &ast::Ast,
        m: NodeId<ast::Stencil>,
    ) -> Result<(), Self::Error> {
        for p in ast.iter_list_node(ast[m].parameters) {
            let ty = self.type_from_ast(ast, ast[p].ty)?;
            let sym_id = self.symbols.ast_to_symbol[ast[p].sym];
            self.ty.symbol_to_type.insert_fill_default(sym_id, Some(ty));
        }

        visit::visit_stencil(self, ast, m)
    }

    fn visit_expr(&mut self, ast: &ast::Ast, e: NodeId<ast::Expr>) -> Result<(), Self::Error> {
        match ast[e] {
            ast::Expr::If(node_id) => {
                visit::visit_if(self, ast, node_id)?;
                let _if = &ast[node_id];
                // Condition must be a boolean.
                self.ty
                    .unify(self.ty.expr_to_type[_if.condition].unwrap(), Types::BOOL_ID)?;

                // if there is an `else` then all blocks must have the same type.
                // Otherwise they must all be ().
                if let Some(_else) = _if.otherwise {
                    // the if and else need to be the same type.
                    self.ty.unify(
                        self.ty.block_type[_if.then].unwrap(),
                        self.ty.block_type[_else].unwrap(),
                    )?;
                    // this expression has the same type as the if or else block.
                    self.ty
                        .expr_to_type
                        .insert_fill_default(e, self.ty.block_type[_if.then]);
                } else {
                    // No else so this must be NIL
                    self.ty
                        .unify(self.ty.block_type[_if.then].unwrap(), Types::NIL_ID)?;
                    self.ty
                        .expr_to_type
                        .insert_fill_default(e, Some(Types::NIL_ID));
                }
            }
            ast::Expr::Binary(n) => {
                visit::visit_binary(self, ast, n)?;
                let left = ast[n].left;
                let right = ast[n].right;
                self.ty.unify(
                    self.ty.expr_to_type[left].unwrap(),
                    self.ty.expr_to_type[right].unwrap(),
                )?;

                match ast[n].op {
                    ast::BinOp::Add
                    | ast::BinOp::Sub
                    | ast::BinOp::Mull
                    | ast::BinOp::Div
                    | ast::BinOp::Rem
                    | ast::BinOp::Shl
                    | ast::BinOp::Shr
                    | ast::BinOp::BitXor
                    | ast::BinOp::BitAnd
                    | ast::BinOp::BitOr => {
                        let number_ty = self.ty.new_type_var(Narrowing::Integer);
                        self.ty
                            .unify(self.ty.expr_to_type[left].unwrap(), number_ty)?;
                        self.ty.expr_to_type.insert_fill_default(e, Some(number_ty));
                    }
                    ast::BinOp::And
                    | ast::BinOp::Or
                    | ast::BinOp::Eq
                    | ast::BinOp::Lt
                    | ast::BinOp::Le
                    | ast::BinOp::Ne
                    | ast::BinOp::Ge
                    | ast::BinOp::Gt => {
                        self.ty
                            .expr_to_type
                            .insert_fill_default(e, Some(Types::BOOL_ID));
                    }
                    ast::BinOp::Assign
                    | ast::BinOp::AddAssign
                    | ast::BinOp::SubAssign
                    | ast::BinOp::MullAssign
                    | ast::BinOp::DivAssign
                    | ast::BinOp::RemAssign
                    | ast::BinOp::BitXorAssign
                    | ast::BinOp::BitAndAssign
                    | ast::BinOp::BitOrAssign
                    | ast::BinOp::ShlAssign
                    | ast::BinOp::ShrAssign => {
                        self.ty
                            .expr_to_type
                            .insert_fill_default(e, Some(Types::NIL_ID));
                    }
                }
            }
            ast::Expr::Unary(n) => {
                visit::visit_unary(self, ast, n)?;
                let n = ast[n].expr;
                self.ty
                    .expr_to_type
                    .insert_fill_default(e, self.ty.expr_to_type[n]);
            }
            ast::Expr::Block(node_id) => {
                self.visit_block(ast, node_id)?;
                self.ty
                    .expr_to_type
                    .insert_fill_default(e, Some(self.ty.block_type[node_id].unwrap()));
            }
            ast::Expr::Cast(c) => {
                self.visit_expr(ast, ast[c].expr)?;
                let from = self.ty.expr_to_type[ast[c].expr].unwrap();
                let to = self.type_from_ast(ast, ast[c].ty)?;
                if !self.can_cast(from, to)? {
                    return Err(TypeError::InvalidCast { from, to });
                }
                self.ty.expr_to_type.insert_fill_default(e, Some(to));
            }
            ast::Expr::Loop(n) => {
                let block = ast[n].body;
                let ty_id = self.ty.new_type_var(Narrowing::None);
                self.ty.block_type.insert_fill_default(block, Some(ty_id));
                self.loop_block_stack.push(block);
                visit::visit_inner_block(self, ast, ast[block].body)?;
                self.loop_block_stack.pop();
                self.ty.expr_to_type.insert_fill_default(e, Some(ty_id));
            }
            ast::Expr::While(n) => {
                visit::visit_expr(self, ast, ast[n].condition)?;
                self.ty.unify(
                    self.ty.expr_to_type[ast[n].condition].unwrap(),
                    Types::BOOL_ID,
                )?;

                let block = ast[n].then;
                let ty_id = self.ty.new_type_var(Narrowing::None);
                self.ty.block_type.insert_fill_default(block, Some(ty_id));
                self.loop_block_stack.push(block);
                visit::visit_inner_block(self, ast, ast[block].body)?;
                self.loop_block_stack.pop();
            }
            ast::Expr::Let(n) => {
                // TODO: Type declarations
                let expr = ast[n].expr;
                let symbol = self.symbols.ast_to_symbol[ast[n].sym];
                self.visit_let(ast, n)?;
                if let Some(x) = self.ty.symbol_to_type.get(symbol).copied().flatten() {
                    self.ty.unify(x, self.ty.expr_to_type[expr].unwrap())?;
                } else {
                    self.ty
                        .symbol_to_type
                        .insert_fill_default(symbol, self.ty.expr_to_type[expr]);
                };
                self.ty
                    .expr_to_type
                    .insert_fill_default(e, Some(Types::NIL_ID));
            }
            ast::Expr::Continue(_) => {}
            ast::Expr::Break(n) => {
                visit::visit_break(self, ast, n)?;
                let ty = if let Some(expr) = ast[n].expr {
                    self.ty.expr_to_type[expr].unwrap()
                } else {
                    Types::NIL_ID
                };
                let block_ty =
                    self.ty.block_type[self.loop_block_stack.last().copied().unwrap()].unwrap();
                self.ty.unify(ty, block_ty)?;
                self.ty
                    .expr_to_type
                    .insert_fill_default(e, Some(Types::DIVERGES_ID));
            }
            ast::Expr::Return(n) => {
                visit::visit_return(self, ast, n)?;
                if let Some(expr) = ast[n].expr {
                    let ty = self.ty.expr_to_type[expr].unwrap();
                    self.ty.unify(self.stencil_type, ty)?;
                } else {
                    self.ty.unify(self.stencil_type, Types::NIL_ID)?;
                }
                self.ty
                    .expr_to_type
                    .insert_fill_default(e, Some(Types::DIVERGES_ID));
            }
            ast::Expr::Become(n) => {
                visit::visit_become(self, ast, n)?;
                self.ty
                    .expr_to_type
                    .insert_fill_default(e, Some(Types::DIVERGES_ID));
            }
            ast::Expr::Call(n) => {
                visit::visit_call(self, ast, n)?;
                let args = ast
                    .iter_list_node(ast[n].args)
                    .map(|x| self.ty.expr_to_type[x].unwrap())
                    .collect();
                let out = self.ty.new_type_var(Narrowing::None);
                let expected_ty = self.ty.type_graph.push(Ty::Fn(args, out)).unwrap();
                let func_ty = self.ty.expr_to_type[ast[n].func].unwrap();
                self.ty.unify(expected_ty, func_ty)?;
                self.ty.expr_to_type.insert_fill_default(e, Some(out));
            }
            ast::Expr::Method(n) => {
                visit::visit_method(self, ast, n)?;
                let recv_type = self.ty.expr_to_type[ast[n].receiver].unwrap();
                let ty = self.resolve_method(ast, n, recv_type)?;
                self.ty.expr_to_type.insert_fill_default(e, Some(ty))
            }
            ast::Expr::Field(_) => todo!(),
            ast::Expr::Index(_) => todo!(),
            ast::Expr::Literal(node_id) => match ast[node_id] {
                Lit::Int(ref lit_int) => match lit_int.suffix() {
                    LitIntSuffix::I64 => self
                        .ty
                        .expr_to_type
                        .insert_fill_default(e, Some(Types::I64_ID)),
                    LitIntSuffix::U64 => self
                        .ty
                        .expr_to_type
                        .insert_fill_default(e, Some(Types::U64_ID)),
                    LitIntSuffix::Usize => self
                        .ty
                        .expr_to_type
                        .insert_fill_default(e, Some(Types::USIZE_ID)),
                    LitIntSuffix::Isize => self
                        .ty
                        .expr_to_type
                        .insert_fill_default(e, Some(Types::ISIZE_ID)),
                    LitIntSuffix::None => {
                        let ty_var = self
                            .ty
                            .type_graph
                            .push(Ty::Var(Narrowing::Integer, Cell::new(None)))
                            .unwrap();
                        self.ty.expr_to_type.insert_fill_default(e, Some(ty_var));
                    }
                },
                Lit::Bool(_) => self
                    .ty
                    .expr_to_type
                    .insert_fill_default(e, Some(Types::BOOL_ID)),
                _ => todo!(),
            },
            ast::Expr::Symbol(node_id) => {
                let symbol = self.symbols.ast_to_symbol[node_id];
                let ty_id = self
                    .ty
                    .symbol_to_type
                    .get(symbol)
                    .copied()
                    .flatten()
                    .expect("undefined symbol");
                self.ty.expr_to_type.insert_fill_default(e, Some(ty_id));
            }
            ast::Expr::Covered(node_id) => {
                self.visit_expr(ast, node_id)?;
                self.ty
                    .expr_to_type
                    .insert_fill_default(e, Some(self.ty.expr_to_type[node_id].unwrap()));
            }
        }

        Ok(())
    }
}
