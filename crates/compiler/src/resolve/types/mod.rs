pub mod pass;

use ast::{Ast, NodeId};
use common::{
    id,
    id::{IdSet, IndexMap, PartialIndexMap},
};
use std::collections::HashMap;
use token::{Span, token::Ident};

id!(TypeId);
id!(TypeTupleId);
id!(TypeFieldId);
id!(TypeDeclId);

#[derive(Clone, Copy, Eq, PartialEq, Hash)]
pub enum Type {
    Struct {
        // Declaration, to make every struct unique.
        decl: TypeDeclId,
        fields: Option<TypeFieldId>,
    },
    Ptr {
        to: TypeId,
    },
    PtrMut {
        to: TypeId,
    },
    Usize,
    U64,
    Bool,
    Nil,
    Never,
    Fn {
        args: Option<TypeTupleId>,
        output: TypeId,
    },
    Decl {
        decl: TypeDeclId,
    },
}

pub struct TypeDecl {
    ty: TypeId,
    declare: Option<NodeId<ast::TypeName>>,
}

#[derive(Clone, Copy, Eq, PartialEq, Hash)]
pub struct TypeTupleEntry {
    ty: TypeId,
    next: Option<TypeTupleId>,
}

#[derive(Clone, Copy, Eq, PartialEq, Hash)]
pub struct TypeFieldEntry {
    name: NodeId<Ident>,
    ty: TypeId,
    next: Option<TypeFieldId>,
}

pub struct TypeTable {
    pub types: IdSet<TypeId, Type>,
    pub type_tuples: IdSet<TypeTupleId, TypeTupleEntry>,
    pub type_fields: IdSet<TypeFieldId, TypeFieldEntry>,

    pub field_name_to_type: HashMap<(TypeDeclId, NodeId<Ident>), TypeId>,

    pub declarations: IndexMap<TypeDeclId, TypeDecl>,
    pub ast_to_type: PartialIndexMap<NodeId<ast::Type>, TypeId>,
    pub ast_name_to_type: PartialIndexMap<NodeId<ast::TypeName>, TypeDeclId>,
    pub ast_fn_to_type: PartialIndexMap<NodeId<ast::Function>, TypeId>,
    pub name_to_type: HashMap<NodeId<Ident>, TypeDeclId>,
    pub predefined: HashMap<Ident, TypeDeclId>,
    pub definition: Option<(NodeId<ast::ModuleDefinition>, TypeId)>,

    pub expr_to_ty: IndexMap<NodeId<ast::Expr>, TypeId>,
}

impl TypeId {
    pub const NIL: TypeId = unsafe { TypeId::from_u32_unchecked(0) };
    pub const NEVER: TypeId = unsafe { TypeId::from_u32_unchecked(1) };
    pub const BOOL: TypeId = unsafe { TypeId::from_u32_unchecked(2) };
    pub const U64: TypeId = unsafe { TypeId::from_u32_unchecked(3) };
}

impl TypeTable {
    pub fn new() -> Self {
        let mut res = TypeTable {
            types: IdSet::new(),
            type_tuples: IdSet::new(),
            type_fields: IdSet::new(),
            field_name_to_type: HashMap::new(),
            declarations: IndexMap::new(),
            ast_to_type: PartialIndexMap::new(),
            ast_name_to_type: PartialIndexMap::new(),
            ast_fn_to_type: PartialIndexMap::new(),
            name_to_type: HashMap::new(),
            predefined: HashMap::new(),
            definition: None,

            expr_to_ty: IndexMap::new(),
        };

        assert_eq!(res.types.push_expect(Type::Nil), TypeId::NIL);
        assert_eq!(res.types.push_expect(Type::Never), TypeId::NEVER);
        assert_eq!(res.insert_predefined("bool", Type::Bool), TypeId::BOOL);
        assert_eq!(res.insert_predefined("u64", Type::U64), TypeId::U64);
        //res.insert_predefined("usize", Type::Usize);

        res
    }

    fn insert_predefined(&mut self, name: &str, ty: Type) -> TypeId {
        let ty = self.types.push_expect(ty);
        let decl = self
            .declarations
            .push_expect(TypeDecl { ty, declare: None });
        self.predefined
            .insert(Ident::new(name, Span::call_site().into()), decl);
        ty
    }

    pub fn coerces_to(&self, from: TypeId, _to: TypeId) -> bool {
        if from == TypeId::NEVER {
            return true;
        }

        false
    }

    pub fn format_type(&self, ast: &Ast, ty: TypeId) -> String {
        let mut res = String::new();
        self.format_type_inner(ast, ty, &mut res);
        res
    }

    fn format_type_inner(&self, ast: &Ast, ty: TypeId, res: &mut String) {
        use std::fmt::Write;

        match self.types[ty] {
            Type::Struct { decl, .. } => {
                if let Some(x) = self.declarations[decl].declare {
                    let _ = write!(res, "{}", x.index(ast).name.index(ast));
                } else {
                    todo!()
                }
            }
            Type::Ptr { to } => {
                res.push_str("*const ");
                self.format_type_inner(ast, to, res);
            }
            Type::PtrMut { to } => {
                res.push_str("*mut ");
                self.format_type_inner(ast, to, res);
            }
            Type::Usize => res.push_str("usize"),
            Type::U64 => res.push_str("u64"),
            Type::Bool => res.push_str("bool"),
            Type::Nil => res.push_str("()"),
            Type::Never => res.push('!'),
            Type::Fn { args, output } => {
                res.push_str("fn(");
                let mut cur = args;
                let mut buffer = Vec::new();
                while let Some(c) = cur {
                    buffer.push(self.type_tuples[c].ty);
                    cur = self.type_tuples[c].next;
                }

                for (idx, arg) in buffer.into_iter().rev().enumerate() {
                    if idx != 0 {
                        res.push(',')
                    }
                    self.format_type_inner(ast, arg, res);
                }
                res.push(')');
                if output != TypeId::NIL {
                    res.push_str(" -> ");
                    self.format_type_inner(ast, output, res);
                }
            }
            Type::Decl { decl } => {
                if let Some(x) = self.declarations[decl].declare {
                    let _ = write!(res, "{}", x.index(ast).name.index(ast));
                } else {
                    todo!()
                }
            }
        }
    }
}
