pub mod pass;

use ast::NodeId;
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
    types: IdSet<TypeId, Type>,
    type_tuples: IdSet<TypeTupleId, TypeTupleEntry>,
    type_fields: IdSet<TypeFieldId, TypeFieldEntry>,

    field_name_to_type: HashMap<(TypeDeclId, NodeId<Ident>), TypeId>,

    declarations: IndexMap<TypeDeclId, TypeDecl>,
    ast_to_type: PartialIndexMap<NodeId<ast::Type>, TypeId>,
    ast_name_to_type: PartialIndexMap<NodeId<ast::TypeName>, TypeDeclId>,
    ast_fn_to_type: PartialIndexMap<NodeId<ast::Function>, TypeId>,
    name_to_type: HashMap<NodeId<Ident>, TypeDeclId>,
    predefined: HashMap<Ident, TypeDeclId>,
}

impl TypeTable {
    pub const NIL_ID: TypeId = unsafe { TypeId::from_u32_unchecked(0) };
    pub const NEVER_ID: TypeId = unsafe { TypeId::from_u32_unchecked(1) };
    pub const BOOL_ID: TypeId = unsafe { TypeId::from_u32_unchecked(2) };

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
        };

        assert_eq!(res.types.push_expect(Type::Nil), Self::NIL_ID);
        assert_eq!(res.types.push_expect(Type::Never), Self::NEVER_ID);
        assert_eq!(res.insert_predefined("bool", Type::Bool), Self::BOOL_ID);
        res.insert_predefined("usize", Type::Usize);
        res.insert_predefined("u64", Type::U64);

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
}
