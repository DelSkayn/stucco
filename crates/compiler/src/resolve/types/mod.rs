pub mod pass;

use ast::NodeId;
use common::{
    id,
    id::{IdRange, IndexMap, PartialIndexMap},
};
use std::collections::HashMap;
use token::{Span, token::Ident};

id!(TypeId);
id!(ChildTypeId);

#[derive(Clone, Copy, Eq, PartialEq, Hash)]
pub enum TypeKind {
    Struct {
        name: NodeId<ast::TypeName>,
        fields: Option<IdRange<ChildTypeId>>,
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
        args: Option<IdRange<ChildTypeId>>,
        output: Option<TypeId>,
    },
}

pub struct Type {
    kind: TypeKind,
    declare: Option<NodeId<ast::TypeName>>,
}

pub struct TypeTable {
    types: IndexMap<TypeId, Type>,
    child_types: IndexMap<ChildTypeId, TypeId>,
    ast_to_type: PartialIndexMap<NodeId<ast::TypeName>, TypeId>,
    name_to_type: HashMap<NodeId<Ident>, TypeId>,
    predefined: HashMap<Ident, TypeId>,
}

impl TypeTable {
    pub fn new() -> Self {
        let mut res = TypeTable {
            types: IndexMap::new(),
            child_types: IndexMap::new(),
            ast_to_type: PartialIndexMap::new(),
            name_to_type: HashMap::new(),
            predefined: HashMap::new(),
        };

        res.insert_predefined("usize", TypeKind::Usize);
        res.insert_predefined("u64", TypeKind::U64);
        res.insert_predefined("bool", TypeKind::Bool);

        res
    }

    fn insert_predefined(&mut self, name: &str, ty: TypeKind) {
        let ty = self.types.push_expect(Type {
            kind: ty,
            declare: None,
        });
        self.predefined
            .insert(Ident::new(name, Span::call_site().into()), ty);
    }
}
