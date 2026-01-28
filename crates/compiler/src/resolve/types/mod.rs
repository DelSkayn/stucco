pub mod pass;

use ast::{Ast, NodeId};
use common::{
    id,
    id::{IdSet, IndexMap, PartialIndexMap},
};
use std::collections::HashMap;
use token::{Span, token::Ident};

use crate::resolve::SymbolId;

id!(TypeId);
id!(TypeTupleId);
id!(TypeFieldId);
id!(TypeDeclId);

id!(GenericId);

/// Actual type definition.
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
    /// Result a statement with a semicolon.
    Nil,
    /// Result of a diverging operation.
    Never,
    Fn {
        args: Option<TypeTupleId>,
        output: TypeId,
    },
    /// Used for type names, refers to a declared type,
    /// Not used for builtins.
    Decl {
        decl: TypeDeclId,
    },
    // Not a real type used for when the type result of an expression is ignored.
    // All types coerce to ignored.
    Ignore,
}

/// Data about a declared type.
pub enum TypeDecl {
    Builtin(TypeId),
    Template {
        id: GenericId,
        declared: NodeId<ast::TypeName>,
        /// Is this type shadowing another type.
        shadowed: Option<TypeDeclId>,
    },
    Defined {
        ty: TypeId,
        declared: NodeId<ast::TypeName>,
        /// Is this type shadowing another type.
        shadowed: Option<TypeDeclId>,
    },
}

#[derive(Clone, Copy, Eq, PartialEq, Hash)]
pub struct TypeTupleEntry {
    pub ty: TypeId,
    pub next: Option<TypeTupleId>,
}

#[derive(Clone, Copy, Eq, PartialEq, Hash)]
pub struct TypeFieldEntry {
    pub name: NodeId<Ident>,
    pub ty: TypeId,
    pub next: Option<TypeFieldId>,
}

pub struct TypeTable {
    pub types: IdSet<TypeId, Type>,
    pub type_tuples: IdSet<TypeTupleId, TypeTupleEntry>,
    pub type_fields: IdSet<TypeFieldId, TypeFieldEntry>,

    /// Hashmap for looking up the type of fields of a struct
    /// Key format is `TypeDeclId` of the struct + NodeId of the name of the field.
    pub field_name_to_type: HashMap<(TypeDeclId, NodeId<Ident>), TypeId>,

    /// Types which are declared and not infered, builtin + struct + generic.
    pub declarations: IndexMap<TypeDeclId, TypeDecl>,

    /// Maps from the ast to a type.
    pub ast_to_type: PartialIndexMap<NodeId<ast::Type>, TypeId>,
    pub ast_name_to_type: PartialIndexMap<NodeId<ast::TypeName>, TypeDeclId>,
    pub ast_fn_to_type: PartialIndexMap<NodeId<ast::Function>, TypeId>,

    /// Map for predefined names to a specific declared type.
    pub predefined: HashMap<Ident, TypeDeclId>,

    /// Type for the stucco module definition, has to be some after initial type resolve pass.
    pub definition: Option<(NodeId<ast::ModuleDefinition>, TypeId)>,

    // Set in the type_check pass.
    pub expr_to_ty: PartialIndexMap<NodeId<ast::Expr>, TypeId>,
    pub symbol_to_ty: PartialIndexMap<SymbolId, TypeId>,

    /// Map of the generics to a specific type, altered during a type pass.
    pub generics: IndexMap<GenericId, Option<TypeId>>,
}

impl TypeId {
    pub const NIL: TypeId = unsafe { TypeId::from_u32_unchecked(0) };
    pub const NEVER: TypeId = unsafe { TypeId::from_u32_unchecked(1) };
    pub const IGNORE: TypeId = unsafe { TypeId::from_u32_unchecked(2) };
    pub const BOOL: TypeId = unsafe { TypeId::from_u32_unchecked(3) };
    pub const U64: TypeId = unsafe { TypeId::from_u32_unchecked(4) };
}

impl TypeTable {
    pub fn new() -> Self {
        let mut res = TypeTable {
            types: IdSet::new(),
            type_tuples: IdSet::new(),
            type_fields: IdSet::new(),

            field_name_to_type: HashMap::new(),

            declarations: IndexMap::new(),
            generics: IndexMap::new(),

            ast_to_type: PartialIndexMap::new(),
            ast_name_to_type: PartialIndexMap::new(),
            ast_fn_to_type: PartialIndexMap::new(),
            predefined: HashMap::new(),
            definition: None,

            expr_to_ty: PartialIndexMap::new(),
            symbol_to_ty: PartialIndexMap::new(),
        };

        assert_eq!(res.types.push_expect(Type::Nil), TypeId::NIL);
        assert_eq!(res.types.push_expect(Type::Never), TypeId::NEVER);
        assert_eq!(res.types.push_expect(Type::Ignore), TypeId::IGNORE);
        assert_eq!(res.insert_predefined("bool", Type::Bool), TypeId::BOOL);
        assert_eq!(res.insert_predefined("u64", Type::U64), TypeId::U64);
        //res.insert_predefined("usize", Type::Usize);

        res
    }

    fn insert_predefined(&mut self, name: &str, ty: Type) -> TypeId {
        let ty = self.types.push_expect(ty);
        let decl = self.declarations.push_expect(TypeDecl::Builtin(ty));
        self.predefined
            .insert(Ident::new(name, Span::call_site().into()), decl);
        ty
    }

    pub fn coerces_to(&self, from: TypeId, to: TypeId) -> bool {
        if from == TypeId::NEVER {
            return true;
        }

        if to == TypeId::IGNORE {
            return true;
        }

        self.is_equal(from, to)
    }

    pub fn is_equal(&self, a: TypeId, b: TypeId) -> bool {
        // TODO: Incorrect with generics, fix
        a == b
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
                if let TypeDecl::Defined { declared, .. } = self.declarations[decl] {
                    let _ = write!(res, "{}", declared.index(ast).name.index(ast));
                } else {
                    panic!("all struct should have been defined")
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
            Type::Decl { decl } => match self.declarations[decl] {
                TypeDecl::Builtin(_) => panic!("A decl should not be a builtin"),
                TypeDecl::Template { declared, .. } | TypeDecl::Defined { declared, .. } => {
                    let _ = write!(res, "{}", declared.index(ast).name.index(ast));
                }
            },
            Type::Ignore => {
                res.push_str("#IGNORE, NOT A REAL TYPE#");
            }
        }
    }
}
