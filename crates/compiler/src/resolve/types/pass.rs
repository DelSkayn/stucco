use std::{
    collections::{HashMap, hash_map::Entry},
    ops::Range,
};

use ast::{Ast, NodeId, visit::Visit};
use error::{AnnotationKind, Diagnostic, Level, Snippet};
use token::{Span, token::Ident};

use crate::resolve::types::{
    Type, TypeDecl, TypeDeclId, TypeFieldEntry, TypeId, TypeTable, TypeTupleEntry,
};

pub struct TypeResolvePass<'src, 't> {
    src: &'src str,
    table: &'t mut TypeTable,
    scopes: Vec<Range<u32>>,
    scope_types: Vec<TypeDeclId>,
    name_to_type: HashMap<NodeId<Ident>, TypeDeclId>,
}

impl<'src, 't> TypeResolvePass<'src, 't> {
    pub fn new(src: &'src str, table: &'t mut TypeTable) -> Self {
        assert!(
            table.definition.is_none(),
            "Type table already used in a resolve pass"
        );
        TypeResolvePass {
            src,
            table,
            scopes: vec![0..0],
            scope_types: Vec::new(),
            name_to_type: HashMap::new(),
        }
    }

    /// Run the type resolve pass.
    pub fn pass(mut self, ast: &Ast, root: NodeId<ast::Module>) -> Result<(), Diagnostic<'src>> {
        // Predeclare all the type first.
        for stmt in ast.iter_list_node(ast[root].stmts) {
            match ast[stmt] {
                ast::Stmt::Stencil(_)
                | ast::Stmt::Function(_)
                | ast::Stmt::Definition(_)
                | ast::Stmt::Impl(_) => {}
                ast::Stmt::Struct(n) => self.declare_type(
                    ast,
                    ast[n].name,
                    TypeDecl::Defined {
                        ty: TypeId::MAX,
                        declared: ast[n].name,
                        shadowed: None,
                    },
                )?,
            }
        }

        // Visit all the types to link them.
        self.visit_module(ast, root)
    }

    pub fn declare_type(
        &mut self,
        ast: &Ast,
        name: NodeId<ast::TypeName>,
        decl: TypeDecl,
    ) -> Result<(), Diagnostic<'src>> {
        let n = ast[name].name;

        // Type cannot be already a predefined name.
        if self.table.predefined.contains_key(&ast[n]) {
            return Err(Level::Error
                .title("Cannot redeclare builtin type `{}`")
                .snippet(
                    Snippet::source(self.src)
                        .annotate(AnnotationKind::Primary.span(ast[name].span)),
                )
                .to_diagnostic());
        }

        match self.name_to_type.entry(n) {
            Entry::Occupied(mut entry) => {
                let scope_types = self.scopes.last().unwrap();
                if self.scope_types[(scope_types.start as usize)..(scope_types.end as usize)]
                    .contains(entry.get())
                {
                    let (TypeDecl::Template { declared, .. } | TypeDecl::Defined { declared, .. }) =
                        self.table.declarations[*entry.get()]
                    else {
                        unreachable!()
                    };

                    return Err(Level::Error
                        .title(format!(
                            "Cannot redeclare type `{}`",
                            declared.index(ast).name.index(ast)
                        ))
                        .snippet(
                            Snippet::source(self.src)
                                .annotate(AnnotationKind::Primary.span(ast[name].span))
                                .annotate(
                                    AnnotationKind::Context
                                        .span(ast[declared].span)
                                        .label("Type first declared here"),
                                ),
                        )
                        .to_diagnostic());
                };

                let id = self.table.declarations.push_expect(decl);
                *entry.get_mut() = id;
                self.table.ast_name_to_type.insert_fill(name, id);
                self.scope_types.push(id);
                self.scopes.last_mut().unwrap().end += 1;
            }
            Entry::Vacant(entry) => {
                let id = self.table.declarations.push_expect(decl);
                entry.insert(id);
                self.table.ast_name_to_type.insert_fill(name, id);
                self.scope_types.push(id);
                self.scopes.last_mut().unwrap().end += 1;
            }
        }

        Ok(())
    }

    pub fn use_type_name(
        &mut self,
        ast: &Ast,
        id: NodeId<ast::TypeName>,
    ) -> Result<TypeDeclId, Diagnostic<'src>> {
        let n = ast[id].name;

        if let Some(x) = self.table.predefined.get(&ast[n]).copied() {
            self.table.ast_name_to_type.insert_fill(id, x);
            return Ok(x);
        }

        if let Some(x) = self.name_to_type.get(&n).copied() {
            self.table.ast_name_to_type.insert_fill(id, x);
            return Ok(x);
        }

        Err(Level::Error
            .title(format!("Use of undeclared type `{}`", ast[n]))
            .snippet(Snippet::source(self.src).annotate(AnnotationKind::Primary.span(ast[id].span)))
            .to_diagnostic())
    }

    /// Constructs a function type from the ast.
    pub fn construct_type_fn(
        &mut self,
        ast: &Ast,
        id: NodeId<ast::TypeFn>,
    ) -> Result<TypeId, Diagnostic<'src>> {
        // HACK: Pushing types in reverse order for now.
        let mut last = None;
        for ty in ast.iter_list_node(ast[id].params) {
            let ty = self.construct_type(ast, ty)?;
            last = Some(
                self.table
                    .type_tuples
                    .push_expect(TypeTupleEntry { ty, next: last }),
            );
        }

        let output = if let Some(x) = ast[id].output {
            self.construct_type(ast, x)?
        } else {
            TypeId::NIL
        };

        Ok(self
            .table
            .types
            .push_expect(Type::Fn { args: last, output }))
    }

    /// Constructs type from the ast.
    pub fn construct_type(
        &mut self,
        ast: &Ast,
        id: NodeId<ast::Type>,
    ) -> Result<TypeId, Diagnostic<'src>> {
        let ty = match ast[id] {
            ast::Type::Fn(n) => self.construct_type_fn(ast, n)?,
            ast::Type::Ptr(n) => {
                let ty = self.construct_type(ast, ast[n].ty)?;
                let ty = if ast[n].mutable {
                    Type::PtrMut { to: ty }
                } else {
                    Type::Ptr { to: ty }
                };
                self.table.types.push_expect(ty)
            }
            ast::Type::Name(n) => {
                if let Some(x) = self
                    .table
                    .predefined
                    .get(n.index(ast).name.index(ast))
                    .copied()
                {
                    let TypeDecl::Builtin(ty) = self.table.declarations[x] else {
                        unreachable!()
                    };
                    ty
                } else if let Some(decl) = self.name_to_type.get(&ast[n].name).copied() {
                    self.table.types.push_expect(Type::Decl { decl })
                } else {
                    return Err(Level::Error
                        .title(format!(
                            "Use of undeclared type `{}`",
                            n.index(ast).name.index(ast)
                        ))
                        .snippet(
                            Snippet::source(self.src)
                                .annotate(AnnotationKind::Primary.span(ast[n].span)),
                        )
                        .to_diagnostic());
                }
            }
        };

        self.table.ast_to_type.insert_fill(id, ty);

        return Ok(ty);
    }

    pub fn with_scope<R, F: FnOnce(&Ast, &mut Self) -> R>(&mut self, ast: &Ast, f: F) -> R {
        let last_scope_end = self.scopes.last_mut().unwrap().end;

        self.scopes.push(last_scope_end..last_scope_end);

        let r = f(ast, self);

        for _ in self.scopes.pop().unwrap() {
            let ty = self.scope_types.pop().unwrap();
            let (TypeDecl::Template {
                shadowed, declared, ..
            }
            | TypeDecl::Defined {
                shadowed, declared, ..
            }) = self.table.declarations[ty]
            else {
                unreachable!("scope_types should only contain non predefined types");
            };

            let name = ast[declared].name;
            if let Some(x) = shadowed {
                *self.name_to_type.get_mut(&name).unwrap() = x;
            } else {
                self.name_to_type.remove(&name).unwrap();
            }
        }

        r
    }
}

impl<'src, 't> Visit for TypeResolvePass<'src, 't> {
    type Error = Diagnostic<'src>;

    fn visit_module(&mut self, ast: &Ast, id: NodeId<ast::Module>) -> Result<(), Self::Error> {
        if let Some(x) = ast[id].definition {
            let ty = self.construct_type_fn(ast, ast[x].ty)?;
            self.table.definition = Some((x, ty));
        }

        ast::visit::visit_module(self, ast, id)?;

        if self.table.definition.is_none() {
            return Err(Level::Error
                .title("Missing `mod name fn(Arg1,Arg2) -> Res;` module definition")
                .snippet(Snippet::source(self.src).annotate(
                    AnnotationKind::Primary.span(Span::call_site()).label(
                        "All stucco modules must define the type of the function to be constructed",
                    ),
                ))
                .to_diagnostic());
        }

        Ok(())
    }

    fn visit_module_definition(
        &mut self,
        ast: &Ast,
        id: NodeId<ast::ModuleDefinition>,
    ) -> Result<(), Self::Error> {
        if let Some(x) = self.table.definition {
            return Err(Level::Error
                .title("Module defined more then once.")
                .snippet(
                    Snippet::source(self.src)
                        .annotate(
                            AnnotationKind::Primary
                                .span(ast[x.0].span)
                                .label("Second definition found here"),
                        )
                        .annotate(
                            AnnotationKind::Context
                                .span(ast[x.0].span)
                                .label("First definition found here"),
                        ),
                )
                .to_diagnostic());
        }

        let ty = self.construct_type_fn(ast, ast[id].ty)?;
        self.table.definition = Some((id, ty));

        Ok(())
    }

    fn visit_type_name(&mut self, ast: &Ast, id: NodeId<ast::TypeName>) -> Result<(), Self::Error> {
        self.use_type_name(ast, id)?;
        Ok(())
    }

    fn visit_struct(&mut self, ast: &Ast, id: NodeId<ast::Struct>) -> Result<(), Self::Error> {
        let ty_decl = self.table.ast_name_to_type[ast[id].name];

        self.with_scope(ast, |ast, this| {
            for f in ast.iter_list_node(ast[id].templates) {
                let id = this.table.generics.push_expect(None);
                let shadowed = this.name_to_type.get(&ast[f].name).copied();

                this.declare_type(
                    ast,
                    f,
                    TypeDecl::Template {
                        id,
                        declared: f,
                        shadowed,
                    },
                )?;
            }

            let mut last = None;
            for f in ast.iter_list_node(ast[id].fields) {
                let ty = this.construct_type(ast, ast[f].ty)?;
                let name = ast[f].name;
                last = Some(this.table.type_fields.push_expect(TypeFieldEntry {
                    name,
                    ty,
                    next: last,
                }));
                if this
                    .table
                    .field_name_to_type
                    .insert((ty_decl, name), ty)
                    .is_some()
                {
                    return Err(Level::Error
                        .title(format!(
                            "Conflicting field definition, field `{}` defined twice",
                            ast[name],
                        ))
                        .snippet(
                            Snippet::source(this.src)
                                .annotate(AnnotationKind::Primary.span(ast[f].span)),
                        )
                        .to_diagnostic());
                }
            }

            let ty_id = this.table.types.push_expect(Type::Struct {
                decl: ty_decl,
                fields: last,
            });
            let TypeDecl::Defined { ref mut ty, .. } = this.table.declarations[ty_decl] else {
                unreachable!()
            };
            *ty = ty_id;

            Ok(())
        })
    }

    fn visit_type(&mut self, ast: &Ast, id: NodeId<ast::Type>) -> Result<(), Self::Error> {
        self.construct_type(ast, id)?;

        Ok(())
    }

    fn visit_function(&mut self, ast: &Ast, id: NodeId<ast::Function>) -> Result<(), Self::Error> {
        self.with_scope(ast, |ast, this| {
            for f in ast.iter_list_node(ast[id].templates) {
                let id = this.table.generics.push_expect(None);
                let shadowed = this.name_to_type.get(&ast[f].name).copied();

                this.declare_type(
                    ast,
                    f,
                    TypeDecl::Template {
                        id,
                        declared: f,
                        shadowed,
                    },
                )?;
            }

            let mut args = None;
            for p in ast.iter_list_node(ast[id].parameters) {
                let ty = this.construct_type(ast, ast[p].ty)?;
                args = Some(
                    this.table
                        .type_tuples
                        .push_expect(TypeTupleEntry { ty, next: args }),
                )
            }

            let output = if let Some(x) = ast[id].output {
                this.construct_type(ast, x)?
            } else {
                TypeId::NIL
            };

            let ty_id = this.table.types.push_expect(Type::Fn { args, output });
            this.table.ast_fn_to_type.insert_fill(id, ty_id);

            this.visit_expr(ast, ast[id].body)?;
            Ok(())
        })
    }

    fn visit_stencil(&mut self, ast: &Ast, id: NodeId<ast::Stencil>) -> Result<(), Self::Error> {
        for p in ast.iter_list_node(ast[id].parameters) {
            self.construct_type(ast, ast[p].ty)?;
        }

        self.visit_expr(ast, ast[id].body)?;

        Ok(())
    }
}
