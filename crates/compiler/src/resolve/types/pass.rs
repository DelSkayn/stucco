use ast::{Ast, NodeId, visit::Visit};
use error::{AnnotationKind, Diagnostic, Level, Snippet};

use crate::resolve::types::{Type, TypeDecl, TypeDeclId, TypeId, TypeTable};

pub struct TypeResolvePass<'src, 't> {
    src: &'src str,
    table: &'t mut TypeTable,
}

impl<'src, 't> TypeResolvePass<'src, 't> {
    pub fn new(src: &'src str, table: &'t mut TypeTable) -> Self {
        TypeResolvePass { src, table }
    }

    pub fn declare_type(
        &mut self,
        ast: &Ast,
        name: NodeId<ast::TypeName>,
    ) -> Result<(), Diagnostic<'src>> {
        let n = ast[name].name;

        if self.table.predefined.contains_key(&ast[n]) {
            return Err(Level::Error
                .title("Cannot redeclare builtin type `{}`")
                .snippet(
                    Snippet::source(self.src)
                        .annotate(AnnotationKind::Primary.span(ast[name].span)),
                )
                .to_diagnostic());
        }

        if let Some(x) = self.table.name_to_type.get(&n) {
            let declared = self.table.declarations[*x]
                .declare
                .expect("types in name to type should have been declared");

            return Err(Level::Error
                .title("Cannot redeclare type `{}`")
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
        }

        let id = self.table.declarations.push_expect(TypeDecl {
            // Will later be fix to a proper typeid.
            ty: TypeId::MAX,
            declare: Some(name),
        });

        self.table.ast_name_to_type.insert_fill(name, id);
        self.table.name_to_type.insert(n, id);
        Ok(())
    }

    pub fn pass(mut self, ast: &Ast, root: NodeId<ast::Module>) -> Result<(), Diagnostic<'src>> {
        for stmt in ast.iter_list_node(ast[root].stmts) {
            match ast[stmt] {
                ast::Stmt::Stencil(_) | ast::Stmt::Function(_) => {}
                ast::Stmt::Struct(n) => self.declare_type(ast, ast[n].name)?,
            }
        }

        self.visit_module(ast, root)
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

        if let Some(x) = self.table.name_to_type.get(&n).copied() {
            self.table.ast_name_to_type.insert_fill(id, x);
            return Ok(x);
        }

        Err(Level::Error
            .title(format!("Use of undeclared type `{}`", ast[n]))
            .snippet(Snippet::source(self.src).annotate(AnnotationKind::Primary.span(ast[id].span)))
            .to_diagnostic())
    }

    pub fn construct_type(
        &mut self,
        ast: &Ast,
        id: NodeId<ast::Type>,
    ) -> Result<TypeId, Diagnostic<'src>> {
        let ty = match ast[id] {
            ast::Type::Fn(n) => {
                // HACK: Pushing types in reverse order for now.
                let mut last = None;
                for ty in ast.iter_list_node(ast[n].params) {
                    let ty = self.construct_type(ast, ty)?;
                    last = Some(self.table.type_tuples.push_expect((ty, last)));
                }

                let output = if let Some(x) = ast[n].output {
                    self.construct_type(ast, x)?
                } else {
                    TypeTable::NIL_ID
                };

                self.table
                    .types
                    .push_expect(Type::Fn { args: last, output })
            }
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
                    self.table.declarations[x].ty
                } else if let Some(decl) = self.table.name_to_type.get(&ast[n].name).copied() {
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
}

impl<'src, 't> Visit for TypeResolvePass<'src, 't> {
    type Error = Diagnostic<'src>;

    fn visit_type_name(&mut self, ast: &Ast, id: NodeId<ast::TypeName>) -> Result<(), Self::Error> {
        self.use_type_name(ast, id)?;
        Ok(())
    }

    fn visit_struct(&mut self, ast: &Ast, id: NodeId<ast::Struct>) -> Result<(), Self::Error> {
        let ty_decl = self.table.ast_name_to_type[ast[id].name];

        let mut last = None;
        for f in ast.iter_list_node(ast[id].fields) {
            let ty = self.construct_type(ast, ast[f].ty)?;
            last = Some(self.table.type_tuples.push_expect((ty, last)));
        }

        let ty = self.table.types.push_expect(Type::Struct {
            decl: ty_decl,
            fields: last,
        });
        self.table.declarations[ty_decl].ty = ty;

        Ok(())
    }

    fn visit_type(&mut self, ast: &Ast, id: NodeId<ast::Type>) -> Result<(), Self::Error> {
        self.construct_type(ast, id)?;

        Ok(())
    }

    fn visit_function(&mut self, ast: &Ast, id: NodeId<ast::Function>) -> Result<(), Self::Error> {
        let mut args = None;
        for p in ast.iter_list_node(ast[id].parameters) {
            let ty = self.construct_type(ast, ast[p].ty)?;
            args = Some(self.table.type_tuples.push_expect((ty, args)))
        }

        let output = if let Some(x) = ast[id].output {
            self.construct_type(ast, x)?
        } else {
            TypeTable::NIL_ID
        };

        let ty_id = self.table.types.push_expect(Type::Fn { args, output });
        self.table.ast_fn_to_type.insert_fill(id, ty_id);

        Ok(())
    }
}
