use ast::{Ast, NodeId, visit::Visit};
use error::{AnnotationKind, Diagnostic, Level, Snippet};

use crate::resolve::types::{Type, TypeKind, TypeTable};

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
            let declared = self.table.types[*x]
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

        let id = self.table.types.push_expect(Type {
            declare: Some(name),
            // Will be later adjusted with the correct type.
            kind: TypeKind::Nil,
        });

        self.table.ast_to_type.insert_fill(name, id);
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
    ) -> Result<(), Diagnostic<'src>> {
        let n = ast[id].name;
        if let Some(x) = self.table.predefined.get(&ast[n]).copied() {
            self.table.ast_to_type.insert_fill(id, x);
            return Ok(());
        }

        if let Some(x) = self.table.name_to_type.get(&n).copied() {
            self.table.ast_to_type.insert_fill(id, x);
            return Ok(());
        }

        Err(Level::Error
            .title(format!("Use of undeclared type `{}`", ast[n]))
            .snippet(Snippet::source(self.src).annotate(AnnotationKind::Primary.span(ast[id].span)))
            .to_diagnostic())
    }
}

impl<'src, 't> Visit for TypeResolvePass<'src, 't> {
    type Error = Diagnostic<'src>;

    fn visit_type_name(&mut self, ast: &Ast, id: NodeId<ast::TypeName>) -> Result<(), Self::Error> {
        self.use_type_name(ast, id)
    }
}
