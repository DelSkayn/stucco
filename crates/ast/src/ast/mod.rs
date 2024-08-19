use std::{
    any::Any,
    error,
    fmt::{self},
    hash::{Hash, Hasher},
    marker::PhantomData,
    ops::{Index, IndexMut},
    u32,
};

#[cfg(feature = "print")]
mod print;
use common::{id, id::Id};
#[cfg(feature = "print")]
pub use print::{AstDisplay, AstFormatter, AstRender};

id!(NodeId<T>);

impl<T> NodeId<T> {
    pub fn index<L>(self, ast: &Ast<L>) -> &T
    where
        L: NodeLibrary + 'static,
        T: 'static,
    {
        &ast[self]
    }

    pub fn index_mut<L>(self, ast: &mut Ast<L>) -> &mut T
    where
        L: NodeLibrary + 'static,
        T: 'static,
    {
        &mut ast[self]
    }
}

pub struct NodeListId<T> {
    id: NodeId<NodeList<T>>,
}
impl<T> Clone for NodeListId<T> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<T> Copy for NodeListId<T> {}
impl<T> PartialEq for NodeListId<T> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}
impl<T> Eq for NodeListId<T> {}
impl<T> Hash for NodeListId<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state)
    }
}

impl<T> fmt::Debug for NodeListId<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("NodeListId")
            .field("id", &self.id.into_u32())
            .finish()
    }
}

impl<T> Id for NodeListId<T> {
    fn idx(self) -> usize {
        self.id.idx()
    }

    fn from_idx(idx: usize) -> Option<Self> {
        Some(Self {
            id: NodeId::from_idx(idx)?,
        })
    }
}

impl<T> NodeListId<T> {
    pub fn index<L>(self, ast: &Ast<L>) -> &NodeList<T>
    where
        L: NodeLibrary + 'static,
        T: 'static,
    {
        &ast[self]
    }

    pub fn index_mut<L>(self, ast: &mut Ast<L>) -> &mut NodeList<T>
    where
        L: NodeLibrary + 'static,
        T: 'static,
    {
        &mut ast[self]
    }
}

pub struct NodeList<T> {
    pub value: NodeId<T>,
    pub next: Option<NodeListId<T>>,
}

impl<T> fmt::Debug for NodeList<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("NodeList")
            .field("id", &self.value)
            .field("next", &self.next)
            .finish()
    }
}

#[derive(Clone, Debug)]
pub struct PushNodeError(());

impl fmt::Display for PushNodeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Failed to push node to ast, number of nodes exceeded maximum limit"
        )
    }
}
impl error::Error for PushNodeError {}

impl From<PushNodeError> for syn::Error {
    fn from(value: PushNodeError) -> Self {
        let _ = value;
        syn::Error::new(
            proc_macro2::Span::call_site(),
            "exceeded maximum number of tokens",
        )
    }
}

pub struct Ast<L> {
    library: L,
}

impl<L> Ast<L>
where
    L: NodeLibrary,
{
    pub fn new() -> Self {
        Self { library: L::new() }
    }

    pub fn push<T: Any>(&mut self, value: T) -> Result<NodeId<T>, PushNodeError> {
        let idx = self.library.push(value);
        NodeId::from_u32(idx).ok_or(PushNodeError(()))
    }

    pub fn push_list<T: Any>(
        &mut self,
        head: &mut Option<NodeListId<T>>,
        current: &mut Option<NodeListId<T>>,
        value: NodeId<T>,
    ) -> Result<(), PushNodeError> {
        let list_idx = self.push(NodeList { value, next: None })?;

        if let Some(x) = *current {
            self[x.id].next = Some(NodeListId { id: list_idx });
        }

        *current = Some(NodeListId { id: list_idx });
        *head = head.or(*current);

        Ok(())
    }

    pub fn clear(&mut self) {
        self.library.clear()
    }

    pub fn iter_list<T>(&self, id: Option<NodeListId<T>>) -> ListIter<L, T> {
        ListIter {
            ast: self,
            current: id,
        }
    }

    pub fn next_list<'a, T: 'static>(
        &'a self,
        id: &mut Option<NodeListId<T>>,
    ) -> Option<NodeId<T>> {
        let node = (*id)?;

        *id = self[node].next;
        let v = self[node].value;

        Some(v)
    }

    pub fn next_list_mut<'a, T: 'static>(
        &'a mut self,
        id: &mut Option<NodeListId<T>>,
    ) -> Option<NodeId<T>> {
        let node = (*id)?;

        *id = self[node].next;
        let v = self[node].value;

        Some(v)
    }
}

pub struct ListIter<'a, L, T> {
    ast: &'a Ast<L>,
    current: Option<NodeListId<T>>,
}

impl<'a, L, T: 'static> Iterator for ListIter<'a, L, T>
where
    L: NodeLibrary,
{
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        let idx = self.ast.next_list(&mut self.current)?;
        Some(&self.ast[idx])
    }
}

impl<T: Any, L: NodeLibrary> Index<NodeId<T>> for Ast<L> {
    type Output = T;

    fn index(&self, index: NodeId<T>) -> &Self::Output {
        let Some(x) = self.library.get(index.into_u32()) else {
            panic!("invalid node id for type `{}`", std::any::type_name::<T>());
        };
        x
    }
}

impl<T: Any, L: NodeLibrary> IndexMut<NodeId<T>> for Ast<L> {
    fn index_mut(&mut self, index: NodeId<T>) -> &mut Self::Output {
        let Some(x) = self.library.get_mut(index.into_u32()) else {
            panic!("invalid node id for type `{}`", std::any::type_name::<T>());
        };
        x
    }
}

impl<T: Any, L: NodeLibrary> Index<NodeListId<T>> for Ast<L> {
    type Output = NodeList<T>;

    fn index(&self, index: NodeListId<T>) -> &Self::Output {
        &self[index.id]
    }
}

impl<T: Any, L: NodeLibrary> IndexMut<NodeListId<T>> for Ast<L> {
    fn index_mut(&mut self, index: NodeListId<T>) -> &mut Self::Output {
        &mut self[index.id]
    }
}

pub trait NodeLibrary {
    fn new() -> Self;

    fn get<T: Any>(&self, idx: u32) -> Option<&T>;

    fn get_mut<T: Any>(&mut self, idx: u32) -> Option<&mut T>;

    fn push<T: Any>(&mut self, value: T) -> u32;

    fn clear(&mut self);
}

#[derive(Clone, Copy, Debug)]
pub struct Span(proc_macro2::Span);

impl Span {
    pub fn call_site() -> Self {
        Self(proc_macro2::Span::call_site())
    }

    pub fn try_join(&self, other: Self) -> Self {
        self.0
            .join(other.0)
            .map(Span)
            .unwrap_or_else(|| self.clone())
    }
}

impl From<proc_macro2::Span> for Span {
    fn from(value: proc_macro2::Span) -> Self {
        Self(value)
    }
}

impl From<Span> for proc_macro2::Span {
    fn from(value: Span) -> Self {
        value.0
    }
}

pub trait Spanned {
    fn span(&self) -> Span;
}

impl<T> Spanned for T
where
    T: syn::spanned::Spanned,
{
    fn span(&self) -> Span {
        Span::from(self.span())
    }
}

pub trait AstSpanned {
    fn ast_span<L: NodeLibrary>(&self, ast: &Ast<L>) -> Span;
}

impl<T> AstSpanned for T
where
    T: Spanned,
{
    fn ast_span<L: NodeLibrary>(&self, _: &Ast<L>) -> Span {
        self.span()
    }
}

impl<T> AstSpanned for NodeId<T>
where
    T: AstSpanned + 'static,
{
    fn ast_span<L: NodeLibrary>(&self, ast: &Ast<L>) -> Span {
        ast[*self].ast_span(ast)
    }
}

impl AstSpanned for Span {
    fn ast_span<L: NodeLibrary>(&self, _: &Ast<L>) -> Span {
        *self
    }
}

#[macro_export]
macro_rules! library {
    (
        $(#[$m:meta])*
        $name:ident{ $($field:ident: $container:ident<$ty:ty>),* $(,)? }
    ) => {

        $(#[$m])*
        pub struct $name{
            $(
                $field: $container<$ty>
            ),*
        }

        impl $crate::ast::NodeLibrary for $name{
            fn new() -> Self{
                $name{
                    $($field: $container::new()),*
                }
            }

            fn get<T: std::any::Any>(&self, idx: u32) -> Option<&T>{
                let type_id = std::any::TypeId::of::<T>();
                $(
                    if std::any::TypeId::of::<$ty>() == type_id{
                        unsafe{
                            return std::mem::transmute::<&$container<$ty>,&$container<T>>(&self.$field).get(idx)
                        }
                    }

                )*

                panic!("type '{}' not part of node library",std::any::type_name::<T>());
            }

            fn get_mut<T: std::any::Any>(&mut self, idx: u32) -> Option<&mut T>{
                let type_id = std::any::TypeId::of::<T>();
                $(
                    if std::any::TypeId::of::<$ty>() == type_id{
                        unsafe{
                            return std::mem::transmute::<&mut $container<$ty>,&mut $container<T>>(&mut self.$field).get_mut(idx)
                        }
                    }
                )*
                panic!("type '{}' not part of node library",std::any::type_name::<T>());
            }

            fn push<T: std::any::Any>(&mut self, value: T) -> u32{
                let type_id = std::any::TypeId::of::<T>();
                $(
                    if std::any::TypeId::of::<$ty>() == type_id{
                        unsafe{
                            let idx = self.$field.len();
                            std::mem::transmute::<&mut $container<$ty>,&mut $container<T>>(&mut self.$field).push(value);
                            return idx
                        }
                    }
                )*
                panic!("type '{}' not part of node library",std::any::type_name::<T>());
            }

            fn clear(&mut self){
                $(
                    self.$field.clear();
                )*
            }

        }

    };
}

#[macro_export]
macro_rules! ast_struct {
    (
        $(#[$m:meta])*
        $vis:vis struct $name:ident {
            $(
                pub $field:ident: $ty:ty
            ),*$(,)?
        }


    ) => {

        #[derive(Clone,Copy,Debug)]
        $vis struct $name {
            $(
                pub $field: $ty,
            )*
            pub span: crate::ast::Span
        }

        impl crate::ast::Spanned for $name
        {
            fn span(&self) -> crate::ast::Span {
                self.span
            }
        }

        #[cfg(feature = "print")]
        impl<L,W> crate::ast::AstDisplay<L,W> for $name
            where L: crate::ast::NodeLibrary,
                  W: ::std::fmt::Write,
        {
            fn fmt(&self, fmt: &mut crate::ast::AstFormatter<L,W>) -> ::std::fmt::Result{
                writeln!(fmt,concat!(stringify!($name)," {{"))?;
                fmt.indent(|fmt|{
                    $(
                        write!(fmt,concat!(stringify!($field),": "))?;
                        crate::ast::AstDisplay::fmt(&self.$field,fmt)?;
                        writeln!(fmt,",")?;
                    )*
                    Ok(())
                })?;
                write!(fmt,"}}")
            }
        }
    };
}

#[macro_export]
macro_rules! ast_enum{
    (
        $(#[$m:meta])*
        $vis:vis enum $name:ident {
            $(
                $variant:ident($ty:ty)
            ),*$(,)?
        }


    ) => {

        #[derive(Clone,Copy,Debug)]
        $vis enum $name {
            $(
                $variant($ty)
            ),*
        }


        impl $crate::ast::AstSpanned for $name
        {
            fn ast_span<L: $crate::ast::NodeLibrary>(&self, ast: &$crate::ast::Ast<L>) -> $crate::ast::Span {
                match *self{
                    $(
                        Self::$variant(x) => {
                            x.ast_span(ast)
                        }
                    )*
                }
            }
        }


        #[cfg(feature = "print")]
        impl<L,W> crate::ast::AstDisplay<L,W> for $name
            where L: crate::ast::NodeLibrary,
                  W: ::std::fmt::Write,
        {
            fn fmt(&self, fmt: &mut crate::ast::AstFormatter<L,W>) -> ::std::fmt::Result{
                match self{
                    $(
                    $name::$variant(x) => {
                        write!(fmt,concat!(stringify!($name),"::",stringify!($variant),"("))?;
                        x.fmt(fmt)?;
                        write!(fmt,")")
                    }
                    )*
                }
            }
        }
    };
}
