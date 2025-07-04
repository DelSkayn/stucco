use common::{
    id,
    id::{Id, IdSet},
    thinvec::ThinVec,
};
use std::{
    cmp::Eq,
    error,
    fmt::{self},
    hash::{Hash, Hasher},
    ops::{Index, IndexMut},
    u32,
};
use token::{
    Span, Spanned,
    token::{Ident, Lit},
};

#[cfg(feature = "print")]
mod print;
#[cfg(feature = "print")]
pub use print::{AstDisplay, AstFormatter, AstRender};

pub trait NodeStorage<T: Node> {
    fn storage_get(&self, idx: u32) -> Option<&T>;

    fn storage_get_mut(&mut self, idx: u32) -> Option<&mut T>;

    fn storage_push(&mut self, value: T) -> u32;
}

impl<T: Node> NodeStorage<T> for Vec<T> {
    fn storage_get(&self, idx: u32) -> Option<&T> {
        self.get(idx as usize)
    }
    fn storage_get_mut(&mut self, idx: u32) -> Option<&mut T> {
        self.get_mut(idx as usize)
    }

    fn storage_push(&mut self, value: T) -> u32 {
        let res = self.len().try_into().expect("too many nodes");
        self.push(value);
        res
    }
}

impl<T: Node> NodeStorage<T> for ThinVec<T> {
    fn storage_get(&self, idx: u32) -> Option<&T> {
        self.get(idx)
    }
    fn storage_get_mut(&mut self, idx: u32) -> Option<&mut T> {
        self.get_mut(idx)
    }

    fn storage_push(&mut self, value: T) -> u32 {
        let res = self.len().try_into().expect("too many nodes");
        self.push(value);
        res
    }
}

impl<T: Node> NodeStorage<T> for IdSet<u32, T> {
    fn storage_get(&self, idx: u32) -> Option<&T> {
        self.get(idx)
    }

    fn storage_get_mut(&mut self, idx: u32) -> Option<&mut T> {
        self.get_mut(idx)
    }

    fn storage_push(&mut self, value: T) -> u32 {
        self.push(value).expect("too many nodes")
    }
}

pub trait Node: Hash + Eq + 'static {}

impl Node for Span {}
impl Node for Ident {}
impl Node for Lit {}

id!(NodeId<T>);

impl<T: Node> NodeId<T> {
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

impl<T: Node> NodeListId<T> {
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

impl<T> Eq for NodeList<T> {}
impl<T> PartialEq for NodeList<T> {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value && self.next == other.next
    }
}

impl<T> Hash for NodeList<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.value.hash(state);
        self.next.hash(state);
    }
}

impl<T: Node> Node for NodeList<T> {}

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

    pub fn push<T: Node>(&mut self, value: T) -> Result<NodeId<T>, PushNodeError> {
        let idx = self.library.push(value);
        NodeId::from_u32(idx).ok_or(PushNodeError(()))
    }

    pub fn push_list<T: Node>(
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

    pub fn iter_list<'a, T>(&'a self, id: Option<NodeListId<T>>) -> ListIter<'a, L, T> {
        ListIter {
            ast: self,
            current: id,
        }
    }

    pub fn iter_list_node<'a, T>(&'a self, id: Option<NodeListId<T>>) -> ListIterNode<'a, L, T> {
        ListIterNode {
            ast: self,
            current: id,
        }
    }

    pub fn next_list<'a, T: Node>(&'a self, id: &mut Option<NodeListId<T>>) -> Option<NodeId<T>> {
        let node = (*id)?;

        *id = self[node].next;
        let v = self[node].value;

        Some(v)
    }

    pub fn next_list_mut<'a, T: Node>(
        &'a mut self,
        id: &mut Option<NodeListId<T>>,
    ) -> Option<NodeId<T>> {
        let node = (*id)?;

        *id = self[node].next;
        let v = self[node].value;

        Some(v)
    }

    pub fn library(&self) -> &L {
        &self.library
    }
}

pub struct ListIter<'a, L, T> {
    ast: &'a Ast<L>,
    current: Option<NodeListId<T>>,
}

impl<'a, L, T: Node> Iterator for ListIter<'a, L, T>
where
    L: NodeLibrary,
{
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        let idx = self.ast.next_list(&mut self.current)?;
        Some(&self.ast[idx])
    }
}

pub struct ListIterNode<'a, L, T> {
    ast: &'a Ast<L>,
    current: Option<NodeListId<T>>,
}

impl<'a, L, T: Node> Iterator for ListIterNode<'a, L, T>
where
    L: NodeLibrary,
{
    type Item = NodeId<T>;

    fn next(&mut self) -> Option<Self::Item> {
        let idx = self.ast.next_list(&mut self.current)?;
        Some(idx)
    }
}

impl<T: Node, L: NodeLibrary> Index<NodeId<T>> for Ast<L> {
    type Output = T;

    fn index(&self, index: NodeId<T>) -> &Self::Output {
        let Some(x) = self.library.get(index.into_u32()) else {
            panic!(
                "invalid node id `{}` for type `{}`",
                index.into_u32(),
                std::any::type_name::<T>()
            );
        };
        x
    }
}

impl<T: Node, L: NodeLibrary> IndexMut<NodeId<T>> for Ast<L> {
    fn index_mut(&mut self, index: NodeId<T>) -> &mut Self::Output {
        let Some(x) = self.library.get_mut(index.into_u32()) else {
            panic!("invalid node id for type `{}`", std::any::type_name::<T>());
        };
        x
    }
}

impl<T: Node, L: NodeLibrary> Index<NodeListId<T>> for Ast<L> {
    type Output = NodeList<T>;

    fn index(&self, index: NodeListId<T>) -> &Self::Output {
        &self[index.id]
    }
}

impl<T: Node, L: NodeLibrary> IndexMut<NodeListId<T>> for Ast<L> {
    fn index_mut(&mut self, index: NodeListId<T>) -> &mut Self::Output {
        &mut self[index.id]
    }
}

pub trait NodeLibrary {
    fn new() -> Self;

    fn get<T: Node>(&self, idx: u32) -> Option<&T>;

    fn get_mut<T: Node>(&mut self, idx: u32) -> Option<&mut T>;

    fn push<T: Node>(&mut self, value: T) -> u32;

    fn clear(&mut self);
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
    T: AstSpanned + Node,
{
    fn ast_span<L: NodeLibrary>(&self, ast: &Ast<L>) -> Span {
        ast[*self].ast_span(ast)
    }
}

#[macro_export]
macro_rules! library {
    (
        $(#[$m:meta])*
        $name:ident{ $($field:ident: $container:ident<$( $ty:ty ),* $(,)?>),* $(,)? }
    ) => {

        $(#[$m])*
        pub struct $name{
            $(
                pub $field: $container<$( $ty ),*>
            ),*
        }

        impl $crate::ast::NodeLibrary for $name{
            fn new() -> Self{
                $name{
                    $($field: $container::new()),*
                }
            }

            fn get<T: Node>(&self, idx: u32) -> Option<&T>{
                let type_id = std::any::TypeId::of::<T>();
                $(
                    if std::any::TypeId::of::<library!(@last $($ty,)*)>() == type_id{
                        unsafe{
                            let cntr = std::mem::transmute::<&$container<$($ty,)*>,&$container<T>>(&self.$field);
                            return $crate::ast::NodeStorage::storage_get(cntr,idx);
                        }
                    }

                )*

                panic!("type '{}' not part of node library",std::any::type_name::<T>());
            }

            fn get_mut<T: Node>(&mut self, idx: u32) -> Option<&mut T>{
                let type_id = std::any::TypeId::of::<T>();
                $(
                    if std::any::TypeId::of::<library!(@last $($ty,)*)>() == type_id{
                        unsafe{
                            let cntr = std::mem::transmute::<&mut $container<$($ty,)*>,&mut $container<T>>(&mut self.$field);
                            return $crate::ast::NodeStorage::storage_get_mut(cntr,idx);
                        }
                    }
                )*
                panic!("type '{}' not part of node library",std::any::type_name::<T>());
            }

            fn push<T: Node>(&mut self, value: T) -> u32{
                let type_id = std::any::TypeId::of::<T>();
                $(
                    if std::any::TypeId::of::<library!(@last $($ty,)*)>() == type_id{
                        unsafe{
                            let cntr = std::mem::transmute::<&mut $container<$($ty,)*>,&mut $container<T>>(&mut self.$field);
                            return $crate::ast::NodeStorage::storage_push(cntr,value);
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

    (@last $f:ty, $($t:ty,)+) => {
        library!(@last $($t,)*)
    };
    (@last $f:ty,) => {
        $f
    };
}

#[macro_export]
macro_rules! ast_struct {
    (
        $(#[$m:meta])*
        $vis:vis struct $name:ident {
            $(
                $(#[$field_m:meta])*
                pub $field:ident: $ty:ty
            ),*$(,)?
        }


    ) => {

        #[derive(Clone,Copy,Debug,PartialEq,Eq,Hash)]
        $vis struct $name {
            $(
                $(#[$field_m])*
                pub $field: $ty,
            )*
            pub span: ::token::Span
        }

        impl $crate::ast::Node for $name{}

        impl ::token::Spanned for $name
        {
            fn span(&self) -> ::token::Span {
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

        #[derive(Clone,Copy,Debug, PartialEq,Eq, Hash)]
        $vis enum $name {
            $(
                $variant($ty)
            ),*
        }

        impl $crate::ast::Node for $name{}


        impl $crate::ast::AstSpanned for $name
        {
            fn ast_span<L: $crate::ast::NodeLibrary>(&self, ast: &$crate::ast::Ast<L>) -> ::token::Span {
                match *self{
                    $(
                        Self::$variant(ref x) => {
                            crate::ast::AstSpanned::ast_span(x,ast)
                        }
                    )*
                }
            }
        }


        #[cfg(feature = "print")]
        impl<L,W> $crate::ast::AstDisplay<L,W> for $name
            where L: $crate::ast::NodeLibrary,
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
