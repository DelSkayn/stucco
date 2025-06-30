mod ast;
pub mod visit;

pub use ast::Node;
use ast::NodeLibrary;
pub use ast::{AstSpanned, NodeId, NodeList, NodeListId, PushNodeError};
use common::id::IdSet;
use token::{
    Span,
    token::{Ident, Lit},
};

#[cfg(feature = "print")]
pub use ast::{AstDisplay, AstFormatter, AstRender};

type LibrarySet<T> = IdSet<u32, T>;

library!(Library {
    module: Vec<Module>,

    stencil_function: Vec<Stencil>,
    stencil_functions: Vec<NodeList<Stencil>>,

    variant: Vec<Variant>,
    variants: Vec<NodeList<Variant>>,

    variation: Vec<Variation>,
    variations: Vec<NodeList<Variation>>,

    variation_slot: Vec<VariationSlot>,
    variation_constant: Vec<VariationImmediate>,

    parameter: Vec<Parameter>,
    parameters: Vec<NodeList<Parameter>>,

    expr: Vec<Expr>,
    exprs: Vec<NodeList<Expr>>,
    binary: Vec<BinaryExpr>,
    unary: Vec<UnaryExpr>,
    if_: Vec<If>,
    while_: Vec<While>,
    block: Vec<Block>,

    literal: Vec<Lit>,

    let_: Vec<Let>,

    method: Vec<Method>,
    call: Vec<Call>,
    field: Vec<Field>,
    index: Vec<Index>,

    break_: Vec<Break>,
    return_: Vec<Return>,
    tail: Vec<Become>,

    type_: Vec<Type>,
    types: Vec<NodeList<Type>>,
    type_fn: Vec<TypeFn>,
    type_array: Vec<TypeArray>,
    type_tuple: Vec<TypeTuple>,
    type_ptr: Vec<TypePtr>,
    type_reference: Vec<TypeReference>,

    arg: Vec<Arg>,
    args: Vec<NodeList<Arg>>,

    symbol: Vec<Symbol>,

    ident: LibrarySet<Ident>,
});

pub type Ast = ast::Ast<Library>;

ast_enum! {
        pub enum Expr {
        If(NodeId<If>),
        Binary(NodeId<BinaryExpr>),
        Unary(NodeId<UnaryExpr>),
        Block(NodeId<Block>),
        Cast(NodeId<Cast>),
        Loop(NodeId<Block>),
        While(NodeId<While>),
        Let(NodeId<Let>),
        Continue(Span),
        Break(NodeId<Break>),
        Return(NodeId<Return>),
        Become(NodeId<Become>),
        Call(NodeId<Call>),
        Method(NodeId<Method>),
        Field(NodeId<Field>),
        Index(NodeId<Index>),
        Literal(NodeId<Lit>),
        Symbol(NodeId<Symbol>),
        Covered(NodeId<Expr>),
    }
}

ast_struct! {
        pub struct If {
        pub condition: NodeId<Expr>,
        pub then: NodeId<Block>,
        pub otherwise: Option<NodeId<Block>>,
    }
}

ast_struct! {
        pub struct While {
        pub condition: NodeId<Expr>,
        pub then: NodeId<Block>,
    }
}

#[derive(Clone, Copy, Eq, PartialEq, Debug, Hash)]
pub enum BinOp {
    Add,
    Sub,
    Mull,
    Div,
    Rem,
    And,
    Or,
    BitXor,
    BitAnd,
    BitOr,
    Shl,
    Shr,
    Eq,
    Lt,
    Le,
    Ne,
    Ge,
    Gt,
    Assign,
    AddAssign,
    SubAssign,
    MullAssign,
    DivAssign,
    RemAssign,
    BitXorAssign,
    BitAndAssign,
    BitOrAssign,
    ShlAssign,
    ShrAssign,
}

#[cfg(feature = "print")]
impl<L, W> ast::AstDisplay<L, W> for BinOp
where
    L: NodeLibrary,
    W: std::fmt::Write,
{
    fn fmt(&self, fmt: &mut ast::AstFormatter<L, W>) -> std::fmt::Result {
        write!(fmt, "{:?}", self)
    }
}

ast_struct! {
        pub struct BinaryExpr {
        pub left: NodeId<Expr>,
        pub op: BinOp,
        pub right: NodeId<Expr>,
    }
}

#[derive(Clone, Copy, Eq, PartialEq, Debug, Hash)]
pub enum UnOp {
    Star,
    Not,
    Minus,
}

#[cfg(feature = "print")]
impl<L, W> ast::AstDisplay<L, W> for UnOp
where
    L: NodeLibrary,
    W: std::fmt::Write,
{
    fn fmt(&self, fmt: &mut ast::AstFormatter<L, W>) -> std::fmt::Result {
        write!(fmt, "{:?}", self)
    }
}

ast_struct! {
        pub struct UnaryExpr {
        pub op: UnOp,
        pub expr: NodeId<Expr>,
    }
}

ast_struct! {
        pub struct Cast {
        pub expr: NodeId<Expr>,
        pub ty: NodeId<Type>,
    }
}

ast_struct! {
        pub struct Break {
        pub expr: Option<NodeId<Expr>>,
    }
}

ast_struct! {
        pub struct Return {
        pub expr: Option<NodeId<Expr>>,
    }
}

ast_struct! {
        pub struct Become{
        pub callee: NodeId<Ident>,
        pub args: Option<NodeListId<Expr>>,
    }
}

ast_struct! {
        pub struct Let {
        pub sym: NodeId<Symbol>,
        pub mutable: bool,
        pub ty: Option<NodeId<Type>>,
        pub expr: NodeId<Expr>,
    }
}

ast_struct! {
        pub struct Call {
        pub func: NodeId<Expr>,
        pub args: Option<NodeListId<Expr>>,
    }
}

ast_struct! {
        pub struct Field {
        pub base: NodeId<Expr>,
        pub field: NodeId<Ident>,
    }
}

ast_struct! {
        pub struct Index {
        pub base: NodeId<Expr>,
        pub index: NodeId<Expr>,
    }
}

ast_struct! {
    pub struct Method {
        pub receiver: NodeId<Expr>,
        pub name: NodeId<Ident>,
        pub args: Option<NodeListId<Expr>>,
    }
}

ast_enum! {
    pub enum Type {
        Array(NodeId<TypeArray>),
        Fn(NodeId<TypeFn>),
        Tuple(NodeId<TypeTuple>),
        Ptr(NodeId<TypePtr>),
        Reference(NodeId<TypeReference>),
        Direct(NodeId<Ident>),
    }
}

ast_struct! {
        pub struct TypeArray {
        pub elem: NodeId<Type>,
        pub len: NodeId<Expr>,
    }
}

ast_struct! {
        pub struct TypeFn {
        pub params: Option<NodeListId<Type>>,
        pub output: Option<NodeId<Type>>,
    }
}

ast_struct! {
        pub struct Arg {
        pub sym: NodeId<Symbol>,
        pub ty: NodeId<Type>,
    }
}

ast_struct! {
        pub struct TypeTuple {
        pub fields: Option<NodeListId<Type>>,
    }
}

ast_struct! {
        pub struct TypePtr {
        pub mutable: bool,
        pub ty: NodeId<Type>,
    }
}

ast_struct! {
        pub struct TypeReference {
        pub mutable: bool,
        pub ty: NodeId<Type>,
    }
}

ast_struct! {
    pub struct Module {
        // Can be None when the module is reference externally.
        pub sym: Option<NodeId<Symbol>>,
        pub stencils: Option<NodeListId<Stencil>>,
    }
}

ast_struct! {
        pub struct Stencil {
        pub sym: NodeId<Symbol>,
        pub parameters: Option<NodeListId<Parameter>>,
        pub variants: Option<NodeListId<Variant>>,
        pub output: Option<NodeId<Type>>,
        /// Guaranteed to be Expr::Block
        pub body: NodeId<Expr>
    }
}

ast_struct! {
    pub struct Variant {
        pub variations: Option<NodeListId<Variation>>
    }
}

ast_enum! {
    pub enum Variation {
        Immediate(NodeId<VariationImmediate>),
        Slot(NodeId<VariationSlot>),
    }
}

ast_struct! {
    pub struct VariationImmediate {
        pub sym: NodeId<Symbol>,
    }
}

ast_struct! {
    pub struct VariationSlot{
        pub sym: NodeId<Symbol>,
    }
}

ast_struct! {
    pub struct Function {
        pub sym: NodeId<Symbol>,
        pub parameters: Option<NodeListId<Parameter>>,
        pub body: Option<NodeListId<Expr>>,
    }
}

ast_struct! {
    pub struct Parameter {
        pub sym: NodeId<Symbol>,
        pub ty: NodeId<Type>,
    }
}

ast_struct! {
    pub struct Block {
        pub body: Option<NodeListId<Expr>>,
        pub returns_last: bool,
    }
}

ast_struct! {
    pub struct Symbol {
        pub name: NodeId<Ident>,
    }
}
