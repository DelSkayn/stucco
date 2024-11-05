mod ast;
pub mod visit;

pub use ast::Node;
use ast::NodeLibrary;
pub use ast::{AstSpanned, NodeId, NodeList, NodeListId, PushNodeError, Span, Spanned};
use common::{id::IdSet, thinvec::ThinVec};
use syn::{Ident, Lit};

#[cfg(feature = "print")]
pub use ast::{AstDisplay, AstFormatter, AstRender};

type LibrarySet<T> = IdSet<u32, T>;

library!(Library {
    module: ThinVec<Module>,

    stencil_function: ThinVec<StencilFunction>,
    stencil_functions: ThinVec<NodeList<StencilFunction>>,

    variant: ThinVec<Variant>,
    variants: ThinVec<NodeList<Variant>>,

    variant_constant: ThinVec<VariantConstant>,

    parameter: ThinVec<Parameter>,
    parameters: ThinVec<NodeList<Parameter>>,

    expr: ThinVec<Expr>,
    exprs: ThinVec<NodeList<Expr>>,
    binary: ThinVec<BinaryExpr>,
    unary: ThinVec<UnaryExpr>,
    if_: ThinVec<If>,
    while_: ThinVec<While>,

    literal: ThinVec<Lit>,

    let_: ThinVec<Let>,

    method: ThinVec<Method>,
    call: ThinVec<Call>,
    field: ThinVec<Field>,
    index: ThinVec<Index>,

    break_: ThinVec<Break>,
    return_: ThinVec<Return>,
    tail: ThinVec<Tail>,

    type_: ThinVec<Type>,
    types: ThinVec<NodeList<Type>>,
    type_fn: ThinVec<TypeFn>,
    type_array: ThinVec<TypeArray>,
    type_tuple: ThinVec<TypeTuple>,
    type_ptr: ThinVec<TypePtr>,
    type_reference: ThinVec<TypeReference>,

    arg: ThinVec<Arg>,
    args: ThinVec<NodeList<Arg>>,

    symbol: ThinVec<Symbol>,

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
        Tail(NodeId<Tail>),
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
        pub then: Option<NodeListId<Expr>>,
        pub otherwise: Option<NodeListId<Expr>>,
    }
}

ast_struct! {
        pub struct While {
        pub condition: NodeId<Expr>,
        pub then: Option<NodeListId<Expr>>,
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
        pub struct Tail{
        pub callee: NodeId<Ident>,
        pub args: Option<NodeListId<Expr>>,
    }
}

ast_struct! {
        pub struct Let {
        pub sym: NodeId<Symbol>,
        pub mutable: bool,
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
        pub sym: NodeId<Symbol>,
        pub functions: Option<NodeListId<StencilFunction>>,
    }
}

ast_struct! {
        pub struct StencilFunction {
        pub sym: NodeId<Symbol>,
        pub entry: bool,
        pub variants: Option<NodeListId<Variant>>,
        pub parameters: Option<NodeListId<Parameter>>,
        pub output: Option<NodeId<Type>>,
        pub body: Option<NodeListId<Expr>>,
    }
}

ast_enum! {
        pub enum Variant {
        Constant(NodeId<VariantConstant>),
    }
}

ast_struct! {
        pub struct VariantConstant {
        pub sym: NodeId<Symbol>,
        pub ty: NodeId<Type>,
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
        pub body: Option<NodeListId<Expr>>
    }
}

ast_struct! {
    pub struct Symbol {
        pub name: NodeId<Ident>,
    }
}
