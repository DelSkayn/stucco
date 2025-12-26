mod ast;
pub mod visit;

use ast::NodeLibrary;
pub use ast::{AstSpanned, NodeId, NodeList, NodeListId, PushNodeError};
pub use ast::{Node, UniqueNode};
use common::{id::IdSet, u32_vec::U32Vec};
use token::{
    Span,
    token::{Ident, Lit},
};

#[cfg(feature = "print")]
pub use ast::{AstDisplay, AstFormatter, AstRender};

type LibrarySet<T> = IdSet<u32, T>;

library!(Library {
    module: U32Vec<Module>,

    stmt: U32Vec<Stmt>,
    stmts: U32Vec<NodeList<Stmt>>,

    r#struct: U32Vec<Struct>,
    field: U32Vec<Field>,
    fields: U32Vec<NodeList<Field>>,

    function: U32Vec<Function>,

    stencil_function: U32Vec<Stencil>,
    stencil_functions: U32Vec<NodeList<Stencil>>,

    variant: U32Vec<Variant>,
    variants: U32Vec<NodeList<Variant>>,

    variation: U32Vec<Variation>,
    variations: U32Vec<NodeList<Variation>>,

    variation_slot: U32Vec<VariationSlot>,
    variation_imm: U32Vec<VariationImmediate>,
    variation_const: U32Vec<VariationConst>,

    parameter: U32Vec<Parameter>,
    parameters: U32Vec<NodeList<Parameter>>,

    expr: U32Vec<Expr>,
    exprs: U32Vec<NodeList<Expr>>,
    binary: U32Vec<BinaryExpr>,
    unary: U32Vec<UnaryExpr>,
    if_: U32Vec<If>,
    while_: U32Vec<While>,
    loop_: U32Vec<Loop>,
    block: U32Vec<Block>,

    literal: U32Vec<Lit>,

    let_: U32Vec<Let>,

    method: U32Vec<Method>,
    call: U32Vec<Call>,
    field_expr: U32Vec<FieldExpr>,
    index: U32Vec<Index>,
    cast: U32Vec<Cast>,

    break_: U32Vec<Break>,
    return_: U32Vec<Return>,
    tail: U32Vec<Become>,

    type_: U32Vec<Type>,
    types: U32Vec<NodeList<Type>>,
    type_fn: U32Vec<TypeFn>,
    type_array: U32Vec<TypeArray>,
    type_tuple: U32Vec<TypeTuple>,
    type_ptr: U32Vec<TypePtr>,
    type_reference: U32Vec<TypeReference>,

    arg: U32Vec<Arg>,
    args: U32Vec<NodeList<Arg>>,

    symbol: U32Vec<Symbol>,
    type_name: U32Vec<TypeName>,

    #[set]
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
        Loop(NodeId<Loop>),
        While(NodeId<While>),
        Let(NodeId<Let>),
        Continue(Span),
        Break(NodeId<Break>),
        Return(NodeId<Return>),
        Become(NodeId<Become>),
        Call(NodeId<Call>),
        Method(NodeId<Method>),
        Field(NodeId<FieldExpr>),
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

ast_struct! {
    pub struct Loop {
        pub body: NodeId<Block>,
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
    pub struct FieldExpr {
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
        Fn(NodeId<TypeFn>),
        Ptr(NodeId<TypePtr>),
        Name(NodeId<TypeName>),
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
    pub struct Field{
        pub name: NodeId<Ident>,
        pub ty: NodeId<Type>,
    }
}

ast_struct! {
    pub struct Module {
        // Can be None when the module is reference externally.
        pub sym: Option<NodeId<Symbol>>,
        pub stmts: Option<NodeListId<Stmt>>,
    }
}

ast_enum! {
    pub enum Stmt{
        Stencil(NodeId<Stencil>),
        Struct(NodeId<Struct>),
        Function(NodeId<Function>),
    }
}

ast_struct! {
    pub struct Struct{
        pub public: bool,
        pub name: NodeId<TypeName>,
        pub fields: Option<NodeListId<Field>>,
    }
}

ast_struct! {
    pub struct Function{
        pub sym: NodeId<Symbol>,
        pub parameters: Option<NodeListId<Parameter>>,
        pub output: Option<NodeId<Type>>,
        /// Guaranteed to be Expr::Block
        pub body: NodeId<Expr>
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
        pub name: NodeId<Ident>,
        pub variations: Option<NodeListId<Variation>>
    }
}

ast_enum! {
    pub enum Variation {
        Immediate(NodeId<VariationImmediate>),
        Slot(NodeId<VariationSlot>),
        Const(NodeId<VariationConst>)
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
    pub struct VariationConst{
        pub sym: NodeId<Symbol>,
        pub expr: NodeId<Expr>,
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

ast_struct! {
    pub struct TypeName {
        pub name: NodeId<Ident>,
    }
}
