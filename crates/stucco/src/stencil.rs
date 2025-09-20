/// A stencil module.
///
/// every set of stencils belong to a given module.
/// This module defines the type of the final build function as well as bytecode for entering into
/// a build function.
pub unsafe trait Module {
    /// The type of function this module produces once successfully compiled.
    type Args;
    type Result;

    unsafe fn call(func_ptr: *mut (), args: Self::Args) -> Self::Result;

    /// The bytecode of the entry.
    const BYTECODE: &[u8];
    /// The first jump to start the chain of variants.
    const ENTRY: Jump;
}

#[derive(Debug)]
pub enum JumpSize {
    /// default 32 bit jump target
    Default,
    /// a long 64 bit jump, generally rare.
    Long,
}

#[derive(Debug)]
pub enum Fallthrough {
    /// This jump is not a fallthrough jump
    None,
    /// This jump can be made fallthrough by omiting the given number of bytes from the jump
    /// byte code.
    Omit(u16),
}

#[derive(Debug)]
pub struct Jump {
    /// The size of the jump data in bytes
    pub size: JumpSize,
    /// The offset into the variant, from the start of the variant.
    pub offset: u32,
    /// The addened value which must be added to the calculated jump offset to produce the data which must be
    /// written into the bytecode.
    pub addend: i32,
    /// Is this jump a fallthrough jump, if the jump is a fallthrough the jump can be optimized by
    /// inserting the to target variant as the next variant and removing the jump instruction in
    /// it's entirity from the byte code.
    pub fallthrough: Fallthrough,
}

#[derive(Debug)]
pub enum ImmediateData {
    U32(u32),
    U64(u64),
}

#[derive(Debug)]
pub struct Immediate {
    pub data: ImmediateData,
    pub offset: u32,
}

/// A compiled variant of a stencil.
///
/// This represents an actual piece of bytecode along with jumps and possible immediates that, when
/// combined, result in a part of the final compiled function.
pub unsafe trait Variant {
    /// The module to which this variant belongs to.
    type Module: Module;

    /// Type containing input required to build to variant, like immediates.
    type Args;
    /// Type containing the continuation of jumps which this stencil produces.
    type Jumps;

    /// The actual bytecode of the variant.
    const BYTECODE: &[u8];

    /// A function, which when called will callback with a set of immediate values produced from
    /// the given argument.
    fn produce_immediates<F: FnMut(Immediate)>(args: Self::Args, f: &mut F);

    /// A function, which when called will callback with a set of jumps
    fn collect_jumps<F: FnMut(Jump) -> Cont>(f: &mut F) -> Self::Jumps;
}

/// A continuation of a variant jump.
///
/// Variants generally have a set of jumps from which one variant can jump into the next.
/// When instantiated these jumps produce continuations which must be filled with another
/// instantiated variant.
pub struct Cont(pub(crate) usize);
