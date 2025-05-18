mod obj;
mod target;

#[cfg(not(any(feature = "stand-alone", feature = "proc-macro")))]
compile_error!(
    "Missing feature on stucco_codegen, either feature stand-alone or feature proc-macro must be enabled"
);

pub struct StencilSet {
    stencils: Vec<Stencil>,
}

pub struct Stencil {
    name: String,
    variants: Vec<StencilVariant>,
}

pub struct StencilVariant {
    variations: Vec<Variation>,
    jumps: Vec<StencilJump>,
    code: Vec<u8>,
}

pub struct Variation {
    name: String,
    kind: VariationKind,
}

pub enum VariationKind {
    SlotReg { register: u8 },
    SlotLoad { offset: u32 },
    Const { offset: u32, kind: ConstKind },
}

pub enum ConstKind {
    Clobbered,
    NonClobbered,
}

pub struct StencilJump {
    name: String,
    offset: u32,
    link: JumpLink,
    is_fallthrough: bool,
}

pub enum JumpLink {}
