pub use derive::{file, module};

pub struct Jumps {
    pub jumps: &'static [(&'static str, Jump)],
}

impl Jumps {
    pub fn get(&self, name: &str) -> Option<&Jump> {
        self.jumps.iter().find(|x| x.0 == name).map(|x| &x.1)
    }

    pub fn all(&self) -> impl Iterator<Item = &(&'static str, Jump)> {
        self.jumps.iter()
    }
}

pub struct Jump {
    pub size: u32,
    pub offset: u32,
    pub addend: i64,
    pub is_fallthrough: bool,
}

pub struct Immediates {
    pub imms: &'static [(&'static str, Immediate)],
}

impl Immediates {
    pub fn get(&self, name: &str) -> Option<&Immediate> {
        self.imms.iter().find(|x| x.0 == name).map(|x| &x.1)
    }

    pub fn all(&self) -> impl Iterator<Item = &(&'static str, Immediate)> {
        self.imms.iter()
    }
}

pub struct Immediate {
    pub size: u32,
    pub offset: u32,
}

pub struct Variant {
    pub bytes: &'static [u8],
    pub jumps: Jumps,
    pub immediates: Immediates,
}

impl Variant {
    pub fn bytes(&self) -> &'static [u8] {
        self.bytes
    }

    pub fn jumps(&self) -> &Jumps {
        &self.jumps
    }

    pub fn immediates(&self) -> &Immediates {
        &self.immediates
    }
}

pub struct StencilData {
    pub variants: &'static [Variant],
}

impl StencilData {
    pub fn variants(&self) -> &[Variant] {
        self.variants
    }
}

pub unsafe trait Stencil {
    fn data() -> &'static StencilData;
}
