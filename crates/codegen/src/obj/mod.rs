use std::collections::HashMap;

use object::{
    LittleEndian, Object as _, ObjectSection as _, ObjectSymbol as _, RelocationTarget,
    read::elf::ElfFile64,
};

#[derive(Debug)]
pub struct StencilSet {
    /// Stencil mapped by the name of the stencil.
    pub stencils: HashMap<String, Stencil>,
}

#[derive(Debug)]
pub struct Stencil {
    pub variants: Vec<StencilVariant>,
}

#[derive(Debug)]
pub struct StencilVariant {
    pub bytes: Vec<u8>,
    pub immediates: HashMap<String, Immediate>,
    pub jumps: HashMap<String, Jump>,
}

#[derive(Debug)]
pub struct Immediate {
    pub size: u32,
    pub offset: u32,
}

#[derive(Debug)]
pub struct Jump {
    pub size: u32,
    pub offset: u32,
    pub addend: i64,
    pub is_fallthrough: bool,
}

pub enum ObjErrors {
    InvalidObj(object::read::Error),
}

/// Extract a stencil from a compiled object file.
pub fn extract_stencil_variant(object_file_bytes: &[u8]) -> Result<StencilVariant, ObjErrors> {
    let object =
        ElfFile64::<LittleEndian>::parse(&object_file_bytes).map_err(ObjErrors::InvalidObj)?;

    let text_section = object.section_by_name_bytes(b".text").unwrap();

    let entry_symbol = object
        .symbols()
        .find(|s| s.name_bytes().ok() == Some(b"__main__"))
        .unwrap();

    let text = text_section
        .data_range(entry_symbol.address(), entry_symbol.size())
        .unwrap()
        .unwrap()
        .to_vec();

    let mut jumps = HashMap::new();
    let mut immediates = HashMap::new();

    for (offset, reloc) in text_section.relocations() {
        let RelocationTarget::Symbol(tgt) = reloc.target() else {
            continue;
        };

        let sym = object.symbol_by_index(tgt).unwrap();
        let name = sym.name().unwrap();

        if name.starts_with("__become_") {
            let name = name.split_at("__become_".len()).1.to_string();
            let size = (reloc.size() / 8) as u32;
            let is_fallthrough = (offset as u32 + size) == text.len() as u32
                // jmp instruction
                 && text[offset as usize - 1] == 233;
            let jump = Jump {
                size,
                offset: offset as u32,
                addend: reloc.addend() as i64,
                is_fallthrough,
            };
            jumps.insert(name, jump);
        } else if name.starts_with("__immediate_") {
            let size = (reloc.size() / 8) as u32;
            let name = name.split_at("__immediate_".len()).1.to_string();
            let imm = Immediate {
                offset: offset as u32,
                size,
            };
            immediates.insert(name, imm);
        }
    }

    Ok(StencilVariant {
        bytes: text,
        jumps,
        immediates,
    })
}
