use object::{LittleEndian, read::elf::ElfFile64};

pub enum ObjErrors {
    InvalidObj(object::read::Error),
}

/// Extract a stencil from a compiled object file.
pub fn extract_stencil(object_file_bytes: &[u8]) -> Result<(), ObjErrors> {
    let _ = ElfFile64::<LittleEndian>::parse(object_file_bytes).map_err(ObjErrors::InvalidObj)?;

    todo!()
}

/*
pub fn extract_patches(object: &[u8]) {
    let object: ElfFile64 = ElfFile64::parse(object).unwrap();

    let text_section = object.section_by_name_bytes(b".text").unwrap();

    let entry_symbol = object
        .symbols()
        .find(|s| s.name_bytes().ok() == Some(b"__stencil__"))
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
            jumps.insert(
                name.split_at("__become_".len()).1.to_string(),
                JumpPatch {
                    target_size: (reloc.size() / 8) as usize,
                    offset: offset as usize,
                    addend: reloc.addend() as isize,
                },
            );
        } else if name.starts_with("__patch_global__") {
            immediates.insert(
                name.split_at("__patch_global__".len()).1.to_string(),
                ImmediatePatch {
                    target_size: (reloc.size() / 8) as usize,
                    offset: offset as usize,
                },
            );
        }
    }

    Patches {
        text,
        jumps,
        immediates,
    }
}
*/
