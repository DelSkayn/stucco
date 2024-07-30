mod page;
pub use page::JitFunction;

use object::write::Symbol;
use std::{
    collections::{HashMap, VecDeque},
    marker::PhantomData,
};
pub use stucco_derive::template;

const JUMP_INSTRUCTION: u8 = 0xe9;

#[derive(Clone, Debug)]
pub enum Ty {
    U64,
}

#[derive(Clone, Debug)]
pub struct TemplateConstant {
    name: &'static str,
    ty: Ty,
    offset: usize,
    target_size: usize,
}

impl TemplateConstant {
    #[doc(hidden)]
    pub const fn __create(name: &'static str, ty: Ty, offset: usize, target_size: usize) -> Self {
        TemplateConstant {
            name,
            ty,
            offset,
            target_size,
        }
    }
}

#[derive(Clone, Debug)]
pub struct TemplateJump {
    name: &'static str,
    args: &'static [Ty],
    offset: usize,
    addend: isize,
    target_size: usize,
}

impl TemplateJump {
    #[doc(hidden)]
    pub const fn __create(
        name: &'static str,
        args: &'static [Ty],
        offset: usize,
        addend: isize,
        target_size: usize,
    ) -> Self {
        TemplateJump {
            name,
            args,
            offset,
            addend,
            target_size,
        }
    }
}

pub struct Entry;
pub struct Plain;

pub unsafe trait JitFunc {
    type Args;
    type Output;

    unsafe fn call(fn_ptr: *const u8, args: Self::Args) -> Self::Output;
}

pub trait EntryTemplate<Fn> {
    const BYTES: &[u8];
    const CONSTANTS: &[TemplateConstant];
    const JUMPS: &[TemplateJump];
}

pub trait Template<Args> {
    const BYTES: &[u8];
    const CONSTANTS: &[TemplateConstant];
    const JUMPS: &[TemplateJump];
}

pub struct FuncInstructions(Vec<u8>);

impl FuncInstructions {
    pub fn dump_to_obj(&self) -> Vec<u8> {
        let mut obj = object::write::Object::new(
            object::BinaryFormat::Elf,
            object::Architecture::X86_64,
            object::Endianness::Little,
        );
        let main_symbol = obj.add_symbol(Symbol {
            name: b"jit_func".into(),
            value: 0,
            size: 0,
            kind: object::SymbolKind::Text,
            scope: object::SymbolScope::Linkage,
            weak: false,
            section: object::write::SymbolSection::Undefined,
            flags: object::SymbolFlags::None,
        });

        let main_section = obj.add_subsection(object::write::StandardSection::Text, b"jit_func");
        let _main_offset = obj.add_symbol_data(main_symbol, main_section, &self.0, 1);

        obj.write().unwrap()
    }
}

fn patch_slice(tgt: &mut [u8], offset: usize, s: &[u8]) {
    println!("Patching at {:x}", offset);
    tgt[offset..(offset + s.len())].copy_from_slice(s)
}

fn patch_u32(tgt: &mut [u8], offset: usize, v: u32) {
    patch_slice(tgt, offset, &v.to_ne_bytes())
}

fn patch_i32(tgt: &mut [u8], offset: usize, v: i32) {
    patch_slice(tgt, offset, &v.to_ne_bytes())
}

fn patch_i64(tgt: &mut [u8], offset: usize, v: i64) {
    patch_slice(tgt, offset, &v.to_ne_bytes())
}

fn patch_u64(tgt: &mut [u8], offset: usize, v: u64) {
    patch_slice(tgt, offset, &v.to_ne_bytes())
}

#[derive(Debug)]
pub struct PartJump {
    offset: usize,
    addend: isize,
    size: usize,
    target: Option<PartId>,
    patched: bool,
}

impl PartJump {
    fn from_template(j: &TemplateJump) -> Self {
        PartJump {
            offset: j.offset,
            addend: j.addend,
            size: j.target_size,
            target: None,
            patched: false,
        }
    }

    fn is_fallthrough_jump(&self, bytes: &[u8]) -> bool {
        if self.offset + self.size as usize != bytes.len() {
            return false;
        }

        bytes
            .get(self.offset - 1)
            .map(|x| *x == JUMP_INSTRUCTION)
            .unwrap_or(false)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct PartId(usize);

#[derive(Debug, Clone, Copy)]
pub struct JumpId(usize);

#[derive(Debug)]
pub struct Part {
    text: &'static [u8],
    constants: &'static [TemplateConstant],
    constants_values: Vec<u64>,
    // offset once instantiated.
    offset: Option<usize>,
    jumps: HashMap<&'static str, JumpId>,
    did_jumps: bool,
}

#[derive(Debug)]
pub struct Builder<Fn> {
    tree: Vec<Part>,
    jumps: Vec<PartJump>,
    _marker: PhantomData<Fn>,
}

impl<Fn> Builder<Fn> {
    pub fn new() -> Self {
        Builder {
            tree: Vec::new(),
            jumps: Vec::new(),
            _marker: PhantomData,
        }
    }

    pub fn next_part(&mut self) -> PartId {
        let id = self.tree.len();
        PartId(id)
    }

    pub fn start<T: EntryTemplate<Fn>>(&mut self, constants: &[u64]) -> PartId {
        assert!(self.tree.is_empty());
        assert!(T::CONSTANTS.is_empty());
        assert_eq!(
            T::CONSTANTS.len(),
            constants.len(),
            "invalid number of constants"
        );

        self.tree.push(Part {
            text: T::BYTES,
            offset: None,
            constants: T::CONSTANTS,
            constants_values: constants.to_vec(),
            did_jumps: false,
            jumps: T::JUMPS
                .iter()
                .map(|x| {
                    let id = self.jumps.len();
                    self.jumps.push(PartJump::from_template(x));
                    (x.name, JumpId(id))
                })
                .collect(),
        });

        PartId(0)
    }

    pub fn push<T: Template<Fn>>(&mut self, constants: &[u64]) -> PartId {
        let id = self.tree.len();
        assert_eq!(
            T::CONSTANTS.len(),
            constants.len(),
            "invalid number of constants"
        );

        self.tree.push(Part {
            text: T::BYTES,
            offset: None,
            constants: T::CONSTANTS,
            constants_values: constants.to_vec(),
            did_jumps: false,
            jumps: T::JUMPS
                .iter()
                .map(|x| {
                    let id = self.jumps.len();
                    self.jumps.push(PartJump::from_template(x));
                    (x.name, JumpId(id))
                })
                .collect(),
        });

        PartId(id)
    }

    #[track_caller]
    pub fn jump_to(&mut self, name: &str, from: PartId, to: PartId) {
        let idx = self.tree[from.0]
            .jumps
            .get_mut(name)
            .expect("jump did not exist")
            .0;

        let r = self.jumps[idx].target.replace(to);
        if r.is_some() {
            panic!("rewrote jump")
        }
    }

    pub fn finish(mut self) -> FuncInstructions {
        let mut buffer = Vec::new();
        let mut pending = VecDeque::new();
        self.write_bytes(PartId(0), &mut buffer, &mut pending);
        while let Some(p) = pending.pop_front() {
            self.write_pending(p, &mut buffer, &mut pending);
        }

        for (idx, p) in self.tree.iter().enumerate() {
            print!("{} @ {:0>2x} = ", idx, p.offset.unwrap());
            for p in p.text {
                print!("{:0>2x} ", p);
            }
            println!()
        }

        for (idx, p) in self.tree.iter().enumerate() {
            print!("from {idx:0>2} to ");
            for (n, j) in p.jumps.iter() {
                print!("{n}={}, ", self.jumps[j.0].target.unwrap().0);
            }
            println!()
        }

        self.patch_jumps(PartId(0), &mut buffer, &mut pending);
        while let Some(p) = pending.pop_front() {
            let tgt = self.jumps[p.0].target.unwrap();
            self.patch_jumps(tgt, &mut buffer, &mut pending)
        }

        FuncInstructions(buffer)
    }

    pub fn write_bytes(
        &mut self,
        current: PartId,
        buffer: &mut Vec<u8>,
        pending: &mut VecDeque<JumpId>,
    ) {
        let offset = buffer.len();
        self.tree[current.0].offset = Some(offset);
        buffer.extend_from_slice(self.tree[current.0].text);

        for (c, v) in self.tree[current.0]
            .constants
            .iter()
            .zip(self.tree[current.0].constants_values.iter())
        {
            let offset = c.offset + offset;
            if c.target_size == 8 {
                patch_u64(buffer, offset, *v)
            } else {
                patch_u32(buffer, offset, (*v) as u32)
            }
        }

        let text = self.tree[current.0].text;
        let mut fallthrough = None;
        let mut found = false;

        for j in self.tree[current.0].jumps.values().copied() {
            let jump = &mut self.jumps[j.0];
            let unwritten = self.tree[jump.target.unwrap().0].offset.is_none();

            if unwritten && jump.is_fallthrough_jump(text) {
                assert!(fallthrough.is_none());
                fallthrough = Some(j);
                if found {
                    break;
                }
                continue;
            }
            if unwritten {
                found = true;
                pending.push_back(j);
            }
        }

        let Some(fallthrough) = fallthrough else {
            return;
        };

        let Some(tgt) = self.jumps[fallthrough.0].target else {
            panic!("found dangling jump")
        };

        let size = self.jumps[fallthrough.0].size;
        self.jumps[fallthrough.0].patched = true;

        if self.tree[tgt.0].offset.is_some() {
            return;
        }

        for _ in 0..size {
            assert_eq!(buffer.pop(), Some(0));
        }
        assert_eq!(buffer.pop(), Some(JUMP_INSTRUCTION));

        self.write_bytes(tgt, buffer, pending)
    }

    pub fn write_pending(
        &mut self,
        current: JumpId,
        buffer: &mut Vec<u8>,
        pending: &mut VecDeque<JumpId>,
    ) {
        let tgt = self.jumps[current.0].target.unwrap();
        if self.tree[tgt.0].offset.is_some() {
            return;
        }
        self.write_bytes(tgt, buffer, pending)
    }

    pub fn patch_jumps(
        &mut self,
        current: PartId,
        buffer: &mut Vec<u8>,
        pending: &mut VecDeque<JumpId>,
    ) {
        if self.tree[current.0].did_jumps {
            return;
        }
        self.tree[current.0].did_jumps = true;
        for j in self.tree[current.0].jumps.values().copied() {
            pending.push_back(j);
            if self.jumps[j.0].patched {
                continue;
            }
            self.jumps[j.0].patched = true;

            let tgt = self.jumps[j.0].target.unwrap();

            let target_address = self.tree[tgt.0].offset.unwrap();
            let source_address = self.tree[current.0].offset.unwrap() + self.jumps[j.0].offset;

            let jump_imm = (target_address as isize + self.jumps[j.0].addend
                - source_address as isize) as usize;

            if self.jumps[j.0].size == 4 {
                patch_i32(buffer, source_address, jump_imm as i32)
            } else {
                patch_i64(buffer, source_address, jump_imm as i64)
            }
        }
    }
}
