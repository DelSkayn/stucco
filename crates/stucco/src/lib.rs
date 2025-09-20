mod bytecode;
pub mod stencil;

use std::{marker::PhantomData, ops::Range};

pub use bytecode::{Bytecode, MappedFunction};
use stencil::{Cont, Fallthrough, Immediate, Jump, Module, Variant};

//pub use derive::{file, module};

#[derive(Debug)]
struct InstantiatedJump {
    jump: Jump,
    target: Option<usize>,
}

#[derive(Debug)]
struct InstantiatedVariant {
    bytes: &'static [u8],
    jumps: Range<usize>,
    imms: Range<usize>,
    place: usize,
}

pub struct Builder<M> {
    __marker: PhantomData<M>,
    variants: Vec<InstantiatedVariant>,
    jumps: Vec<InstantiatedJump>,
    immediates: Vec<Immediate>,
}

pub struct JumpTarget(usize);

impl<M: Module> Builder<M> {
    pub fn new() -> (Self, Cont) {
        (
            Builder {
                __marker: PhantomData,
                variants: vec![InstantiatedVariant {
                    bytes: M::BYTECODE,
                    jumps: 0..1,
                    imms: 0..0,
                    place: 0,
                }],
                jumps: vec![InstantiatedJump {
                    jump: M::ENTRY,
                    target: None,
                }],
                immediates: Vec::new(),
            },
            Cont(0),
        )
    }

    pub fn then_variant<V>(&mut self, cont: Cont, args: V::Args) -> (JumpTarget, V::Jumps)
    where
        V: Variant<Module = M>,
    {
        let jump_offset = self.jumps.len();
        let jumps = V::collect_jumps(&mut |jump| {
            let cont = self.jumps.len();
            self.jumps.push(InstantiatedJump { jump, target: None });
            Cont(cont)
        });
        let jumps_range = jump_offset..self.jumps.len();

        let imm_offset = self.immediates.len();
        V::produce_immediates(args, &mut |imm| {
            self.immediates.push(imm);
        });
        let imms = imm_offset..self.immediates.len();

        let jmp_target = self.variants.len();
        self.jumps[cont.0].target = Some(jmp_target);

        self.variants.push(InstantiatedVariant {
            bytes: V::BYTECODE,
            jumps: jumps_range,
            imms,
            place: 0,
        });
        (JumpTarget(jmp_target), jumps)
    }

    pub fn then_jump(&mut self, cont: Cont, target: JumpTarget) {
        self.jumps[cont.0].target = Some(target.0);
    }

    pub fn build(mut self) -> Bytecode<M> {
        let mut order = Vec::new();
        // TODO: use bitmap;
        let mut reached = vec![false; self.variants.len()];
        let mut stack = vec![0];

        dbg!(&self.variants);
        dbg!(&self.jumps);

        // Topologically sort the variants prefering fallthroughs wherever possible.
        while let Some(next) = stack.pop() {
            if reached[next] {
                continue;
            }
            reached[next] = true;
            self.variants[next].place = order.len();
            order.push(next);

            let jumps = self.variants[next].jumps.clone();
            let jumps = &self.jumps[jumps];
            let mut last = None;
            for j in jumps {
                if let Fallthrough::Omit(_) = j.jump.fallthrough {
                    last = Some(j);
                    continue;
                }

                stack.push(j.target.expect("not all jumps had a continuation set"))
            }
            if let Some(l) = last {
                stack.push(l.target.expect("not all jumps had a continuation set"))
            }
        }

        // write out all the bytes in the topological order, keeping track of the byte offset of
        // every variant.
        let mut byte_offsets = vec![0; self.variants.len()];
        let mut fallthrough_valid = vec![false; self.variants.len()];

        let mut bytes = Vec::new();
        for (idx, o) in order.iter().copied().enumerate() {
            let offset = bytes.len();
            byte_offsets[o] = offset;

            let fallthrough = self.jumps[self.variants[o].jumps.clone()]
                .iter()
                .find_map(|j| {
                    if let Fallthrough::Omit(x) = j.jump.fallthrough {
                        if j.target.unwrap() == idx + 1 {
                            Some(x)
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                });

            if let Some(omit) = fallthrough {
                let b = self.variants[o].bytes;
                bytes.extend_from_slice(&b[..b.len() - (omit as usize)]);
                fallthrough_valid[o] = true
            } else {
                bytes.extend_from_slice(self.variants[o].bytes);
            }

            let imms = self.variants[o].imms.clone();
            let imms = &self.immediates[imms];
            for imm in imms {
                match imm.data {
                    stencil::ImmediateData::U32(x) => {
                        let offset = imm.offset as usize + offset;
                        let end = offset + 4;
                        bytes[offset..end].copy_from_slice(&x.to_ne_bytes());
                    }
                    stencil::ImmediateData::U64(x) => {
                        let offset = imm.offset as usize + offset;
                        let end = offset + 8;
                        bytes[offset..end].copy_from_slice(&x.to_ne_bytes());
                    }
                }
            }
        }

        // Patch all the jumps to the proper location.
        for (idx, v) in self.variants.iter().enumerate() {
            let jumps = v.jumps.clone();
            let jumps = &self.jumps[jumps];
            for j in jumps {
                if matches!(j.jump.fallthrough, Fallthrough::Omit(_)) && fallthrough_valid[idx] {
                    continue;
                }

                let tgt_offset = byte_offsets[j.target.unwrap()];
                let self_offset = byte_offsets[idx];
                let jump_instr_offset = self_offset + j.jump.offset as usize;
                let jump_value =
                    (tgt_offset as i64 - jump_instr_offset as i64) as i32 + j.jump.addend;
                match j.jump.size {
                    stencil::JumpSize::Default => {
                        let range = jump_instr_offset..(jump_instr_offset + 4);
                        bytes[range].copy_from_slice(&jump_value.to_ne_bytes());
                    }
                    stencil::JumpSize::Long => {
                        let range = jump_instr_offset..(jump_instr_offset + 8);
                        bytes[range].copy_from_slice(&(jump_value as i64).to_ne_bytes());
                    }
                }
            }
        }

        Bytecode {
            code: bytes,
            __marker: PhantomData,
        }
    }
}
