//! This is a basic brainfuck jit-compiler.
//!
//! For simplicity it doesn't do any bounds checking on the stack and operates on a pointer to some
//! large stack handed to it by the caller of the function.

use std::{
    io::{Read, Write},
    time::Instant,
};

// The macro which compiles all the variants and generates the rust code required to build a
// function.
// This macro loads the definition from the file at the given path.
stucco::file!(bf => "./examples/brainfuck_import/brainfuck.stucco");

pub struct Reader<'a> {
    bytes: &'a [u8],
}

impl<'a> Reader<'a> {
    pub fn new(bytes: &'a [u8]) -> Self {
        Reader { bytes }
    }

    pub fn next(&mut self) -> Option<u8> {
        if let Some(x) = self.bytes.first() {
            self.bytes = &self.bytes[1..];
            Some(*x)
        } else {
            None
        }
    }

    pub fn peek(&self) -> Option<u8> {
        self.bytes.first().copied()
    }

    pub fn eat_count(&mut self, b: u8) -> usize {
        let mut count = 0;
        while self.peek() == Some(b) {
            count += 1;
            self.next();
        }
        count
    }
}

/// The byte code variants have no access to io or the wider rust standard library
/// so in order to do IO it needs to call out to rust.
///
/// This function implements the gets operation and will be called from the jit-compiled function
/// whenever it needs to do a get. We hand the address to this function as an immediate to when
/// generating a `GetsVariant`.
fn gets() -> u64 {
    std::io::stdin()
        .bytes()
        .next()
        .transpose()
        .ok()
        .flatten()
        .map(|x| x as u64)
        .unwrap_or(0)
}

/// Similar function as `gets` but for the put operation.
fn put(v: u64) {
    let _ = std::io::stdout().write_all(&[v as u8]);
}

fn main() {
    // Read in the brainfuck file.
    let src = if let Some(arg) = std::env::args().skip(1).next() {
        std::fs::read_to_string(arg).unwrap()
    } else {
        let mut buf = String::new();
        std::io::stdin().read_to_string(&mut buf).unwrap();
        buf
    };

    // Setup the bytecode builder.
    let (mut builder, cont) = stucco::Builder::<bf::Module>::new();

    let mut reader = Reader::new(file.as_bytes());
    let mut jump_stack = Vec::new();

    let before_compile = Instant::now();

    // compile the function.
    let mut next = cont;
    while let Some(x) = reader.peek() {
        match x {
            b'>' => {
                let count = reader.eat_count(b'>');
                next = builder
                    .then_variant(
                        next,
                        bf::NextVariant0 {
                            count: count as u64,
                        },
                    )
                    .1
                    .next;
            }
            b'<' => {
                let count = reader.eat_count(b'<');
                next = builder
                    .then_variant(
                        next,
                        bf::PreviousVariant0 {
                            count: count as u64,
                        },
                    )
                    .1
                    .next;
            }
            b'+' => {
                let count = reader.eat_count(b'+');
                next = builder
                    .then_variant(next, bf::IncrementVariant0 { by: count as u64 })
                    .1
                    .next;
            }
            b'-' => {
                let count = reader.eat_count(b'-');
                next = builder
                    .then_variant(next, bf::DecrementVariant0 { by: count as u64 })
                    .1
                    .next;
            }
            b',' => {
                reader.next();
                next = builder
                    .then_variant(
                        next,
                        bf::GetsVariant0 {
                            read: unsafe { std::mem::transmute(gets as fn() -> u64) },
                        },
                    )
                    .1
                    .next;
            }
            b'.' => {
                reader.next();
                next = builder
                    .then_variant(
                        next,
                        bf::PutVariant0 {
                            write: unsafe { std::mem::transmute(put as fn(u64)) },
                        },
                    )
                    .1
                    .next;
            }
            b'[' => {
                reader.next();
                let (tgt, jumps) = builder.then_variant(next, bf::JumpForwardVariant0::default());
                next = jumps.next;
                jump_stack.push((tgt, jumps.forward));
            }
            b']' => {
                reader.next();
                let (tgt, jumps) = builder.then_variant(next, bf::JumpBackwardVariant0::default());
                next = jumps.next;
                let (backward_tgt, forward) = jump_stack.pop().expect("Unbalanced brackets!");
                builder.then_jump(jumps.backward, backward_tgt);
                builder.then_jump(forward, tgt);
            }
            _ => {
                reader.next();
            }
        }
    }

    assert!(jump_stack.is_empty(), "Unbalanced brackets");

    // Add a halt at the end so that all continuations are complete.
    builder.then_variant(next, bf::HaltVariant0 {});
    let bytes = builder.build();

    // Map the compiled function into executable memory.
    let func = bytes.into_mapped_function();

    // Large stack so that we don't overrun.
    // This simple example does no bounds check on the stack.
    let mut stack = vec![0u64; 1024 * 1024];
    let before_run = Instant::now();

    // An then just call the compiled function with a stack.
    func.call((stack.as_mut_ptr(),));

    let full = before_compile.elapsed().as_secs_f32();
    let run = before_run.elapsed().as_secs_f32();

    let _ = std::io::stdout().flush();

    println!("time taken: full {full}, run {run}",)
}
