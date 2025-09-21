use std::{
    io::{Read, Write},
    time::Instant,
};

use stucco::stencil::Cont;

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

fn put(v: u64) {
    let _ = std::io::stdout().write_all(&[v as u8]);
}

fn main() {
    // Read in the brainfuck file
    let path = std::env::args().skip(1).next().expect("missing argument");
    let file = std::fs::read_to_string(path).expect("Failed to read bf file");

    let before_compile = Instant::now();

    // Setup the builder.
    let (mut builder, cont) = stucco::Builder::<bf::Module>::new();

    let mut reader = Reader::new(file.as_bytes());
    let mut jump_stack = Vec::new();

    // build the function according to the brainfuck file.
    let mut next = cont;
    while let Some(x) = reader.peek() {
        match x {
            b'>' => {
                let count = reader.eat_count(b'>');
                next = builder
                    .then_variant::<bf::NextVariant0>(
                        next,
                        bf::NextVariant0Args {
                            count: count as u64,
                        },
                    )
                    .1
                    .next;
            }
            b'<' => {
                let count = reader.eat_count(b'<');
                next = builder
                    .then_variant::<bf::PreviousVariant0>(
                        next,
                        bf::PreviousVariant0Args {
                            count: count as u64,
                        },
                    )
                    .1
                    .next;
            }
            b'+' => {
                let count = reader.eat_count(b'+');
                next = builder
                    .then_variant::<bf::IncrementVariant0>(
                        next,
                        bf::IncrementVariant0Args { by: count as u64 },
                    )
                    .1
                    .next;
            }
            b'-' => {
                let count = reader.eat_count(b'-');
                next = builder
                    .then_variant::<bf::DecrementVariant0>(
                        next,
                        bf::DecrementVariant0Args { by: count as u64 },
                    )
                    .1
                    .next;
            }
            b',' => {
                reader.next();
                next = builder
                    .then_variant::<bf::GetsVariant0>(
                        next,
                        bf::GetsVariant0Args {
                            // This transmute is very unsafe, in the future stucco will generate
                            // the right type for immediates so that these transmute's aren't
                            // needed.
                            read: unsafe { std::mem::transmute(gets as fn() -> u64) },
                        },
                    )
                    .1
                    .next;
            }
            b'.' => {
                reader.next();
                next = builder
                    .then_variant::<bf::PutVariant0>(
                        next,
                        bf::PutVariant0Args {
                            // This transmute is very unsafe, in the future stucco will generate
                            // the right type for immediates so that these transmute's aren't
                            // needed.
                            write: unsafe { std::mem::transmute(put as fn(u64)) },
                        },
                    )
                    .1
                    .next;
            }
            b'[' => {
                reader.next();
                let (tgt, jumps) =
                    builder.then_variant::<bf::JumpForwardVariant0>(next, Default::default());
                next = jumps.next;
                jump_stack.push((tgt, jumps.forward));
            }
            b']' => {
                reader.next();
                let (tgt, jumps) =
                    builder.then_variant::<bf::JumpBackwardVariant0>(next, Default::default());
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

    // Add the halt variant at the end so that all continuaitions are completed.
    builder.then_variant::<bf::HaltVariant0>(next, bf::HaltVariant0Args {});

    // build the function and map it into executable memory.
    let bytes = builder.build();
    let func = bytes.into_mapped_function();

    // setup a stack and run the function.
    let mut stack = vec![0u64; 1024 * 1024];
    let before_run = Instant::now();
    func.call((stack.as_mut_ptr(),));

    // Flush so that all the output makes it to stdout.
    let _ = std::io::stdout().flush();

    let full = before_compile.elapsed().as_secs_f32();
    let run = before_run.elapsed().as_secs_f32();

    println!("time taken: full {full}, run {run}",)
}
