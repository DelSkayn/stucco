use std::{
    error::Error as StdError,
    fmt,
    io::{Read, Write},
};

use stucco::JitFunction;

#[stucco::template(entry)]
fn enter(stack: *mut u64) {
    next!(next(stack))
}

#[stucco::template]
fn next(stack: *mut u64) {
    let stack = stack + 1;
    next!(next(stack))
}

#[stucco::template]
fn previous(stack: *mut u64) {
    let stack = stack - 1;
    next!(next(stack))
}

#[stucco::template]
fn increment(stack: *mut u64) {
    (*stack) += 1;
    next!(next(stack))
}

#[stucco::template]
fn decrement(stack: *mut u64) {
    (*stack) -= 1;
    next!(next(stack))
}

#[stucco::template]
fn next_constant<const C: u64>(stack: *mut u64) {
    let stack = stack + C;
    next!(next(stack))
}

#[stucco::template]
fn previous_constant<const C: u64>(stack: *mut u64) {
    let stack = stack - C;
    next!(next(stack))
}

#[stucco::template]
fn put<const F: fn(u64)>(stack: *mut u64) {
    F(*stack);
    next!(next(stack))
}

#[stucco::template]
fn gets<const F: fn() -> u64>(stack: *mut u64) {
    (*stack) = F();
    next!(next(stack))
}

#[stucco::template]
fn add_constant<const F: u64>(stack: *mut u64) {
    (*stack) += F;
    next!(next(stack))
}

#[stucco::template]
fn sub_constant<const F: u64>(stack: *mut u64) {
    (*stack) -= F;
    next!(next(stack))
}

#[stucco::template]
fn jump_forward(stack: *mut u64) {
    if *stack == 0 {
        next!(forward(stack))
    }
    next!(next(stack))
}

#[stucco::template]
fn jump_backward(stack: *mut u64) {
    if *stack != 0 {
        next!(backward(stack))
    }
    next!(next(stack))
}

#[stucco::template]
fn finish(stack: *mut u64) {
    return;
}

extern "C" fn c_put(v: u64) {
    print!("{}", char::from_u32(v as u32).unwrap_or(' '));
}

extern "C" fn c_get() -> u64 {
    let mut buf = [0u8];
    std::io::stdin().read(&mut buf).ok();
    buf[0] as u64
}
#[derive(Debug)]
enum Error {
    MismatchedJump,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::MismatchedJump => write!(f, "jump missing closing or opening bracket"),
        }
    }
}

impl StdError for Error {}

fn run_brainfuck(source: &str) -> Result<(), Box<dyn StdError>> {
    let mut builder = stucco::Builder::<()>::new();

    let mut prev = builder.start::<enter>(&[]);
    let mut jumps = Vec::new();
    let mut chars = source.chars().peekable();

    while let Some(c) = chars.next() {
        match c {
            '>' => {
                let mut count = 1;
                while let Some('>') = chars.peek() {
                    chars.next();
                    count += 1;
                }

                let n = if count > 1 {
                    builder.push::<next_constant>(&[count])
                } else {
                    builder.push::<next>(&[])
                };
                builder.jump_to("next", prev, n);
                prev = n;
            }
            '<' => {
                let mut count = 1;
                while let Some('<') = chars.peek() {
                    chars.next();
                    count += 1;
                }

                let n = if count > 1 {
                    builder.push::<previous_constant>(&[count])
                } else {
                    builder.push::<previous>(&[])
                };
                builder.jump_to("next", prev, n);
                prev = n;
            }
            '+' => {
                let mut count = 1;
                while let Some('+') = chars.peek() {
                    chars.next();
                    count += 1;
                }

                let n = if count > 1 {
                    builder.push::<add_constant>(&[count])
                } else {
                    builder.push::<increment>(&[])
                };
                builder.jump_to("next", prev, n);
                prev = n;
            }
            '-' => {
                let mut count = 1;
                while let Some('-') = chars.peek() {
                    chars.next();
                    count += 1;
                }

                let n = if count > 1 {
                    builder.push::<sub_constant>(&[count])
                } else {
                    builder.push::<decrement>(&[])
                };
                builder.jump_to("next", prev, n);
                prev = n;
            }
            '.' => {
                let n = builder.push::<put>(&[c_put as *mut () as usize as u64]);
                builder.jump_to("next", prev, n);
                prev = n;
            }
            ',' => {
                let n = builder.push::<gets>(&[c_get as *mut () as usize as u64]);
                builder.jump_to("next", prev, n);
                prev = n;
            }
            '[' => {
                let n = builder.push::<jump_forward>(&[]);
                builder.jump_to("next", prev, n);
                prev = n;
                jumps.push((n, builder.next_part()));
            }
            ']' => {
                let close = builder.push::<jump_backward>(&[]);
                builder.jump_to("next", prev, close);
                prev = close;

                let (open, after_open) = jumps.pop().ok_or(Error::MismatchedJump)?;

                let after_close = builder.next_part();
                builder.jump_to("forward", open, after_close);
                builder.jump_to("backward", close, after_open);
            }
            _ => {}
        }
    }

    if !jumps.is_empty() {
        return Err(Error::MismatchedJump.into());
    }

    let n = builder.push::<finish>(&[]);
    builder.jump_to("next", prev, n);

    let code = builder.finish();

    let func_ptr = unsafe { JitFunction::<()>::from_instructions(&code).unwrap() };

    let mut stack = vec![0u64; 1024 * 1024 * 1024];

    let func: unsafe extern "C" fn(*mut u64) = unsafe { std::mem::transmute(func_ptr.ptr()) };
    let stack_ptr = stack.as_mut_ptr();

    println!();
    println!("== JUMPING TO JIT FUNCTION ==");
    println!();

    unsafe { (func)(stack_ptr) };

    println!();
    println!("== DONE ==");
    println!();

    Ok(())
}

fn main() -> Result<(), Box<dyn StdError>> {
    let text = if let Some(x) = std::env::args().skip(1).next() {
        std::fs::read_to_string(x)?
    } else {
        std::io::read_to_string(std::io::stdin())?
    };
    run_brainfuck(&text)
}

#[cfg(test)]
mod test {
    use super::run_brainfuck;

    #[test]
    fn run_mandelbrot() {
        let src = include_str!("../../test/mandelbrot.bf");
        run_brainfuck(src).unwrap()
    }
}
