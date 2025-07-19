use std::{
    env,
    io::{Read as _, Write},
    time::Instant,
};

pub enum Instr {
    JumpF(u32),
    JumpB(u32),
    Inc(u32),
    Dec(u32),
    Shl(u32),
    Shr(u32),
    Put,
    Gets,
    Ret,
}

fn main() {
    let src = if let Some(arg) = env::args().skip(1).next() {
        std::fs::read_to_string(arg).unwrap()
    } else {
        let mut buf = String::new();
        std::io::stdin().read_to_string(&mut buf).unwrap();
        buf
    };

    let before_compile = Instant::now();
    let mut instructions = Vec::new();
    let mut pending = Vec::new();
    for b in src.as_bytes() {
        match b {
            b'>' => {
                if let Some(Instr::Shl(l)) = instructions.last_mut() {
                    *l += 1
                } else {
                    instructions.push(Instr::Shl(1))
                }
            }
            b'<' => {
                if let Some(Instr::Shr(l)) = instructions.last_mut() {
                    *l += 1
                } else {
                    instructions.push(Instr::Shr(1))
                }
            }
            b'+' => {
                if let Some(Instr::Inc(l)) = instructions.last_mut() {
                    *l += 1
                } else {
                    instructions.push(Instr::Inc(1))
                }
            }
            b'-' => {
                if let Some(Instr::Dec(l)) = instructions.last_mut() {
                    *l += 1
                } else {
                    instructions.push(Instr::Dec(1))
                }
            }
            b'.' => instructions.push(Instr::Put),
            b',' => instructions.push(Instr::Gets),
            b'[' => {
                pending.push(instructions.len());
                instructions.push(Instr::JumpF(0));
            }
            b']' => {
                let tgt = pending.pop().expect("invalid brainfuck");
                let offset = instructions.len() - tgt;
                instructions.push(Instr::JumpB(offset as u32));
                instructions[tgt] = Instr::JumpF(offset as u32);
            }
            _ => {}
        }
    }
    instructions.push(Instr::Ret);
    let mut buffer = vec![0; 1024 * 1024 * 256];
    let mut write_buffer = vec![0; 1024 * 1024 * 256];
    let mut write_ptr = 0;
    let before_run = Instant::now();
    run(
        &mut buffer,
        &instructions,
        &mut write_ptr,
        &mut write_buffer,
    );

    let full = before_compile.elapsed().as_secs_f32();
    let run = before_run.elapsed().as_secs_f32();

    std::io::stdout()
        .write_all(&write_buffer[..write_ptr])
        .unwrap();

    println!("time taken: full {full}, run {run}",)
}

fn run(buffer: &mut [u64], instr: &[Instr], write_ptr: &mut usize, write_buffer: &mut [u8]) {
    let mut ptr = 0;
    let mut cur = 0;

    fn get(buffer: &mut [u64], addr: u32) -> u64 {
        // unsafe buffer access to be more fair to the stucco one for now.
        *unsafe { buffer.get_unchecked(addr as usize) }
    }
    fn set(buffer: &mut [u64], addr: u32, v: u64) {
        // unsafe buffer access to be more fair to the stucco one for now.
        *unsafe { buffer.get_unchecked_mut(addr as usize) } = v;
    }

    loop {
        match instr[cur as usize] {
            Instr::JumpF(off) => {
                if get(buffer, ptr) == 0 {
                    cur += off
                } else {
                    cur += 1;
                }
            }
            Instr::JumpB(off) => {
                if get(buffer, ptr) != 0 {
                    cur -= off
                } else {
                    cur += 1;
                }
            }
            Instr::Inc(inc) => {
                let res = get(buffer, ptr).wrapping_add(inc as u64);
                set(buffer, ptr, res);
                cur += 1;
            }
            Instr::Dec(inc) => {
                let res = get(buffer, ptr).wrapping_sub(inc as u64);
                set(buffer, ptr, res);
                cur += 1;
            }
            Instr::Shl(off) => {
                ptr += off;
                cur += 1;
            }
            Instr::Shr(off) => {
                ptr -= off;
                cur += 1;
            }
            Instr::Put => {
                // buffering io so that we get more consistent times.
                write_buffer[*write_ptr] = get(buffer, ptr) as u8;
                *write_ptr += 1;
                cur += 1;
            }
            Instr::Gets => {
                std::io::stdout()
                    .write_all(&write_buffer[..(*write_ptr)])
                    .unwrap();
                *write_ptr = 0;
                let mut buf = [0u8];
                std::io::stdin().read_exact(&mut buf).unwrap();
                set(buffer, ptr, buf[0] as u64);
                cur += 1;
            }
            Instr::Ret => break,
        }
    }
}
