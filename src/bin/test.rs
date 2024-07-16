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
fn jump_forward(stack: *mut u64) {
    if *stack == 0 {
        jump!('forward)
    }
    next!(next(stack))
}

#[stucco::template]
fn jump_backward(stack: *mut u64) {
    if *stack != 0 {
        jump!('backward)
    }
    next!(next(stack))
}

fn main() {}
