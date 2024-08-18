mod foo {
    #[entry]
    fn enter(stack: *mut u64) {
        tail!(next(stack))
    }

    fn next(stack: *mut u64) {
        let stack = stack + 1;
        tail!(next(stack))
    }

    fn previous(stack: *mut u64) {
        let stack = stack - 1;
        tail!(next(stack))
    }

    fn increment(stack: *mut u64) {
        (*stack) += 1;
        tail!(next(stack))
    }

    fn decrement(stack: *mut u64) {
        (*stack) -= 1;
        tail!(next(stack))
    }

    fn next_constant<const C: u64>(stack: *mut u64) {
        let stack = stack + C;
        tail!(next(stack))
    }

    /*
    fn previous_constant<const C: u64>(stack: *mut u64) {
        let stack = stack - C;
        tail!(next(stack))
    }

    fn put<const F: fn(u64)>(stack: *mut u64) {
        F(*stack);
        tail!(next(stack))
    }

    fn gets<const F: fn() -> u64>(stack: *mut u64) {
        (*stack) = F();
        tail!(next(stack))
    }

    fn add_constant<const F: u64>(stack: *mut u64) {
        (*stack) += F;
        tail!(next(stack))
    }

    fn sub_constant<const F: u64>(stack: *mut u64) {
        (*stack) -= F;
        tail!(next(stack))
    }

    fn jump_forward(stack: *mut u64) {
        if *stack == 0 {
            tail!(forward(stack))
        }
        tail!(next(stack))
    }

    fn jump_backward(stack: *mut u64) {
        if *stack != 0 {
            tail!(backward(stack))
        }
        tail!(next(stack))
    }

    fn finish(stack: *mut u64) {
        return;
    }
    */
}
