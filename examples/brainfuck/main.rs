stucco::module!(mod brainfuck {
    stencil next(stack: *mut usize, count: usize)
        variant
            imm count,
            slot stack,
    {
        let stack = stack.add(COUNT);
        become next(stack);
    }

    stencil previous(stack: *mut usize, count: usize)
        variant
            imm count,
            slot stack,
    {
        let stack = stack.sub(COUNT);
        become next(stack);
    }

    stencil increment(stack: *mut usize, by: usize)
        variant
            imm by,
            slot stack,
    {
        stack.write(stack.read() + by);
        become next(stack)
    }

    stencil decrement(stack: *mut usize, by: usize)
        variant
            imm by,
            slot stack,
    {
        stack.write(stack.read() - by);
        become next(stack)
    }

    stencil put(stack: *mut usize, write: fn(usize))
        variant
        imm write,
        slot stack,
    {
        write(stack.read());
        become next(stack)
    }


    stencil gets(stack: *mut usize, read: fn() -> usize)
        variant
        imm read,
        slot stack,
    {
        stack.write(read());
        become next(stack)
    }

    stencil jump_forward(stack: *mut usize)
        variant
        slot stack
    {
        if stack.read() == 0 {
            become forward(stack)
        }
        become next(stack)
    }


    stencil jump_backward(stack: *mut usize)
        variant
        slot stack
        {
            if stack.read() != 0 {
                become backward(stack)
            }
            become next(stack)
        }

    stencil halt(stack: *mut usize)
        variant
        slot stack
        {
            return;
        }

});

fn main() {}
