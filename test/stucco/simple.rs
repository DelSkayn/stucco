mod foo {
    #[entry]
    fn enter(stack: *mut u64) {
        tail!(next(stack))
    }
}
