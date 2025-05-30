use inkwell::values::{AnyValue, AnyValueEnum, FloatValue, IntValue};

#[derive(Clone)]
pub struct Value<'ctx> {
    llvm: Option<AnyValueEnum<'ctx>>,
}

impl<'ctx> Value<'ctx> {
    pub fn void() -> Self {
        Value { llvm: None }
    }

    pub fn into_int(self) -> IntValue<'ctx> {
        match self.llvm {
            Some(AnyValueEnum::IntValue(x)) => x,
            x => panic!("expected int value, found: {x:?}"),
        }
    }

    pub fn into_float(self) -> FloatValue<'ctx> {
        match self.llvm {
            Some(AnyValueEnum::FloatValue(x)) => x,
            x => panic!("expected float value, found: {x:?}"),
        }
    }
}

impl<'ctx, V> From<V> for Value<'ctx>
where
    V: AnyValue<'ctx>,
{
    fn from(value: V) -> Self {
        Value {
            llvm: Some(value.as_any_value_enum()),
        }
    }
}
