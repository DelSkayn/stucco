use inkwell::values::{
    AnyValue, AnyValueEnum, BasicMetadataValueEnum, BasicValueEnum, FloatValue, FunctionValue,
    IntValue, PointerValue,
};

#[derive(Clone, Debug)]
pub struct Value<'ctx> {
    llvm: Option<AnyValueEnum<'ctx>>,
}

impl<'ctx> Value<'ctx> {
    pub fn nill() -> Self {
        Value { llvm: None }
    }

    pub fn is_nill(&self) -> bool {
        self.llvm.is_none()
    }

    pub fn into_int(self) -> IntValue<'ctx> {
        match self.llvm {
            Some(AnyValueEnum::IntValue(x)) => x,
            x => panic!("expected int value, found: {x:?}"),
        }
    }

    pub fn into_function(self) -> FunctionValue<'ctx> {
        match self.llvm {
            Some(AnyValueEnum::FunctionValue(x)) => x,
            x => panic!("expected function value, found: {x:?}"),
        }
    }

    pub fn into_float(self) -> FloatValue<'ctx> {
        match self.llvm {
            Some(AnyValueEnum::FloatValue(x)) => x,
            x => panic!("expected float value, found: {x:?}"),
        }
    }

    pub fn into_ptr(self) -> PointerValue<'ctx> {
        match self.llvm {
            Some(AnyValueEnum::PointerValue(x)) => x,
            x => panic!("expected pointer value, found: {x:?}"),
        }
    }

    pub fn into_basic(self) -> Option<BasicValueEnum<'ctx>> {
        match self.llvm? {
            AnyValueEnum::ArrayValue(array_value) => Some(BasicValueEnum::ArrayValue(array_value)),
            AnyValueEnum::IntValue(int_value) => Some(BasicValueEnum::IntValue(int_value)),
            AnyValueEnum::FloatValue(float_value) => Some(BasicValueEnum::FloatValue(float_value)),
            AnyValueEnum::PhiValue(_) => None,
            AnyValueEnum::FunctionValue(_) => None,
            AnyValueEnum::PointerValue(pointer_value) => {
                Some(BasicValueEnum::PointerValue(pointer_value))
            }
            AnyValueEnum::StructValue(struct_value) => {
                Some(BasicValueEnum::StructValue(struct_value))
            }
            AnyValueEnum::VectorValue(vector_value) => {
                Some(BasicValueEnum::VectorValue(vector_value))
            }
            AnyValueEnum::ScalableVectorValue(scalable_vector_value) => {
                Some(BasicValueEnum::ScalableVectorValue(scalable_vector_value))
            }
            AnyValueEnum::InstructionValue(_) => None,
            AnyValueEnum::MetadataValue(_) => None,
        }
    }

    pub fn into_basic_meta(self) -> Option<BasicMetadataValueEnum<'ctx>> {
        match self.llvm? {
            AnyValueEnum::ArrayValue(array_value) => {
                Some(BasicMetadataValueEnum::ArrayValue(array_value))
            }
            AnyValueEnum::IntValue(int_value) => Some(BasicMetadataValueEnum::IntValue(int_value)),
            AnyValueEnum::FloatValue(float_value) => {
                Some(BasicMetadataValueEnum::FloatValue(float_value))
            }
            AnyValueEnum::PhiValue(_) => None,
            AnyValueEnum::FunctionValue(_) => None,
            AnyValueEnum::PointerValue(pointer_value) => {
                Some(BasicMetadataValueEnum::PointerValue(pointer_value))
            }
            AnyValueEnum::StructValue(struct_value) => {
                Some(BasicMetadataValueEnum::StructValue(struct_value))
            }
            AnyValueEnum::VectorValue(vector_value) => {
                Some(BasicMetadataValueEnum::VectorValue(vector_value))
            }
            AnyValueEnum::ScalableVectorValue(scalable_vector_value) => Some(
                BasicMetadataValueEnum::ScalableVectorValue(scalable_vector_value),
            ),
            AnyValueEnum::InstructionValue(_) => None,
            AnyValueEnum::MetadataValue(m) => Some(BasicMetadataValueEnum::MetadataValue(m)),
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
