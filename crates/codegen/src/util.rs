use inkwell::types::{AnyTypeEnum, BasicType, BasicTypeEnum, FunctionType, VoidType};
use object::pe::ANON_OBJECT_HEADER_BIGOBJ_CLASS_ID;

pub enum NonBasicTypeEnum<'ctx> {
    VoidType(VoidType<'ctx>),
    FunctionType(FunctionType<'ctx>),
}

pub fn try_any_to_basic(ty: AnyTypeEnum) -> Result<BasicTypeEnum, NonBasicTypeEnum> {
    match ty {
        AnyTypeEnum::ArrayType(x) => Ok(x.as_basic_type_enum()),
        AnyTypeEnum::FloatType(x) => Ok(x.as_basic_type_enum()),
        AnyTypeEnum::IntType(x) => Ok(x.as_basic_type_enum()),
        AnyTypeEnum::PointerType(x) => Ok(x.as_basic_type_enum()),
        AnyTypeEnum::StructType(x) => Ok(x.as_basic_type_enum()),
        AnyTypeEnum::VectorType(x) => Ok(x.as_basic_type_enum()),
        AnyTypeEnum::ScalableVectorType(x) => Ok(x.as_basic_type_enum()),
        AnyTypeEnum::FunctionType(function_type) => {
            Err(NonBasicTypeEnum::FunctionType(function_type))
        }
        AnyTypeEnum::VoidType(void_type) => Err(NonBasicTypeEnum::VoidType(void_type)),
    }
}
