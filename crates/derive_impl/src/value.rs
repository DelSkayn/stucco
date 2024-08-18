use crate::{Result, StencilModule};
use inkwell::{
    llvm_sys::LLVMCallConv,
    types::{BasicMetadataTypeEnum, BasicType as _, BasicTypeEnum},
    values::{AnyValue, AnyValueEnum, BasicMetadataValueEnum, BasicValueEnum, PointerValue},
    AddressSpace, IntPredicate,
};
use proc_macro2::Span;
use std::fmt;
use syn::Error;

#[derive(Eq, PartialEq, Clone, Copy, Debug)]
pub enum Signing {
    Signed,
    Unsigned,
}

#[derive(Eq, PartialEq, Clone, Copy, Debug)]
pub enum Mutability {
    Mutable,
    Constant,
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub enum Ty {
    Bool,
    B64(Signing),
    B32(Signing),
    B16(Signing),
    B8(Signing),
    Char,
    Ptr(Mutability, Box<Ty>),
    Fn(Vec<Ty>, Box<Ty>),
    Void,
}

pub enum Symbol<'ctx> {
    Local(Value<'ctx>),
    Constant {
        ty: Ty,
        addr: PointerValue<'ctx>,
        span: Span,
    },
}

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Ty::B64(x) => {
                if let Signing::Signed = x {
                    write!(f, "i64")
                } else {
                    write!(f, "u64")
                }
            }
            Ty::B32(x) => {
                if let Signing::Signed = x {
                    write!(f, "i32")
                } else {
                    write!(f, "u32")
                }
            }
            Ty::B16(x) => {
                if let Signing::Signed = x {
                    write!(f, "i16")
                } else {
                    write!(f, "u16")
                }
            }
            Ty::B8(x) => {
                if let Signing::Signed = x {
                    write!(f, "i8")
                } else {
                    write!(f, "u8")
                }
            }
            Ty::Char => write!(f, "char"),
            Ty::Bool => write!(f, "bool"),
            Ty::Ptr(m, t) => {
                if let Mutability::Mutable = m {
                    write!(f, "*mut ")?
                } else {
                    write!(f, "*const ")?
                }
                t.fmt(f)
            }
            Ty::Fn(args, res) => {
                write!(f, "fn (")?;
                for (idx, a) in args.iter().enumerate() {
                    if idx != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", a)?;
                }
                write!(f, ")")?;
                if Ty::Void != **res {
                    write!(f, " -> {}", res)?;
                }
                Ok(())
            }
            Ty::Void => write!(f, "void"),
        }
    }
}

impl Ty {
    pub fn is_integer(&self) -> bool {
        matches!(self, Ty::B64(_) | Ty::B32(_) | Ty::B16(_) | Ty::B8(_))
    }

    pub fn is_ptr(&self) -> bool {
        matches!(self, Ty::Ptr(_, _) | Ty::Fn(_, _))
    }

    pub fn byte_width(&self) -> u32 {
        match self {
            Ty::B64(_) => 8,
            Ty::B32(_) => 4,
            Ty::B16(_) => 2,
            Ty::B8(_) => 1,
            Ty::Char => 4,
            Ty::Ptr(_, _) => 8,
            Ty::Fn(_, _) => 8,
            Ty::Bool => 1,
            Ty::Void => 0,
        }
    }

    pub fn to_basic_meta_type<'ctx>(
        &self,
        module: &StencilModule<'ctx>,
    ) -> Option<BasicMetadataTypeEnum<'ctx>> {
        match self {
            Ty::B64(_) => Some(module.context.i64_type().into()),
            Ty::B32(_) => Some(module.context.i32_type().into()),
            Ty::B16(_) => Some(module.context.i32_type().into()),
            Ty::B8(_) => Some(module.context.i8_type().into()),
            Ty::Char => Some(module.context.i32_type().into()),
            Ty::Bool => Some(module.context.bool_type().into()),
            Ty::Ptr(_, _) => Some(module.context.ptr_type(AddressSpace::default()).into()),
            Ty::Fn(_, _) => Some(module.context.ptr_type(AddressSpace::default()).into()),
            Ty::Void => None,
        }
    }

    pub fn to_basic_type<'ctx>(&self, module: &StencilModule<'ctx>) -> Option<BasicTypeEnum<'ctx>> {
        match self {
            Ty::B64(_) => Some(module.context.i64_type().into()),
            Ty::B32(_) => Some(module.context.i32_type().into()),
            Ty::B16(_) => Some(module.context.i32_type().into()),
            Ty::B8(_) => Some(module.context.i8_type().into()),
            Ty::Char => Some(module.context.i32_type().into()),
            Ty::Bool => Some(module.context.bool_type().into()),
            Ty::Ptr(_, _) => Some(module.context.ptr_type(AddressSpace::default()).into()),
            Ty::Fn(_, _) => Some(module.context.ptr_type(AddressSpace::default()).into()),
            Ty::Void => None,
        }
    }
}

#[derive(Clone)]
pub struct Value<'a> {
    span: Span,
    ty: Ty,
    llvm: Option<AnyValueEnum<'a>>,
}

impl<'ctx> Value<'ctx> {
    pub fn new(ty: Ty, llvm: AnyValueEnum<'ctx>, span: Span) -> Self {
        Value {
            ty,
            llvm: Some(llvm),
            span,
        }
    }

    pub fn void(span: Span) -> Self {
        Value {
            ty: Ty::Void,
            span,
            llvm: None,
        }
    }

    pub fn ty(&self) -> &Ty {
        &self.ty
    }

    pub fn span(&self) -> &Span {
        &self.span
    }

    pub fn value(&self) -> Option<&AnyValueEnum<'ctx>> {
        self.llvm.as_ref()
    }

    pub fn from_u64(module: &mut StencilModule<'ctx>, v: u64, span: Span) -> Self {
        let llvm = module.context.i64_type().const_int(v, false);
        Value {
            span,
            ty: Ty::B64(Signing::Unsigned),
            llvm: Some(llvm.into()),
        }
    }

    pub fn to_basic_value(&self) -> Result<BasicValueEnum<'ctx>> {
        if let Some(v) = self.llvm {
            Ok(BasicValueEnum::try_from(v.clone()).unwrap())
        } else {
            return Err(Error::new(self.span, "use of void value"));
        }
    }

    pub fn to_basic_meta_value(&self) -> Result<BasicMetadataValueEnum<'ctx>> {
        if let Some(v) = self.llvm {
            Ok(BasicMetadataValueEnum::try_from(v.clone()).unwrap())
        } else {
            return Err(Error::new(self.span, "use of void value"));
        }
    }

    pub fn to_basic_meta_type(&self) -> Result<BasicMetadataTypeEnum<'ctx>> {
        if let Some(v) = self.llvm {
            Ok(BasicMetadataTypeEnum::try_from(v.get_type()).unwrap())
        } else {
            return Err(Error::new(self.span, "use of void value"));
        }
    }

    pub fn deref(self, span: Span, module: &mut StencilModule<'ctx>) -> Result<Self> {
        let Ty::Ptr(_, ty) = self.ty else {
            return Err(Error::new(
                span,
                &format!("{} cannot be dereferenced", self.ty),
            ));
        };

        let v = self.llvm.unwrap().into_pointer_value();

        let Some(basic) = ty.to_basic_type(module) else {
            return Err(Error::new(
                span,
                &format!("pointer of type {} cannot be dereferenced", ty),
            ));
        };

        let v = module
            .builder
            .build_load(basic, v, &module.next_name("load"))
            .unwrap();

        Ok(Value {
            ty: *ty,
            span,
            llvm: Some(v.into()),
        })
    }

    pub fn store(self, span: Span, value: Self, module: &mut StencilModule<'ctx>) -> Result<()> {
        let Ty::Ptr(_, ty) = self.ty else {
            panic!("called store on non-pointer value")
        };
        if *ty != value.ty {
            return Err(Error::new(
                span,
                &format!(
                    "cannot assign value of type {} to pointer of type {}",
                    value.ty, ty,
                ),
            ));
        }

        let value = value.to_basic_value()?;

        module
            .builder
            .build_store(self.llvm.unwrap().into_pointer_value(), value)
            .unwrap();

        Ok(())
    }

    pub fn call(
        self,
        span: Span,
        args: Vec<Value<'ctx>>,
        module: &StencilModule<'ctx>,
    ) -> Result<Value<'ctx>> {
        let Ty::Fn(ty_args, output) = self.ty else {
            return Err(Error::new(span, "tried to call, non function value"));
        };

        if args.len() != ty_args.len() {
            return Err(Error::new(
                span,
                &format!(
                    "invalid number of arguments, expected {}, found {}",
                    ty_args.len(),
                    args.len()
                ),
            ));
        }

        let mut param = Vec::new();
        for (f, t) in args.iter().zip(ty_args.iter()) {
            if f.ty != *t {
                return Err(Error::new(
                    f.span,
                    &format!("argument type mismatch, expected {}, found {}", t, f.ty),
                ));
            }
            param.push(f.to_basic_meta_type()?);
        }

        let fn_ty = if let Some(ty) = output.to_basic_type(module) {
            ty.fn_type(&param, false)
        } else {
            module.context.void_type().fn_type(&param, false)
        };

        let mut arg_value = Vec::new();
        for f in args.iter() {
            arg_value.push(f.to_basic_meta_value()?)
        }

        let v = module
            .builder
            .build_indirect_call(
                fn_ty,
                self.llvm.unwrap().into_pointer_value(),
                &arg_value,
                &module.next_name("call"),
            )
            .unwrap();
        v.set_call_convention(LLVMCallConv::LLVMCCallConv as u32);

        Ok(Value {
            span,
            llvm: (!matches!(*output, Ty::Void)).then(|| v.as_any_value_enum()),
            ty: *output,
        })
    }

    pub fn cmp(
        self,
        kind: IntPredicate,
        other: Self,
        span: Span,
        module: &mut StencilModule<'ctx>,
    ) -> Result<Self> {
        if self.ty != other.ty {
            return Err(Error::new(
                self.span,
                format!("cannot compare non-equal type {} and {}", self.ty, other.ty),
            ));
        }

        if self.ty.is_integer() {
            let v = module
                .builder
                .build_int_compare(
                    kind,
                    self.llvm.unwrap().into_int_value(),
                    other.llvm.unwrap().into_int_value(),
                    &module.next_name("int_cmp"),
                )
                .unwrap();

            return Ok(Value {
                span,
                ty: Ty::Bool,
                llvm: Some(v.into()),
            });
        }

        todo!()
    }

    pub fn add(self, other: Self, span: Span, module: &mut StencilModule<'ctx>) -> Result<Self> {
        if self.ty != other.ty {
            if let Ty::Ptr(m, x) = self.ty {
                let Ty::B64(_) = other.ty else {
                    return Err(Error::new(
                        other.span,
                        format!("Invalid type {} for pointer arithmatic", other.ty),
                    ));
                };

                let ptr = self.llvm.unwrap().into_pointer_value();
                let ptr_int = module
                    .builder
                    .build_ptr_to_int(
                        ptr,
                        module.context.i64_type(),
                        &module.next_name("ptr_to_int"),
                    )
                    .unwrap();

                let size = module
                    .context
                    .i64_type()
                    .const_int(x.byte_width() as u64, false);
                let by = module
                    .builder
                    .build_int_mul(
                        other.llvm.unwrap().into_int_value(),
                        size,
                        &module.next_name("size_offset"),
                    )
                    .unwrap();

                let v = module
                    .builder
                    .build_int_add(ptr_int, by, &module.next_name("add"))
                    .unwrap();

                let v = module
                    .builder
                    .build_int_to_ptr(
                        v,
                        module.context.ptr_type(Default::default()),
                        &module.next_name("int_to_ptr"),
                    )
                    .unwrap();

                return Ok(Value {
                    span,
                    ty: Ty::Ptr(m, x),
                    llvm: Some(v.into()),
                });
            }
            return Err(Error::new(
                self.span,
                format!("cannot add non-equal type {} and {}", self.ty, other.ty),
            ));
        }

        if !self.ty.is_integer() {
            return Err(Error::new(
                self.span,
                format!("cannot add two {} values", self.ty),
            ));
        }

        let name = module.next_name("add");
        let v = module
            .builder
            .build_int_add(
                self.llvm.unwrap().into_int_value(),
                other.llvm.unwrap().into_int_value(),
                &name,
            )
            .unwrap();

        Ok(Value {
            span,
            ty: self.ty,
            llvm: Some(v.into()),
        })
    }

    pub fn sub(self, other: Self, span: Span, module: &mut StencilModule<'ctx>) -> Result<Self> {
        if self.ty != other.ty {
            if let Ty::Ptr(m, x) = self.ty {
                let Ty::B64(_) = other.ty else {
                    return Err(Error::new(
                        other.span,
                        format!("Invalid type {} for pointer arithmatic", other.ty),
                    ));
                };

                let ptr = self.llvm.unwrap().into_pointer_value();
                let ptr_int = module
                    .builder
                    .build_ptr_to_int(
                        ptr,
                        module.context.i64_type(),
                        &module.next_name("ptr_to_int"),
                    )
                    .unwrap();

                let size = module
                    .context
                    .i64_type()
                    .const_int(x.byte_width() as u64, false);
                let by = module
                    .builder
                    .build_int_mul(
                        other.llvm.unwrap().into_int_value(),
                        size,
                        &module.next_name("size_offset"),
                    )
                    .unwrap();

                let v = module
                    .builder
                    .build_int_sub(ptr_int, by, &module.next_name("sub"))
                    .unwrap();

                let v = module
                    .builder
                    .build_int_to_ptr(
                        v,
                        module.context.ptr_type(Default::default()),
                        &module.next_name("int_to_ptr"),
                    )
                    .unwrap();

                return Ok(Value {
                    span,
                    ty: Ty::Ptr(m, x),
                    llvm: Some(v.into()),
                });
            }
            return Err(Error::new(
                self.span,
                format!(
                    "cannot subtract non-equal type {} and {}",
                    self.ty, other.ty
                ),
            ));
        }

        if !self.ty.is_integer() {
            return Err(Error::new(
                self.span,
                format!("cannot subtract two {} values", self.ty),
            ));
        }

        let name = module.next_name("add");
        let v = module
            .builder
            .build_int_sub(
                self.llvm.unwrap().into_int_value(),
                other.llvm.unwrap().into_int_value(),
                &name,
            )
            .unwrap();

        Ok(Value {
            span,
            ty: self.ty,
            llvm: Some(v.into()),
        })
    }
}
