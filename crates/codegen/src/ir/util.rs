use compiler::infer::{PrimTy, Ty, TyId};
use inkwell::{
    AddressSpace,
    types::{BasicType as _, BasicTypeEnum, FunctionType},
};

use super::VariantGen;

impl<'ctx> VariantGen<'ctx> {
    pub(super) fn tyid_to_llvm_type(&self, id: TyId) -> Option<BasicTypeEnum<'ctx>> {
        let ty = self.ctx.types.find_type(id);
        let r = match &self.ctx.types.type_graph[ty] {
            Ty::Prim(p) => match p {
                PrimTy::Nil => return None,
                PrimTy::Bool => self.ctx.context.bool_type().as_basic_type_enum(),
                // TODO: Change when supporting 32 bit.
                PrimTy::Usize => self.ctx.context.i64_type().as_basic_type_enum(),
                PrimTy::Isize => self.ctx.context.i64_type().as_basic_type_enum(),
                PrimTy::U64 => self.ctx.context.i64_type().as_basic_type_enum(),
                PrimTy::I64 => self.ctx.context.i64_type().as_basic_type_enum(),
                PrimTy::U32 => self.ctx.context.i32_type().as_basic_type_enum(),
                PrimTy::I32 => self.ctx.context.i32_type().as_basic_type_enum(),
                PrimTy::U16 => self.ctx.context.i16_type().as_basic_type_enum(),
                PrimTy::I16 => self.ctx.context.i16_type().as_basic_type_enum(),
                PrimTy::U8 => self.ctx.context.i8_type().as_basic_type_enum(),
                PrimTy::I8 => self.ctx.context.i8_type().as_basic_type_enum(),
                PrimTy::F32 => self.ctx.context.f32_type().as_basic_type_enum(),
                PrimTy::F64 => self.ctx.context.f64_type().as_basic_type_enum(),
                PrimTy::Diverges => return None,
            },
            Ty::Ptr(_) | Ty::PtrMut(_) => self
                .ctx
                .context
                .ptr_type(AddressSpace::default())
                .as_basic_type_enum(),
            Ty::Ref(_) => todo!(),
            Ty::RefMut(_) => todo!(),
            Ty::Tuple(_) => todo!(),
            Ty::Fn(..) => return None,
            Ty::Array(_, _) => todo!(),
            Ty::Var(..) => panic!("unresolved type"),
        };
        Some(r)
    }

    pub(super) fn tyid_to_llvm_func_type(&self, id: TyId) -> Option<FunctionType<'ctx>> {
        let ty = self.ctx.types.find_type(id);
        let Ty::Fn(ref args, ref res) = self.ctx.types.type_graph[ty] else {
            return None;
        };

        let mut llvm_args = Vec::new();
        for a in args.iter() {
            llvm_args.push(self.tyid_to_llvm_type(*a).unwrap().into())
        }

        if let Ty::Prim(PrimTy::Nil) = self.ctx.types.type_graph[*res] {
            Some(self.ctx.context.void_type().fn_type(&llvm_args, false))
        } else {
            let res = self.tyid_to_llvm_type(*res).unwrap();

            Some(res.fn_type(&llvm_args, false))
        }
    }
}
