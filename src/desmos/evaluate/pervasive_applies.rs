use crate::desmos::evaluate::value::{CompPrim, Computable, Primitive, VarValue};
use crate::desmos::evaluate::{POOL_BOOL, POOL_PRIMITIVE, POOL_VAR_VALUE};
use crate::pooled_vec::{Id, PooledVec};
use crate::take_pat;


use std::mem::MaybeUninit;

pub(super) fn pervasive_apply_non_prim_known<const ADIC: usize>(
    id: Id,
    params: [VarValue; ADIC],
    mut func: impl FnMut([Primitive; ADIC]) -> VarValue,
) -> VarValue {
    if params.iter().all(|v| matches!(v, VarValue::Prim(_))) {
        let values = params.map(|v| take_pat!(v => x from VarValue::Prim(x)));
        func(values)
    } else {
        let length = params
            .iter()
            .map(|v| v.len().unwrap_or(usize::MAX))
            .min()
            .unwrap();

        let vs: [VarValue; ADIC] = params.try_into().unwrap();
        let a = PooledVec::from_iter(
            &POOL_VAR_VALUE,
            id,
            (0..length).map(|i| func(collect_array(vs.iter().map(|v| v.get_cloned(i))))),
        );

        VarValue::List(a.into())
    }
}

pub(super) fn pervasive_apply_known<const ADIC: usize>(
    id: Id,
    params: [VarValue; ADIC],
    mut func: impl FnMut([Primitive; ADIC]) -> Primitive,
) -> VarValue {
    if params.iter().all(|v| matches!(v, VarValue::Prim(_))) {
        let values = params.map(|v| take_pat!(v => x from VarValue::Prim(x)));
        VarValue::Prim(func(values))
    } else {
        let length = params
            .iter()
            .map(|v| v.len().unwrap_or(usize::MAX))
            .min()
            .unwrap();

        let a = PooledVec::from_iter(
            &POOL_PRIMITIVE,
            id,
            (0..length).map(|i| func(collect_array(params.iter().map(|v| v.get_cloned(i))))),
        );

        VarValue::List(a.into())
    }
}

fn collect_array<T, const ADIC: usize>(iter: impl IntoIterator<Item = T>) -> [T; ADIC] {
    let mut result = [(); ADIC].map(|_| MaybeUninit::uninit());

    let mut iter = iter.into_iter();

    for i in 0..ADIC {
        result[i] = MaybeUninit::new(
            iter.next()
                .unwrap_or_else(|| unreachable!("Not enough elements in the iterator!")),
        );
    }

    result.map(|i| unsafe { i.assume_init() })
}

pub(super) fn pervasive_apply_comp_known<const ADIC: usize>(
    id: Id,
    params: [Computable; ADIC],
    mut func: impl FnMut([CompPrim; ADIC]) -> Primitive,
) -> VarValue {
    if params.iter().all(|v| matches!(v, Computable::Prim(_))) {
        let values = params.map(|v| take_pat!(v => x from Computable::Prim(x)));
        VarValue::Prim(func(values))
    } else {
        let length = params
            .iter()
            .map(|v| v.len().unwrap_or(usize::MAX))
            .min()
            .unwrap();

        let a = PooledVec::from_iter(
            &POOL_PRIMITIVE,
            id,
            (0..length).map(|i| func(collect_array(params.iter().map(|v| v.get(i))))),
        );

        VarValue::List(a.into())
    }
}

pub(super) fn pervasive_apply_variadic(
    id: Id,
    params: Vec<VarValue>,
    mut func: impl FnMut(&mut dyn Iterator<Item = Primitive>) -> Primitive,
) -> VarValue {
    if params.iter().all(|v| matches!(v, VarValue::Prim(_))) {
        let values = &mut params
            .into_iter()
            .map(|v| take_pat!(v => x from VarValue::Prim(x)));

        VarValue::Prim(func(values))
    } else {
        let length = params
            .iter()
            .map(|v| v.len().unwrap_or(usize::MAX))
            .min()
            .unwrap();

        let a = PooledVec::from_iter(
            &POOL_PRIMITIVE,
            id,
            (0..length).map(|i| func(&mut params.iter().map(|v| v.get_cloned(i)))),
        );

        VarValue::List(a.into())
    }
}

pub(super) fn pervasive_apply_comp_variadic_bool(
    id: Id,
    params: Vec<Computable>,
    mut func: impl FnMut(&mut dyn Iterator<Item = CompPrim>) -> bool,
) -> Result<bool, PooledVec<bool>> {
    if params.iter().all(|v| matches!(v, Computable::Prim(_))) {
        let values = &mut params
            .into_iter()
            .map(|v| take_pat!(v => x from Computable::Prim(x)));
        let v = func(values);

        Ok(v)
    } else {
        let length = params
            .iter()
            .map(|v| v.len().unwrap_or(usize::MAX))
            .min()
            .unwrap();

        let a = PooledVec::from_iter(
            &POOL_BOOL,
            id,
            (0..length).map(|i| func(&mut params.iter().map(|v| v.get(i)))),
        );

        Err(a)
    }
}
