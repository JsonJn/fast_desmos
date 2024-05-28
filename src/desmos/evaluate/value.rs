pub use builtins::*;
pub use computable::*;
pub use list::*;
pub use listables::*;
pub use primitive::*;

use crate::desmos::evaluate::{
    Builtins, EvalTree, Ident, TriadicPervasive, UserIdent, VariadicPervasive,
};
use crate::pooled_vec::PooledVec;

use super::tree::EvalExpr;

mod builtins;
mod computable;
mod list;
mod listables;
mod noncomputable;
mod primitive;

#[macro_export]
macro_rules! take_pat {
    ($v:expr => $id:ident from $pa:pat $(,$err:literal)?) => {
        if let $pa = $v {
            $id
        } else {
            unreachable!($($err)?)
        }
    };
}

#[derive(Debug)]
pub enum IdentValue {
    Val(VarValue),
    Function(Function),
}

impl TryFrom<IdentValue> for VarValue {
    type Error = Function;

    fn try_from(value: IdentValue) -> Result<Self, Self::Error> {
        match value {
            IdentValue::Val(v) => Ok(v),
            IdentValue::Function(f) => Err(f),
        }
    }
}

impl TryFrom<IdentValue> for Function {
    type Error = VarValue;

    fn try_from(value: IdentValue) -> Result<Self, Self::Error> {
        match value {
            IdentValue::Val(v) => Err(v),
            IdentValue::Function(f) => Ok(f),
        }
    }
}

#[derive(Debug)]
pub struct Function {
    pub params: Vec<UserIdent>,
    pub expr: EvalExpr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum VarValue {
    Prim(Primitive),
    List(PrimList),
}

impl Default for VarValue {
    fn default() -> Self {
        Self::Prim(Primitive::default())
    }
}

impl VarValue {
    pub fn get_cloned(&self, index: usize) -> Primitive {
        match self {
            VarValue::Prim(s) => s.clone(),
            VarValue::List(v) => v.get_cloned(index),
        }
    }

    pub fn color(c: Color) -> Self {
        Self::Prim(Primitive::NonComputable(NonCompPrim::Color(c)))
    }

    pub fn polygon(c: Polygon) -> Self {
        Self::Prim(Primitive::NonComputable(NonCompPrim::Polygon(c)))
    }

    pub fn len(&self) -> Option<usize> {
        match self {
            VarValue::Prim(_) => None,
            VarValue::List(k) => Some(k.len()),
        }
    }

    pub const fn number(n: f64) -> Self {
        Self::Prim(Primitive::Computable(CompPrim::Number(n)))
    }
    pub const fn point(p: Point) -> Self {
        Self::Prim(Primitive::Computable(CompPrim::Point(p)))
    }

    pub const fn num_list(ns: PooledVec<f64>) -> Self {
        Self::List(PrimList::Computable(CompList::Number(ns)))
    }
}

impl TryFrom<VarValue> for Computable {
    type Error = ();

    fn try_from(value: VarValue) -> Result<Self, Self::Error> {
        match value {
            VarValue::Prim(p) => p.try_into().map(Computable::Prim),
            VarValue::List(l) => l.try_into().map(Computable::List),
        }
    }
}
