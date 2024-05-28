use super::list::CompList;
use super::primitive::CompPrim;
use crate::desmos::evaluate::value::{PrimList, Primitive, VarValue};

mod impl_add;
mod impl_div;
mod impl_eq;
mod impl_mul;
mod impl_neg;
mod impl_pow;

pub use impl_pow::Pow;

#[derive(Debug, Clone)]
pub enum Computable {
    Prim(CompPrim),
    List(CompList),
}

impl Computable {
    pub fn get(&self, index: usize) -> CompPrim {
        match self {
            &Computable::Prim(x) => x,
            Computable::List(l) => l.get(index),
        }
    }

    pub const fn number(n: f64) -> Self {
        Self::Prim(CompPrim::Number(n))
    }

    pub fn len(&self) -> Option<usize> {
        match self {
            Computable::Prim(_) => None,
            Computable::List(l) => Some(l.len()),
        }
    }
}

impl From<Computable> for VarValue {
    fn from(value: Computable) -> Self {
        match value {
            Computable::Prim(p) => Self::Prim(Primitive::Computable(p)),
            Computable::List(l) => Self::List(PrimList::Computable(l)),
        }
    }
}
