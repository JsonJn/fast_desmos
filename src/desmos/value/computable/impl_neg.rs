use crate::desmos::value::{CompList, CompPrim, Computable};

use std::ops::Neg;

impl Neg for CompPrim {
    type Output = Self;

    fn neg(self) -> Self::Output {
        match self {
            Self::Number(x) => Self::Number(-x),
            Self::Point(p) => Self::Point(-p),
        }
    }
}

impl Neg for CompList {
    type Output = Self;

    fn neg(self) -> Self::Output {
        match self {
            Self::Number(ns) => Self::Number(ns.map_same(Neg::neg)),
            Self::Point(ps) => Self::Point(ps.map_same(Neg::neg)),
        }
    }
}

impl Neg for Computable {
    type Output = Self;

    fn neg(self) -> Self::Output {
        match self {
            Self::Prim(x) => Self::Prim(-x),
            Self::List(l) => Self::List(-l),
        }
    }
}
