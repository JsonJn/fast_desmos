use crate::desmos::evaluate::{
    Color, CompList, CompPrim, NonCompList, NonCompPrim, PrimList, Primitive, VarValue,
};
use crate::iterator::{OptIntoIterRep, OptIterRep};
use crate::pooled_vec::PooledVec;

#[derive(Debug, Clone)]
pub enum Numbers {
    One(f64),
    Many(PooledVec<f64>),
}

impl TryFrom<VarValue> for Numbers {
    type Error = VarValue;

    fn try_from(value: VarValue) -> Result<Self, Self::Error> {
        match value {
            VarValue::Prim(Primitive::Computable(CompPrim::Number(n))) => Ok(Self::One(n)),
            VarValue::List(PrimList::Computable(CompList::Number(ns))) => Ok(Self::Many(ns)),
            a => Err(a),
        }
    }
}

impl IntoIterator for Numbers {
    type Item = f64;
    type IntoIter = OptIntoIterRep<f64>;

    fn into_iter(self) -> Self::IntoIter {
        match self {
            Self::One(x) => OptIntoIterRep::One(x),
            Self::Many(xs) => OptIntoIterRep::Many { vec: xs, index: 0 },
        }
    }
}

impl Numbers {
    pub fn iter(&self) -> OptIterRep<f64> {
        match self {
            Self::One(x) => OptIterRep::One(x),
            Self::Many(xs) => OptIterRep::Many(xs.iter()),
        }
    }

    pub fn first(self) -> f64 {
        match self {
            Numbers::One(x) => x,
            Numbers::Many(v) => v[0],
        }
    }
}

#[derive(Debug, Clone)]
pub enum Colors {
    One(Color),
    Many(PooledVec<Color>),
}

impl TryFrom<VarValue> for Colors {
    type Error = VarValue;

    fn try_from(value: VarValue) -> Result<Self, Self::Error> {
        match value {
            VarValue::Prim(Primitive::NonComputable(NonCompPrim::Color(n))) => Ok(Self::One(n)),
            VarValue::List(PrimList::NonComputable(NonCompList::Color(ns))) => Ok(Self::Many(ns)),
            a => Err(a),
        }
    }
}

impl IntoIterator for Colors {
    type Item = Color;
    type IntoIter = OptIntoIterRep<Color>;

    fn into_iter(self) -> Self::IntoIter {
        match self {
            Self::One(x) => OptIntoIterRep::One(x),
            Self::Many(xs) => OptIntoIterRep::Many { vec: xs, index: 0 },
        }
    }
}

impl Colors {
    pub fn iter(&self) -> OptIterRep<Color> {
        match self {
            Self::One(x) => OptIterRep::One(x),
            Self::Many(xs) => OptIterRep::Many(xs.iter()),
        }
    }

    pub fn first(self) -> Color {
        match self {
            Colors::One(x) => x,
            Colors::Many(v) => v[0],
        }
    }
}
