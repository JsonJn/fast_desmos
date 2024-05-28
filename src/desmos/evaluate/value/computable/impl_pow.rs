use crate::desmos::evaluate::{CompList, CompPrim, Computable};
use crate::pooled_vec::PooledVec;

macro_rules! exp_type_error {
    () => {
        unreachable!("Exp can only be between number and number")
    };
}

pub trait Pow<T = Self> {
    type Output;

    fn pow(self, power: T) -> Self::Output;
}

impl Pow<f64> for CompPrim {
    type Output = CompPrim;

    fn pow(self, power: f64) -> Self::Output {
        match self {
            CompPrim::Number(x) => CompPrim::Number(x.powf(power)),
            CompPrim::Point(_) => exp_type_error!(),
        }
    }
}

impl Pow for CompPrim {
    type Output = Self;

    fn pow(self, power: Self) -> Self::Output {
        match (self, power) {
            (CompPrim::Number(x), CompPrim::Number(p)) => CompPrim::Number(x.powf(p)),
            _ => exp_type_error!(),
        }
    }
}

impl Pow<CompPrim> for CompList {
    type Output = CompList;

    fn pow(self, power: CompPrim) -> Self::Output {
        match (self, power) {
            (Self::Number(xs), CompPrim::Number(x)) => Self::Number(xs.map_same(|v| v.powf(x))),
            _ => exp_type_error!(),
        }
    }
}

impl Pow<CompList> for CompPrim {
    type Output = CompList;

    fn pow(self, power: CompList) -> Self::Output {
        match (self, power) {
            (Self::Number(x), CompList::Number(ps)) => CompList::Number(ps.map_same(|v| x.powf(v))),
            _ => exp_type_error!(),
        }
    }
}

impl Pow for CompList {
    type Output = Self;

    fn pow(self, power: Self) -> Self::Output {
        match (self, power) {
            (Self::Number(xs), Self::Number(ps)) => {
                Self::Number(PooledVec::zip_map_same(xs, ps, f64::powf))
            }
            _ => exp_type_error!(),
        }
    }
}

impl Pow<CompList> for Computable {
    type Output = CompList;

    fn pow(self, power: CompList) -> Self::Output {
        match self {
            Computable::Prim(prim) => prim.pow(power),
            Computable::List(lst) => lst.pow(power),
        }
    }
}

impl Pow<Computable> for CompList {
    type Output = CompList;

    fn pow(self, power: Computable) -> Self::Output {
        match power {
            Computable::Prim(prim) => self.pow(prim),
            Computable::List(lst) => self.pow(lst),
        }
    }
}

impl Pow for Computable {
    type Output = Self;

    fn pow(self, power: Self) -> Self::Output {
        match (self, power) {
            (Self::Prim(x), Self::Prim(p)) => Self::Prim(x.pow(p)),
            (Self::List(l), power) => Self::List(l.pow(power)),
            (base, Self::List(power)) => Self::List(base.pow(power)),
        }
    }
}
