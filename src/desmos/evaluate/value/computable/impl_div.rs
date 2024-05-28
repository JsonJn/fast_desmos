use super::super::list::CompList;
use super::super::primitive::CompPrim;
use super::Computable;
use crate::desmos::evaluate::{POOL_NUMBER, POOL_POINT};
use crate::pooled_vec::PooledVec;
use std::ops::Div;

impl Div<f64> for CompPrim {
    type Output = Self;

    fn div(self, rhs: f64) -> Self::Output {
        match self {
            Self::Number(x) => Self::Number(x / rhs),
            Self::Point(p) => Self::Point(p / rhs),
        }
    }
}

impl Div<CompPrim> for f64 {
    type Output = CompPrim;

    fn div(self, rhs: CompPrim) -> Self::Output {
        match rhs {
            CompPrim::Number(x) => CompPrim::Number(self / x),
            CompPrim::Point(_) => unreachable!("Cannot divide number by point"),
        }
    }
}

impl Div for CompPrim {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Number(x), Self::Number(rhs)) => Self::Number(x / rhs),
            (lhs, Self::Number(x)) => lhs / x,
            (Self::Point(_), Self::Point(_)) => unreachable!("Cannot divide point by point"),
            (Self::Number(_), Self::Point(_)) => unreachable!("Cannot divide point by point"),
        }
    }
}

impl Div<f64> for CompList {
    type Output = Self;

    fn div(self, rhs: f64) -> Self::Output {
        match self {
            Self::Number(x) => Self::Number(x.map_same(|j| j / rhs)),
            Self::Point(p) => Self::Point(p.map_same(|j| j / rhs)),
        }
    }
}

impl Div<CompPrim> for CompList {
    type Output = Self;

    fn div(self, rhs: CompPrim) -> Self::Output {
        match rhs {
            CompPrim::Number(x) => self / x,
            CompPrim::Point(_) => unreachable!("Cannot divide by point"),
        }
    }
}

impl Div<CompList> for f64 {
    type Output = CompList;

    fn div(self, rhs: CompList) -> Self::Output {
        match rhs {
            CompList::Number(xs) => CompList::Number(xs.map_same(|x| self / x)),
            CompList::Point(_) => unreachable!("Cannot divide number by point"),
        }
    }
}

impl Div<CompList> for CompPrim {
    type Output = CompList;

    fn div(self, rhs: CompList) -> Self::Output {
        match (self, rhs) {
            (Self::Number(x), rhs) => x / rhs,
            (Self::Point(p), CompList::Number(xs)) => {
                CompList::Point(xs.map_different(&POOL_POINT, |x| p / x))
            }
            (Self::Point(_), CompList::Point(_)) => unreachable!("Cannot divide point by point"),
        }
    }
}

impl Div for CompList {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Number(x), Self::Number(y)) => {
                Self::Number(PooledVec::zip_map_same(x, y, |a, b| a / b))
            }
            (Self::Number(x), Self::Point(p)) | (Self::Point(p), Self::Number(x)) => {
                Self::Point(PooledVec::zip_map_same(p, x, |p, x| p * x.recip()))
            }
            (Self::Point(_), Self::Point(_)) => {
                unreachable!("Cannot multiply point list by point list")
            }
        }
    }
}

impl Div<CompPrim> for Computable {
    type Output = Computable;

    fn div(self, rhs: CompPrim) -> Self::Output {
        match self {
            Self::Prim(p) => Self::Prim(p / rhs),
            Self::List(l) => Self::List(l / rhs),
        }
    }
}

impl Div<Computable> for CompPrim {
    type Output = Computable;

    fn div(self, rhs: Computable) -> Self::Output {
        match rhs {
            Computable::Prim(prim) => Computable::Prim(self / prim),
            Computable::List(list) => Computable::List(self / list),
        }
    }
}

impl Div for Computable {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Prim(c), rhs) => c / rhs,
            (Self::List(l), Self::Prim(p)) => Self::List(l / p),
            (Self::List(l1), Self::List(l2)) => Self::List(l1 / l2),
        }
    }
}
