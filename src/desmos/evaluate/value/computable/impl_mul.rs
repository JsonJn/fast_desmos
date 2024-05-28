use super::super::list::CompList;
use super::super::primitive::CompPrim;
use super::Computable;
use crate::desmos::evaluate::{Point, POOL_POINT};
use crate::pooled_vec::PooledVec;
use std::iter::Product;
use std::ops::Mul;

impl Mul<f64> for CompPrim {
    type Output = Self;

    fn mul(self, rhs: f64) -> Self::Output {
        match self {
            Self::Number(x) => Self::Number(x * rhs),
            Self::Point(p) => Self::Point(p * rhs),
        }
    }
}

impl Mul for CompPrim {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Number(x), rhs) => rhs * x,
            (lhs, Self::Number(x)) => lhs * x,
            (Self::Point(_), Self::Point(_)) => unreachable!("Cannot multiply point by point"),
        }
    }
}

impl Mul<CompPrim> for CompList {
    type Output = Self;

    fn mul(self, rhs: CompPrim) -> Self::Output {
        match self {
            Self::Number(n) => match rhs {
                CompPrim::Number(x) => Self::Number(n.map_same(|v| v * x)),
                CompPrim::Point(p) => Self::Point(n.map_different(&POOL_POINT, |v| p * v)),
            },
            Self::Point(p) => match rhs {
                CompPrim::Number(x) => Self::Point(p.map_same(|v| v * x)),
                CompPrim::Point(_) => unreachable!("Cannot multiply point by point"),
            },
        }
    }
}

impl Mul for CompList {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Number(x), Self::Number(y)) => {
                Self::Number(PooledVec::zip_map_same(x, y, f64::mul))
            }
            (Self::Number(x), Self::Point(p)) | (Self::Point(p), Self::Number(x)) => {
                Self::Point(PooledVec::zip_map_same(p, x, Point::mul))
            }
            (Self::Point(_), Self::Point(_)) => {
                unreachable!("Cannot multiply point list by point list")
            }
        }
    }
}

impl Mul<CompPrim> for Computable {
    type Output = Computable;

    fn mul(self, rhs: CompPrim) -> Self::Output {
        match self {
            Self::Prim(p) => Self::Prim(p * rhs),
            Self::List(l) => Self::List(l * rhs),
        }
    }
}

impl Mul for Computable {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Prim(c), rhs) => rhs * c,
            (Self::List(l), Self::Prim(p)) => Self::List(l * p),
            (Self::List(l1), Self::List(l2)) => Self::List(l1 * l2),
        }
    }
}

impl Product for CompPrim {
    fn product<I: Iterator<Item = Self>>(iter: I) -> Self {
        iter.fold(CompPrim::Number(1.0), |a, b| a * b)
    }
}
