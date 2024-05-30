use crate::pooled_vec::PooledVec;
use std::iter::Sum;
use std::ops::Add;

use super::super::list::CompList;
use super::super::primitive::CompPrim;
use super::Computable;

impl Add for CompPrim {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Number(x), Self::Number(y)) => Self::Number(x + y),
            (Self::Point(x), Self::Point(y)) => Self::Point(x + y),
            _ => unreachable!("Cannot add different types"),
        }
    }
}

impl Add<CompPrim> for CompList {
    type Output = Self;

    fn add(self, rhs: CompPrim) -> Self::Output {
        match (self, rhs) {
            (Self::Number(ns), CompPrim::Number(x)) => Self::Number(ns.map_same(|i| i + x)),
            (Self::Point(ns), CompPrim::Point(x)) => Self::Point(ns.map_same(|i| i + x)),
            (_s, _rhs) => unreachable!("Cannot add different types: {_s:?} + {_rhs:?}"),
        }
    }
}

impl Add for CompList {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Number(x), Self::Number(y)) => {
                Self::Number(PooledVec::zip_map_same(x, y, |a, b| a + b))
            }
            (Self::Point(x), Self::Point(y)) => {
                Self::Point(PooledVec::zip_map_same(x, y, |a, b| a + b))
            }
            _ => unreachable!("Cannot add different types"),
        }
    }
}

impl Add<CompPrim> for Computable {
    type Output = Computable;

    fn add(self, rhs: CompPrim) -> Self::Output {
        match self {
            Self::Prim(p) => Self::Prim(p + rhs),
            Self::List(l) => Self::List(l + rhs),
        }
    }
}

impl Add for Computable {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::List(l1), Self::List(l2)) => Self::List(l1 + l2),
            (Self::Prim(c), rhs) => rhs + c,
            (Self::List(l), Self::Prim(p)) => Self::List(l + p),
        }
    }
}

impl Sum for CompPrim {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        iter.fold(CompPrim::Number(0.0), |a, b| a + b)
    }
}

impl Sum for Computable {
    fn sum<I: Iterator<Item = Self>>(mut iter: I) -> Self {
        let first = iter.next();
        if let Some(first) = first {
            iter.fold(first, |a, b| a + b)
        } else {
            Computable::number(0.0)
        }
    }
}
