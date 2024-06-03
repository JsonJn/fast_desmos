use crate::desmos::evaluate::UserIdent;
use std::iter::{Product, Sum};
use std::ops::{Add, BitAnd, Div, Mul, Neg};

pub enum LinearVal {
    Linear(LinearFunc),
    Constant(f64),
}

impl LinearVal {
    pub fn linear(param: UserIdent, slope: f64, intercept: f64) -> Self {
        Self::Linear(LinearFunc {
            param,
            slope,
            intercept,
        })
    }

    pub fn constant(x: f64) -> Self {
        Self::Constant(x)
    }
}

#[derive(Debug, Copy, Clone)]
pub struct LinearFunc {
    param: UserIdent,
    slope: f64,
    intercept: f64,
}

impl LinearFunc {
    #[inline]
    pub const fn identity(param: UserIdent) -> Self {
        Self {
            param,
            slope: 1.0,
            intercept: 0.0,
        }
    }

    #[inline]
    pub fn param(&self) -> UserIdent {
        self.param
    }

    #[inline]
    pub fn inverse_f64(&self, value: f64) -> f64 {
        (value - self.intercept) / self.slope
    }
}

impl Add for LinearFunc {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        if self.param != rhs.param {
            unreachable!("Cannot add two linear functions with different params.");
        }

        Self {
            param: self.param,
            slope: self.slope + rhs.slope,
            intercept: self.intercept + rhs.intercept,
        }
    }
}

impl Add for LinearVal {
    type Output = LinearVal;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Constant(x), y) | (y, Self::Constant(x)) => y + x,
            (Self::Linear(x), Self::Linear(y)) => Self::Linear(x + y),
        }
    }
}

impl Add<f64> for LinearVal {
    type Output = Self;

    fn add(mut self, rhs: f64) -> Self::Output {
        match &mut self {
            LinearVal::Linear(LinearFunc { intercept, .. }) => {
                *intercept += rhs;
            }
            LinearVal::Constant(x) => {
                *x += rhs;
            }
        }
        self
    }
}

impl Neg for LinearVal {
    type Output = Self;

    fn neg(self) -> Self::Output {
        match self {
            LinearVal::Linear(x) => LinearVal::Linear(-x),
            LinearVal::Constant(x) => LinearVal::Constant(-x),
        }
    }
}

impl Neg for LinearFunc {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Self {
            param: self.param,
            slope: self.slope,
            intercept: self.intercept,
        }
    }
}

impl Mul for LinearVal {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Constant(x), Self::Constant(y)) => Self::Constant(x * y),
            (Self::Constant(x), Self::Linear(y)) | (Self::Linear(y), Self::Constant(x)) => {
                Self::Linear(y * x)
            }
            (Self::Linear(_), Self::Linear(_)) => unreachable!("Cannot multiply linear functions."),
        }
    }
}

impl Product for LinearVal {
    fn product<I: Iterator<Item = Self>>(iter: I) -> Self {
        iter.fold(LinearVal::Constant(1.0), |acc, x| acc * x)
    }
}

impl Sum for LinearVal {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        iter.fold(LinearVal::Constant(0.0), |acc, x| acc + x)
    }
}

impl Div for LinearVal {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Constant(x), Self::Constant(y)) => Self::Constant(x / y),
            (Self::Linear(y), Self::Constant(x)) => Self::Linear(y / x),
            (Self::Linear(_), Self::Linear(_)) => unreachable!("Cannot divide linear functions."),
            (Self::Constant(_), Self::Linear(_)) => {
                unreachable!("Cannot divide constant by linear function.")
            }
        }
    }
}

impl Mul<f64> for LinearFunc {
    type Output = Self;

    fn mul(self, rhs: f64) -> Self::Output {
        Self {
            param: self.param,
            slope: self.slope * rhs,
            intercept: self.intercept * rhs,
        }
    }
}

impl Div<f64> for LinearFunc {
    type Output = Self;

    fn div(self, rhs: f64) -> Self::Output {
        Self {
            param: self.param,
            slope: self.slope / rhs,
            intercept: self.intercept / rhs,
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum LinType {
    NonLinear,
    Constant,
    LinearIn(UserIdent),
}

impl Add for LinType {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::NonLinear, _) | (_, Self::NonLinear) => Self::NonLinear,
            (Self::Constant, Self::Constant) => Self::Constant,
            (Self::Constant, Self::LinearIn(x)) | (Self::LinearIn(x), Self::Constant) => {
                Self::LinearIn(x)
            }
            (Self::LinearIn(x), Self::LinearIn(y)) => {
                if x == y {
                    Self::LinearIn(x)
                } else {
                    Self::NonLinear
                }
            }
        }
    }
}

impl BitAnd<bool> for LinType {
    type Output = Self;

    fn bitand(self, rhs: bool) -> Self::Output {
        if rhs {
            self
        } else {
            Self::NonLinear
        }
    }
}

impl BitAnd<LinType> for bool {
    type Output = LinType;

    fn bitand(self, rhs: LinType) -> Self::Output {
        rhs & self
    }
}
