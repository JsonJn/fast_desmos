use crate::pooled_vec::PooledVec;
use std::iter::Sum;
use std::ops::{Add, Div, Mul, Neg, Sub};

#[derive(Debug, Copy, Clone, PartialEq, Default)]
pub struct Point(pub f64, pub f64);

impl Point {
    pub fn length(self) -> f64 {
        (self.0 * self.0 + self.1 * self.1).sqrt()
    }
}

impl Neg for Point {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Self(-self.0, -self.1)
    }
}

impl Div<f64> for Point {
    type Output = Point;

    fn div(self, rhs: f64) -> Self::Output {
        Point(self.0 / rhs, self.1 / rhs)
    }
}

impl Mul<f64> for Point {
    type Output = Point;

    fn mul(self, rhs: f64) -> Self::Output {
        Point(self.0 * rhs, self.1 * rhs)
    }
}

impl Add<Point> for Point {
    type Output = Point;

    fn add(self, rhs: Point) -> Self::Output {
        Point(self.0 + rhs.0, self.1 + rhs.1)
    }
}

impl Sum for Point {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        iter.fold(Self::default(), |i, j| i + j)
    }
}

impl Sub<Point> for Point {
    type Output = Point;

    fn sub(self, rhs: Point) -> Self::Output {
        Point(self.0 - rhs.0, self.1 - rhs.1)
    }
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Polygon(pub Vec<Point>);
#[derive(Debug, Copy, Clone, PartialEq, Default)]
pub struct Color(pub u8, pub u8, pub u8);

impl Color {
    pub fn from_hex(color: &str) -> Self {
        // remove the '#'
        let chars = color.chars().skip(1).collect::<Vec<char>>();
        let [r, g, b]: [u8; 3] = chars
            .chunks(2)
            .map(|chunk| {
                const HEX_DIGITS: [u8; 16] = *b"0123456789abcdef";

                let [c1, c2] = chunk.try_into().unwrap();
                let [c1, c2] = [c1, c2]
                    .map(|c| HEX_DIGITS.into_iter().position(|v| v == c as u8).unwrap() as u8);
                c1 * 16 + c2
            })
            .collect::<Vec<_>>()
            .try_into()
            .unwrap();

        Self(r, g, b)
    }
}
