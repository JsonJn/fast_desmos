use super::builtins::*;
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, PartialEq)]
pub enum Primitive {
    Computable(CompPrim),
    NonComputable(NonCompPrim),
}

impl Default for Primitive {
    fn default() -> Self {
        Self::Computable(CompPrim::Number(0.0))
    }
}

impl Display for Primitive {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Computable(x) => Display::fmt(x, f),
            Self::NonComputable(x) => Display::fmt(x, f),
        }
    }
}

impl Primitive {
    pub const fn point(p: Point) -> Self {
        Primitive::Computable(CompPrim::Point(p))
    }

    pub const fn number(p: f64) -> Self {
        Primitive::Computable(CompPrim::Number(p))
    }

    pub const fn color(c: Color) -> Self {
        Self::NonComputable(NonCompPrim::Color(c))
    }

    pub const fn polygon(c: Polygon) -> Self {
        Self::NonComputable(NonCompPrim::Polygon(c))
    }
}

impl TryFrom<Primitive> for f64 {
    type Error = ();

    fn try_from(value: Primitive) -> Result<Self, Self::Error> {
        if let Primitive::Computable(CompPrim::Number(x)) = value {
            Ok(x)
        } else {
            Err(())
        }
    }
}

impl TryFrom<Primitive> for CompPrim {
    type Error = ();

    fn try_from(value: Primitive) -> Result<Self, Self::Error> {
        match value {
            Primitive::Computable(x) => Ok(x),
            Primitive::NonComputable(_) => Err(()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum CompPrim {
    Number(f64),
    Point(Point),
}

#[derive(Debug, Clone, PartialEq)]
pub enum NonCompPrim {
    Polygon(Polygon),
    Color(Color),
}

impl Display for CompPrim {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Number(x) => Display::fmt(x, f),
            Self::Point(x) => Display::fmt(x, f),
        }
    }
}

impl Display for NonCompPrim {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Polygon(x) => Display::fmt(x, f),
            Self::Color(x) => Display::fmt(x, f),
        }
    }
}
