use crate::desmos::evaluate::{
    Color, CompList, CompPrim, NonCompList, NonCompPrim, Point, Polygon, PrimList, Primitive,
    VarValue,
};
use crate::iterator::{OptIntoIter, OptIntoIterRep, OptIter, OptIterRep, OptWindowIter};
use crate::pooled_vec::PooledVec;

macro_rules! listable_def {
    (
        $(
        $name:ident<$ty:ty> {
            One($x:ident) from $one:pat,
            Many($y:ident) from $many:pat,
        }
        )*
    ) => {
        $(
        #[derive(Debug, Clone)]
        pub enum $name {
            One($ty),
            Many(PooledVec<$ty>),
        }

        impl $name {
            #[allow(dead_code)]
            pub fn into_iter(self) -> OptIntoIter<$ty> {
                match self {
                    Self::One(x) => OptIntoIter::One(Some(x)),
                    Self::Many(xs) => OptIntoIter::Many { vec: xs, index: 0 },
                }
            }

            #[allow(dead_code)]
            pub fn into_iter_rep(self) -> OptIntoIterRep<$ty> {
                match self {
                    Self::One(x) => OptIntoIterRep::One(x),
                    Self::Many(xs) => OptIntoIterRep::Many { vec: xs, index: 0 },
                }
            }

            #[allow(dead_code)]
            pub fn iter(&self) -> OptIter<$ty> {
                match self {
                    Self::One(x) => OptIter::One(Some(x)),
                    Self::Many(xs) => OptIter::Many(xs.iter()),
                }
            }

            #[allow(dead_code)]
            pub fn iter_rep(&self) -> OptIterRep<$ty> {
                match self {
                    Self::One(x) => OptIterRep::One(x),
                    Self::Many(xs) => OptIterRep::Many(xs.iter()),
                }
            }

            #[allow(dead_code)]
            pub fn first(self) -> $ty {
                match self {
                    Self::One(x) => x,
                    Self::Many(xs) => xs[0].clone(),
                }
            }

            #[allow(dead_code)]
            pub fn windows(&self, size: usize) -> OptWindowIter<$ty> {
                assert_ne!(size, 1, "Doesn't work with size 1");

                match self {
                    Self::One(_) => OptWindowIter { iter: None },
                    Self::Many(many) => OptWindowIter {
                        iter: Some(many.windows(size)),
                    }
                }
            }
        }

        impl TryFrom<VarValue> for $name {
            type Error = VarValue;

            fn try_from(value: VarValue) -> Result<Self, Self::Error> {
                match value {
                    $one => Ok(Self::One($x)),
                    $many => Ok(Self::Many($y)),
                    a => Err(a),
                }
            }
        }
        )*
    };
}

listable_def! {
    Points<Point> {
        One(x) from VarValue::Prim(Primitive::Computable(CompPrim::Point(x))),
        Many(x) from VarValue::List(PrimList::Computable(CompList::Point(x))),
    }

    Numbers<f64> {
        One(x) from VarValue::Prim(Primitive::Computable(CompPrim::Number(x))),
        Many(x) from VarValue::List(PrimList::Computable(CompList::Number(x))),
    }

    Colors<Color> {
        One(x) from VarValue::Prim(Primitive::NonComputable(NonCompPrim::Color(x))),
        Many(x) from VarValue::List(PrimList::NonComputable(NonCompList::Color(x))),
    }

    Polygons<Polygon> {
        One(x) from VarValue::Prim(Primitive::NonComputable(NonCompPrim::Polygon(x))),
        Many(x) from VarValue::List(PrimList::NonComputable(NonCompList::Polygon(x))),
    }
}
