use super::builtins::*;
use super::primitive::*;
use super::VarValue;
use crate::desmos::evaluate::{POOL_COLOR, POOL_NUMBER, POOL_POINT, POOL_POLYGON, POOL_PRIMITIVE};
use crate::pooled_vec::PooledVec;
use crate::take_pat;

#[derive(Debug, Clone, PartialEq)]
pub enum PrimList {
    Computable(CompList),
    NonComputable(NonCompList),
}

impl PrimList {
    pub fn len(&self) -> usize {
        match self {
            Self::Computable(a) => a.len(),
            Self::NonComputable(a) => a.len(),
        }
    }

    pub fn unique(self) -> Self {
        match self {
            Self::Computable(x) => Self::Computable(x.unique()),
            Self::NonComputable(x) => Self::NonComputable(x.unique()),
        }
    }

    pub fn to_list(self) -> PooledVec<Primitive> {
        match self {
            PrimList::Computable(CompList::Point(p)) => p.map_different(&POOL_PRIMITIVE, |v| {
                Primitive::Computable(CompPrim::Point(v))
            }),
            PrimList::Computable(CompList::Number(p)) => p.map_different(&POOL_PRIMITIVE, |v| {
                Primitive::Computable(CompPrim::Number(v))
            }),
            PrimList::NonComputable(NonCompList::Polygon(p)) => p
                .map_different(&POOL_PRIMITIVE, |v| {
                    Primitive::NonComputable(NonCompPrim::Polygon(v))
                }),
            PrimList::NonComputable(NonCompList::Color(p)) => p
                .map_different(&POOL_PRIMITIVE, |v| {
                    Primitive::NonComputable(NonCompPrim::Color(v))
                }),
        }
    }

    pub fn get_cloned(&self, index: usize) -> Primitive {
        match self {
            PrimList::Computable(x) => Primitive::Computable(x.get(index)),
            PrimList::NonComputable(y) => Primitive::NonComputable(y.get_cloned(index)),
        }
    }

    pub fn from_vec(vec: PooledVec<VarValue>) -> Self {
        Self::from_vec_prim(vec.map_different(
            &POOL_PRIMITIVE,
            |v| take_pat!(v => x from VarValue::Prim(x), "No nested lists!"),
        ))
    }

    pub fn from_vec_prim(vec: PooledVec<Primitive>) -> Self {
        if let Some(first) = vec.first() {
            match first {
                Primitive::Computable(CompPrim::Point(_)) => {
                    PrimList::Computable(CompList::Point(vec.map_different(
                        &POOL_POINT,
                        |v| take_pat!(v => x from Primitive::Computable(CompPrim::Point(x)), "Non homogeneous list"),
                    )))
                }
                Primitive::Computable(CompPrim::Number(_)) => {
                    PrimList::Computable(CompList::Number(vec.map_different(
                        &POOL_NUMBER,
                        |v| take_pat!(v => x from Primitive::Computable(CompPrim::Number(x)), "Non homogeneous list"),
                    )))
                }
                Primitive::NonComputable(NonCompPrim::Color(_)) => {
                    PrimList::NonComputable(NonCompList::Color(vec.map_different(
                        &POOL_COLOR,
                        |v| take_pat!(v => x from Primitive::NonComputable(NonCompPrim::Color(x)), "Non homogeneous list"),
                    )))
                }
                Primitive::NonComputable(NonCompPrim::Polygon(_)) => {
                    PrimList::NonComputable(NonCompList::Polygon(vec.map_different(
                        &POOL_POLYGON,
                        |v| take_pat!(v => x from Primitive::NonComputable(NonCompPrim::Polygon(x)), "Non homogeneous list"),
                    )))
                }
            }
        } else {
            PrimList::Computable(CompList::Number(vec.map_different(&POOL_NUMBER, |_| {
                unreachable!("Empty list, this will never be called")
            })))
        }
    }

    pub fn select(self, indices: PooledVec<usize>) -> Self {
        match self {
            Self::Computable(c) => Self::Computable(c.select(indices)),
            Self::NonComputable(n) => Self::NonComputable(n.select(indices)),
        }
    }

    pub fn truncate(&mut self, len: usize) {
        match self {
            PrimList::Computable(c) => c.truncate(len),
            PrimList::NonComputable(n) => n.truncate(len),
        }
    }
}

impl TryFrom<PrimList> for CompList {
    type Error = ();

    fn try_from(value: PrimList) -> Result<Self, Self::Error> {
        match value {
            PrimList::Computable(x) => Ok(x),
            PrimList::NonComputable(_) => Err(()),
        }
    }
}

impl From<PooledVec<Primitive>> for PrimList {
    fn from(value: PooledVec<Primitive>) -> Self {
        Self::from_vec_prim(value)
    }
}

impl From<PooledVec<VarValue>> for PrimList {
    fn from(value: PooledVec<VarValue>) -> Self {
        Self::from_vec(value)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum CompList {
    Number(PooledVec<f64>),
    Point(PooledVec<Point>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum NonCompList {
    Color(PooledVec<Color>),
    Polygon(PooledVec<Polygon>),
}

fn unique<T: PartialEq>(mut v: PooledVec<T>) -> PooledVec<T> {
    //TODO: better in-place unique algorithm

    let mut i = 0;
    while i < v.len() {
        let at = &v[i];
        if v.iter().take(i).any(|x| x == at) {
            v.remove(i);
        } else {
            i += 1;
        }
    }
    v
}

impl CompList {
    pub fn len(&self) -> usize {
        match self {
            Self::Number(a) => a.len(),
            Self::Point(a) => a.len(),
        }
    }

    pub fn unique(self) -> Self {
        match self {
            Self::Number(x) => Self::Number(unique(x)),
            Self::Point(x) => Self::Point(unique(x)),
        }
    }

    pub fn get(&self, x: usize) -> CompPrim {
        match self {
            Self::Number(a) => CompPrim::Number(a.get(x).copied().unwrap_or(f64::NAN)),
            Self::Point(a) => {
                CompPrim::Point(a.get(x).copied().unwrap_or(Point(f64::NAN, f64::NAN)))
            }
        }
    }

    pub fn select(self, indices: PooledVec<usize>) -> Self {
        match self {
            CompList::Number(ns) => {
                CompList::Number(indices.map_different(&POOL_NUMBER, |i| ns[i]))
            }
            CompList::Point(ps) => CompList::Point(indices.map_different(&POOL_POINT, |i| ps[i])),
        }
    }

    pub fn truncate(&mut self, len: usize) {
        match self {
            CompList::Number(ns) => ns.truncate(len),
            CompList::Point(ps) => ps.truncate(len),
        }
    }
}

impl TryFrom<CompList> for PooledVec<f64> {
    type Error = CompList;

    fn try_from(value: CompList) -> Result<Self, Self::Error> {
        match value {
            CompList::Number(x) => Ok(x),
            a => Err(a),
        }
    }
}

impl NonCompList {
    pub fn len(&self) -> usize {
        match self {
            Self::Color(a) => a.len(),
            Self::Polygon(a) => a.len(),
        }
    }

    pub fn unique(self) -> Self {
        match self {
            Self::Color(x) => Self::Color(unique(x)),
            Self::Polygon(x) => Self::Polygon(unique(x)),
        }
    }

    pub fn get_cloned(&self, x: usize) -> NonCompPrim {
        match self {
            Self::Polygon(a) => {
                NonCompPrim::Polygon(a.get(x).cloned().unwrap_or(Polygon(Vec::new())))
            }
            Self::Color(a) => NonCompPrim::Color(a.get(x).copied().unwrap_or(Color(0, 0, 0))),
        }
    }

    pub fn select(self, indices: PooledVec<usize>) -> Self {
        match self {
            NonCompList::Color(cs) => {
                NonCompList::Color(indices.map_different(&POOL_COLOR, |i| cs[i]))
            }
            NonCompList::Polygon(ps) => {
                NonCompList::Polygon(indices.map_different(&POOL_POLYGON, |i| ps[i].clone()))
            }
        }
    }

    pub fn truncate(&mut self, len: usize) {
        match self {
            NonCompList::Color(cs) => cs.truncate(len),
            NonCompList::Polygon(ps) => ps.truncate(len),
        }
    }
}
