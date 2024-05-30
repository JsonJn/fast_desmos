use crate::pooled_vec::Id;
use std::cmp::Ordering;
use std::fmt::{Debug, Formatter};
use std::iter;
use std::sync::atomic::AtomicU32;
use std::sync::{atomic, OnceLock};

use crate::desmos::evaluate::context::{Functions, ValueContext};
use crate::desmos::evaluate::pervasive_applies::{
    pervasive_apply_comp_variadic_bool, pervasive_apply_known,
};
use crate::desmos::evaluate::{
    pervasive_apply_variadic, Evaluable, IDENTIFIERS, POOL_INDICES, POOL_NUMBER, POOL_POINT,
    POOL_PRIMITIVE, POOL_SORT_PAIRS,
};
use crate::desmos::execute::CanDepend;
use crate::desmos::parsing::{AddOrSub, Element, InequalityType, SumOrProduct};
use crate::desmos::value::{
    Color, CompList, CompPrim, Computable, Point, Polygon, PrimList, Primitive, VarValue,
};
use crate::gamma::gamma;
use crate::pooled_vec::PooledVec;
use crate::take_pat;

#[derive(PartialEq, Debug)]
pub struct EvalExpr {
    pub expr: Box<EvalTree>,
    pub cache: Option<OnceLock<VarValue>>,
}

impl EvalExpr {
    pub fn new(tree: EvalTree) -> Self {
        Self {
            cache: tree.get_deps().is_empty().then(OnceLock::new),
            expr: Box::new(tree),
        }
    }

    pub fn new_generated(eval_kind: EvalKind) -> Self {
        Self::new(EvalTree::new_generated(eval_kind))
    }

    pub fn get_cached(&self) -> Option<VarValue> {
        self.cache.as_ref().and_then(|v| v.get().cloned())
    }

    pub fn can_cache(&self) -> bool {
        self.cache.is_some()
    }

    pub fn set_cache(&self, value: &VarValue) {
        if let Some(cache) = &self.cache {
            let Ok(_) = cache.set(value.clone()) else {
                unreachable!("Cannot cache already cached")
            };
        } else {
            unreachable!("Caching requires that it be cachable")
        }
    }
}

#[derive(PartialEq, Copy, Clone, Eq, Hash)]
pub struct UserIdent(pub usize);

impl Debug for UserIdent {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let name = IDENTIFIERS.int_to_name(*self).unwrap();
        write!(f, "UserIdent(\"{name}\")")
    }
}

impl From<UserIdent> for usize {
    fn from(value: UserIdent) -> Self {
        value.0
    }
}

#[derive(PartialEq, Debug, Copy, Clone)]
pub enum Ident {
    User(UserIdent),
    Builtin(Builtins),
}

impl Ident {
    pub fn to_user(self) -> Option<UserIdent> {
        match self {
            Ident::User(x) => Some(x),
            Ident::Builtin(_) => None,
        }
    }
}

#[derive(PartialEq, Debug, Copy, Clone)]
pub enum MonadicPervasive {
    Sin,
    Cos,
    Tan,
    Sec,
    Csc,
    Cot,

    Cosh,
    Sinh,
    Tanh,
    Sech,
    Csch,
    Coth,

    ArcSin,
    ArcCos,
    ArcTan,
    ArcSec,
    ArcCsc,
    ArcCot,

    ArcCosh,
    ArcSinh,
    ArcTanh,
    ArcSech,
    ArcCsch,
    ArcCoth,

    Sign,
    Floor,
    Ceil,
    Round,
}

#[derive(PartialEq, Debug, Copy, Clone)]
pub enum DyadicPervasive {
    Mod,
    Choose,
    Permutation,
}

#[derive(PartialEq, Debug, Copy, Clone)]
pub enum TriadicPervasive {
    Rgb,
    Hsv,
}

#[derive(PartialEq, Debug, Copy, Clone)]
pub enum ListStat {
    Mean,
    Min,
    Max,
    Total,
}

#[derive(PartialEq, Debug, Copy, Clone)]
pub enum MonadicNonPervasive {
    Length,
    Unique,
}

#[derive(PartialEq, Debug, Copy, Clone)]
pub enum Builtins {
    MonadicPervasive(MonadicPervasive),
    DyadicPervasive(DyadicPervasive),
    TriadicPervasive(TriadicPervasive),
    MonadicNonPervasive(MonadicNonPervasive),

    Join,    // variadic non-pervasive
    Sort,    // monadic/dyadic non-pervasive
    Random,  // zero-adic / monadic non-pervasive / dyadic non-pervasive
    Polygon, // monadic non-pervasive / variadic pervasive
    ListStat(ListStat),
}

impl MonadicPervasive {
    fn inverse(self) -> Self {
        match self {
            Self::Sin => Self::ArcSin,
            Self::Cos => Self::ArcCos,
            Self::Tan => Self::ArcTan,
            Self::Sec => Self::ArcSec,
            Self::Csc => Self::ArcCsc,
            Self::Cot => Self::ArcCot,
            Self::Cosh => Self::ArcCosh,
            Self::Sinh => Self::ArcSinh,
            Self::Tanh => Self::ArcTanh,
            Self::Sech => Self::ArcSech,
            Self::Csch => Self::ArcCsch,
            Self::Coth => Self::ArcCoth,
            Self::ArcSin => Self::Sin,
            Self::ArcCos => Self::Cos,
            Self::ArcTan => Self::Tan,
            Self::ArcSec => Self::Sec,
            Self::ArcCsc => Self::Csc,
            Self::ArcCot => Self::Cot,
            Self::ArcCosh => Self::Cosh,
            Self::ArcSinh => Self::Sinh,
            Self::ArcTanh => Self::Tanh,
            Self::ArcSech => Self::Sech,
            Self::ArcCsch => Self::Csch,
            Self::ArcCoth => Self::Coth,
            Self::Sign => unreachable!("No inverse for sign"),
            Self::Floor => unreachable!("No inverse for floor"),
            Self::Ceil => unreachable!("No inverse for ceil"),
            Self::Round => unreachable!("No inverse for round"),
        }
    }

    pub fn apply(self, [prim]: [Primitive; 1], inv: bool) -> Primitive {
        let num: f64 = prim.try_into().expect("Monadics only support numbers");

        let applied = match inv {
            true => self.inverse(),
            false => self,
        };

        Primitive::number(match applied {
            MonadicPervasive::Sin => num.sin(),
            MonadicPervasive::Cos => num.cos(),
            MonadicPervasive::Tan => num.tan(),
            MonadicPervasive::Sec => num.cos().recip(),
            MonadicPervasive::Csc => num.sin().recip(),
            MonadicPervasive::Cot => num.tan().recip(),
            MonadicPervasive::Cosh => num.cosh(),
            MonadicPervasive::Sinh => num.sinh(),
            MonadicPervasive::Tanh => num.tanh(),
            MonadicPervasive::Sech => num.cosh().recip(),
            MonadicPervasive::Csch => num.sinh().recip(),
            MonadicPervasive::Coth => num.tanh().recip(),
            MonadicPervasive::ArcSin => num.asin(),
            MonadicPervasive::ArcCos => num.acos(),
            MonadicPervasive::ArcTan => num.atan(),
            MonadicPervasive::ArcSec => num.recip().acos(),
            MonadicPervasive::ArcCsc => num.recip().asin(),
            MonadicPervasive::ArcCot => num.recip().atan(),
            MonadicPervasive::ArcCosh => num.acosh(),
            MonadicPervasive::ArcSinh => num.asinh(),
            MonadicPervasive::ArcTanh => num.atanh(),
            MonadicPervasive::ArcSech => num.recip().acosh(),
            MonadicPervasive::ArcCsch => num.recip().asinh(),
            MonadicPervasive::ArcCoth => num.recip().atanh(),
            MonadicPervasive::Sign => {
                if num == 0.0 {
                    0.0
                } else {
                    num.signum()
                }
            }
            MonadicPervasive::Floor => num.floor(),
            MonadicPervasive::Ceil => num.ceil(),
            MonadicPervasive::Round => num.round(),
        })
    }
}

impl DyadicPervasive {
    pub fn apply(self, [a, b]: [Primitive; 2]) -> Primitive {
        let (first, second) = (a.try_into(), b.try_into());
        let (first, second): (f64, f64) = (
            first.expect("Dyadics only support numbers"),
            second.expect("Dyadics only support numbers"),
        );
        Primitive::number(match self {
            DyadicPervasive::Mod => first.rem_euclid(second),
            DyadicPervasive::Choose => gamma(first) / (gamma(second) * gamma(first - second)),
            DyadicPervasive::Permutation => gamma(first) / gamma(first - second),
        })
    }
}

impl TriadicPervasive {
    pub fn apply(self, [a, b, c]: [Primitive; 3]) -> Primitive {
        self.apply_internal(
            a.try_into().unwrap(),
            b.try_into().unwrap(),
            c.try_into().unwrap(),
        )
    }

    pub fn apply_internal(self, a: f64, b: f64, c: f64) -> Primitive {
        match self {
            TriadicPervasive::Rgb => Primitive::color(Color(a as u8, b as u8, c as u8)),
            TriadicPervasive::Hsv => {
                let h = (a * 360.0) % 360.0;
                let s = b.max(0.0).min(1.0);
                let v = c.max(0.0).min(1.0);

                let c = v * s;
                let x = c * (1.0 - ((h / 60.0) % 2.0 - 1.0).abs());
                let m = v - c;

                let (r, g, b) = if h < 60.0 {
                    (c, x, 0.0)
                } else if h < 120.0 {
                    (x, c, 0.0)
                } else if h < 180.0 {
                    (0.0, c, x)
                } else if h < 240.0 {
                    (0.0, x, c)
                } else if h < 300.0 {
                    (x, 0.0, c)
                } else {
                    (c, 0.0, x)
                };

                let (r, g, b) = (
                    ((r + m) * 255.0) as u8,
                    ((g + m) * 255.0) as u8,
                    ((b + m) * 255.0) as u8,
                );

                Primitive::color(Color(r, g, b))
            }
        }
    }
}

impl MonadicNonPervasive {
    pub fn apply(self, data: VarValue) -> VarValue {
        match self {
            MonadicNonPervasive::Length => {
                if let VarValue::List(list) = data {
                    VarValue::number(list.len() as f64)
                } else {
                    unreachable!("Length on a non-list value");
                }
            }
            MonadicNonPervasive::Unique => {
                if let VarValue::List(list) = data {
                    VarValue::List(list.unique())
                } else {
                    unreachable!("Unique on a non-list value");
                }
            }
        }
    }
}

impl ListStat {
    pub fn apply(self, vec: impl IntoIterator<Item = Primitive>) -> Primitive {
        let mut vec = vec
            .into_iter()
            .map(|i| take_pat!(i => x from Primitive::Computable(x), "ListStat must be applied over computables."));
        match vec.next() {
            Some(CompPrim::Number(num)) => {
                let numbers = iter::once(num)
                    .chain(vec.map(|prim| take_pat!(prim => x from CompPrim::Number(x))));
                Primitive::number(match self {
                    Self::Mean => {
                        let (len, sum) =
                            numbers.fold((0.0, 0.0), |(len, sum), x| (len + 1.0, sum + x));
                        sum / len
                    }
                    Self::Min => numbers.fold(f64::NAN, |a, b| a.min(b)),
                    Self::Max => numbers.fold(f64::NAN, |a, b| a.max(b)),
                    Self::Total => numbers.sum(),
                })
            }
            Some(CompPrim::Point(_)) => {
                let points = vec.map(|prim| {
                    take_pat!(
                        prim => x from CompPrim::Point(x)
                    )
                });

                Primitive::point(match self {
                    Self::Mean => {
                        let (len, sum) = points.fold((0.0, Point::default()), |(len, sum), x| {
                            (len + 1.0, sum + x)
                        });
                        sum / len
                    }
                    Self::Total => points.sum::<Point>(),
                    Self::Min | Self::Max => unreachable!("Can't min/max on point"),
                })
            }
            None => Primitive::number(f64::NAN),
        }
    }

    pub fn apply_comp_list(self, list: CompList) -> Primitive {
        match list {
            CompList::Number(ns) => Primitive::number(self.apply_number(&ns)),
            CompList::Point(ps) => Primitive::point(self.apply_point(&ps)),
        }
    }

    pub fn apply_point(self, l: &PooledVec<Point>) -> Point {
        match self {
            ListStat::Mean => {
                let len = l.len() as f64;
                let total: Point = l.iter().copied().sum();
                total / len
            }
            Self::Max => unreachable!("Max cannot be applied to a list of points"),
            Self::Min => unreachable!("Min cannot be applied to a list of points"),
            Self::Total => l.iter().copied().sum(),
        }
    }

    pub fn apply_number(self, l: &PooledVec<f64>) -> f64 {
        match self {
            Self::Mean => {
                let len = l.len();
                l.iter().copied().sum::<f64>() / len as f64
            }
            Self::Min => l.iter().copied().fold(f64::NAN, |a, b| a.min(b)),
            Self::Max => l.iter().copied().fold(f64::NAN, |a, b| a.max(b)),
            Self::Total => l.iter().copied().sum(),
        }
    }
}

impl Builtins {
    pub fn evaluate(self, id: Id, params: Vec<VarValue>, power: Option<VarValue>) -> VarValue {
        let (inverse, squared) = match power {
            Some(VarValue::Prim(Primitive::Computable(CompPrim::Number(x)))) => match x {
                x if x == -1.0 => (true, false),
                x if x == 2.0 => (false, true),
                x if x == -2.0 => (true, true),
                _ => unreachable!("Function exponentiation must be by either -1 or 2"),
            },
            Some(_) => unreachable!("Function exponentiation must be by a power"),
            None => (false, false),
        };

        let normal_val = match self {
            Builtins::MonadicPervasive(m) => {
                if params.len() != 1 {
                    unreachable!("One and only one parameter for monadic function {m:?}");
                }

                pervasive_apply_known(id, params.try_into().unwrap(), |k| m.apply(k, inverse))
            }
            Builtins::DyadicPervasive(m) => {
                if params.len() != 2 {
                    unreachable!("Two and only two parameter for dyadic function {m:?}");
                }

                pervasive_apply_known(id, params.try_into().unwrap(), |k| m.apply(k))
            }
            Builtins::TriadicPervasive(m) => {
                if params.len() != 3 {
                    unreachable!("Three and only three parameters for triadic function {m:?}");
                }

                pervasive_apply_known(id, params.try_into().unwrap(), |k| m.apply(k))
            }
            Builtins::Polygon => {
                if params.len() == 1 {
                    let [x] = params.try_into().unwrap();
                    if let VarValue::List(PrimList::Computable(CompList::Point(p))) = x {
                        VarValue::polygon(Polygon(p.pooled_to_vec()))
                    } else {
                        unreachable!("Invalid monadic parameters to polygon.")
                    }
                } else {
                    pervasive_apply_variadic(id, params, |vars| {
                        let vars = PooledVec::from_iter(&POOL_POINT, id, vars.map(|v| {
                            take_pat!(v => p from Primitive::Computable(CompPrim::Point(p)), "Non point argument to polygon")
                        }));

                        Primitive::polygon(Polygon(vars.pooled_to_vec()))
                    })
                }
            }
            Builtins::MonadicNonPervasive(m) => {
                let Ok([a]): Result<[VarValue; 1], _> = params.try_into() else {
                    unreachable!("Monadic non pervasive is one-parameter.")
                };

                m.apply(a)
            }
            Builtins::Join => {
                let mut joined = PooledVec::new(&POOL_PRIMITIVE, id);
                for param in params {
                    match param {
                        VarValue::Prim(prim) => joined.push(prim),
                        VarValue::List(list) => {
                            for i in 0..list.len() {
                                joined.push(list.get_cloned(i));
                            }
                        }
                    }
                }

                VarValue::List(joined.into())
            }
            Builtins::Sort => match params.len() {
                1 => {
                    let [param] = params.try_into().unwrap();
                    let VarValue::List(list) = param else {
                        unreachable!("Cannot sort non-list")
                    };

                    let PrimList::Computable(CompList::Number(mut list)) = list else {
                        unreachable!("Can only sort numbers")
                    };

                    list.sort_unstable_by(|a, b| a.partial_cmp(b).unwrap());

                    VarValue::num_list(list)
                }
                2 => {
                    let [param, keys]: [VarValue; 2] = params.try_into().unwrap();
                    let (VarValue::List(mut param), VarValue::List(keys)) = (param, keys) else {
                        unreachable!("Dyadic sort must take lists as input")
                    };
                    let PrimList::Computable(CompList::Number(mut keys)) = keys else {
                        unreachable!("Can only sort numbers")
                    };
                    let len = param.len().min(keys.len());
                    param.truncate(len);
                    keys.truncate(len);

                    let mut ind_keys = PooledVec::from_iter(
                        &POOL_SORT_PAIRS,
                        id,
                        (0..keys.len()).zip(keys.iter().copied()),
                    );
                    ind_keys.sort_by(|&(_, a), (_, b)| a.partial_cmp(b).unwrap());
                    let ind = ind_keys.map_different(&POOL_INDICES, |i| i.0);

                    VarValue::List(param.select(ind))
                }
                _ => unreachable!("Sort must take 1 or 2 parameters"),
            },
            Builtins::Random => match params.len() {
                0 => VarValue::number(fastrand::f64()),
                1 => {
                    let [param] = params.try_into().unwrap();
                    match param {
                        VarValue::Prim(p) => {
                            if let Primitive::Computable(CompPrim::Number(amt)) = p {
                                let amt = amt.round() as usize;
                                let ns = PooledVec::from_iter(
                                    &POOL_NUMBER,
                                    id,
                                    (0..amt).map(|_| fastrand::f64()),
                                );
                                VarValue::num_list(ns)
                            } else {
                                unreachable!("Monadic random must take number or list input")
                            }
                        }
                        VarValue::List(l) => {
                            let len = l.len();
                            let index = fastrand::usize(0..len);
                            VarValue::Prim(l.get_cloned(index))
                        }
                    }
                }
                2 => {
                    let [list, amt] = params.try_into().unwrap();
                    let VarValue::List(list) = list else {
                        unreachable!("Dyadic random must take list first parameter")
                    };
                    let VarValue::Prim(Primitive::Computable(CompPrim::Number(amt))) = amt else {
                        unreachable!("Dyadic random must take number second parameter")
                    };
                    let amt = amt.round() as usize;

                    let mut chosen = PooledVec::with_capacity(&POOL_INDICES, id, amt);
                    for _ in 0..amt {
                        loop {
                            let index = fastrand::usize(0..list.len());
                            if !chosen.contains(&index) {
                                chosen.push(index);
                                break;
                            }
                        }
                    }

                    VarValue::List(list.select(chosen))
                }
                _ => unreachable!("Random must take 0, 1, or 2 parameters"),
            },
            Builtins::ListStat(l) => match params.len().cmp(&1) {
                Ordering::Equal => {
                    let [param]: [VarValue; 1] = params.try_into().unwrap();
                    match param {
                        VarValue::Prim(_) => param,
                        VarValue::List(PrimList::Computable(list)) => {
                            VarValue::Prim(l.apply_comp_list(list))
                        }
                        VarValue::List(PrimList::NonComputable(_)) => {
                            unreachable!("Non-computable cannot be summed. ")
                        }
                    }
                }
                Ordering::Greater => pervasive_apply_variadic(id, params, |k| l.apply(k)),
                _ => unreachable!("ListStat must take 1 or more parameters"),
            },
        };

        if squared {
            let comp: Computable = normal_val
                .try_into()
                .expect("Call-like square must take computable");

            (comp.clone() * comp).into()
        } else {
            normal_val
        }
    }

    pub fn from_ident(name: &str) -> Self {
        match name {
            "_sin" => Self::MonadicPervasive(MonadicPervasive::Sin),
            "_cos" => Self::MonadicPervasive(MonadicPervasive::Cos),
            "_tan" => Self::MonadicPervasive(MonadicPervasive::Tan),
            "_sec" => Self::MonadicPervasive(MonadicPervasive::Sec),
            "_csc" => Self::MonadicPervasive(MonadicPervasive::Csc),
            "_cot" => Self::MonadicPervasive(MonadicPervasive::Cot),
            "_cosh" => Self::MonadicPervasive(MonadicPervasive::Cosh),
            "_sinh" => Self::MonadicPervasive(MonadicPervasive::Sinh),
            "_tanh" => Self::MonadicPervasive(MonadicPervasive::Tanh),
            "_sech" => Self::MonadicPervasive(MonadicPervasive::Sech),
            "_csch" => Self::MonadicPervasive(MonadicPervasive::Csch),
            "_coth" => Self::MonadicPervasive(MonadicPervasive::Coth),
            "_arcsin" => Self::MonadicPervasive(MonadicPervasive::ArcSin),
            "_arccos" => Self::MonadicPervasive(MonadicPervasive::ArcCos),
            "_arctan" => Self::MonadicPervasive(MonadicPervasive::ArcTan),
            "_arcsec" => Self::MonadicPervasive(MonadicPervasive::ArcSec),
            "_arccsc" => Self::MonadicPervasive(MonadicPervasive::ArcCsc),
            "_arccot" => Self::MonadicPervasive(MonadicPervasive::ArcCot),
            "_arcot" => Self::MonadicPervasive(MonadicPervasive::ArcCot),
            "_arccosh" => Self::MonadicPervasive(MonadicPervasive::ArcCosh),
            "_arcosh" => Self::MonadicPervasive(MonadicPervasive::ArcCosh),
            "_arcsinh" => Self::MonadicPervasive(MonadicPervasive::ArcSinh),
            "_arctanh" => Self::MonadicPervasive(MonadicPervasive::ArcTanh),
            "_arcsech" => Self::MonadicPervasive(MonadicPervasive::ArcSech),
            "_arccsch" => Self::MonadicPervasive(MonadicPervasive::ArcCsch),
            "_arcsch" => Self::MonadicPervasive(MonadicPervasive::ArcCsch),
            "_arccoth" => Self::MonadicPervasive(MonadicPervasive::ArcCoth),
            "_arcoth" => Self::MonadicPervasive(MonadicPervasive::ArcCoth),
            "_sign" => Self::MonadicPervasive(MonadicPervasive::Sign),
            "_floor" => Self::MonadicPervasive(MonadicPervasive::Floor),
            "_ceil" => Self::MonadicPervasive(MonadicPervasive::Ceil),
            "_round" => Self::MonadicPervasive(MonadicPervasive::Round),

            "_mod" => Self::DyadicPervasive(DyadicPervasive::Mod),
            "_nCr" => Self::DyadicPervasive(DyadicPervasive::Choose),
            "_nPr" => Self::DyadicPervasive(DyadicPervasive::Permutation),

            "_length" => Self::MonadicNonPervasive(MonadicNonPervasive::Length),
            "_unique" => Self::MonadicNonPervasive(MonadicNonPervasive::Unique),

            "_join" => Self::Join,
            "_sort" => Self::Sort,
            "_random" => Self::Random,
            "_polygon" => Self::Polygon,

            "_mean" => Self::ListStat(ListStat::Mean),
            "_min" => Self::ListStat(ListStat::Min),
            "_max" => Self::ListStat(ListStat::Max),
            "_total" => Self::ListStat(ListStat::Total),

            "_rgb" => Self::TriadicPervasive(TriadicPervasive::Rgb),
            "_hsv" => Self::TriadicPervasive(TriadicPervasive::Hsv),

            _ => panic!("Unidentified name: {name:?}"),
        }
    }
}

#[derive(PartialEq, Debug)]
pub struct VarDef {
    pub var: UserIdent,
    pub expr: EvalExpr,
}

#[derive(PartialEq, Debug)]
pub enum Conditional {
    Inequality {
        id: Id,
        comp: Vec<InequalityType>,
        exprs: Vec<EvalExpr>,
    },
    Equality {
        id: Id,
        exprs: Vec<EvalExpr>,
    },
}

impl InequalityType {
    pub fn matches(self, ord: Ordering) -> bool {
        match self {
            InequalityType::LessThan => ord == Ordering::Less,
            InequalityType::LessOrEqual => ord != Ordering::Greater,
            InequalityType::MoreThan => ord == Ordering::Greater,
            InequalityType::MoreOrEqual => ord != Ordering::Less,
        }
    }
}

#[derive(Debug, Clone)]
pub enum CondOutput {
    Prim(bool),
    List(PooledVec<bool>),
}

fn to_f64(b: bool) -> f64 {
    if b {
        1.0
    } else {
        0.0
    }
}

impl From<CondOutput> for Computable {
    fn from(value: CondOutput) -> Self {
        match value {
            CondOutput::Prim(b) => Self::number(to_f64(b)),
            CondOutput::List(bs) => {
                Self::List(CompList::Number(bs.map_different(&POOL_NUMBER, to_f64)))
            }
        }
    }
}

impl From<CondOutput> for VarValue {
    fn from(value: CondOutput) -> Self {
        match value {
            CondOutput::Prim(b) => Self::number(to_f64(b)),
            CondOutput::List(bs) => Self::num_list(bs.map_different(&POOL_NUMBER, to_f64)),
        }
    }
}

impl From<Result<bool, PooledVec<bool>>> for CondOutput {
    fn from(value: Result<bool, PooledVec<bool>>) -> Self {
        match value {
            Ok(b) => Self::Prim(b),
            Err(bs) => Self::List(bs),
        }
    }
}

impl Conditional {
    pub fn evaluate(&self, functions: &Functions, context: &mut ValueContext) -> CondOutput {
        match self {
            Conditional::Inequality { id, exprs, comp } => {
                if exprs.is_empty() {
                    unreachable!("Inequality cannot have zero expressions");
                }

                let values: Vec<_> = exprs
                    .iter()
                    .map(|v| -> Computable { v.evaluate(functions, context).try_into().unwrap() })
                    .collect();

                // println!("Values: {values:?}");
                // println!("Comparisons: {comp:?}");

                let result = pervasive_apply_comp_variadic_bool(*id, values, |vals| {
                    let mut last = vals.next().unwrap();
                    for (next, &c) in vals.zip(comp) {
                        let cmp = last.cmp(next).unwrap();
                        let correct = c.matches(cmp);
                        if !correct {
                            return false;
                        }
                        last = next;
                    }
                    true
                })
                .into();

                // println!("Result: {result:?}");

                result
            }
            Conditional::Equality { id, exprs } => {
                if exprs.is_empty() {
                    unreachable!("Equality cannot have zero expressions");
                }

                let values = exprs
                    .iter()
                    .map(|v| -> Computable { v.evaluate(functions, context).try_into().unwrap() })
                    .collect::<Vec<_>>();

                pervasive_apply_comp_variadic_bool(*id, values, |vals| {
                    let mut last = vals.next().unwrap();
                    for next in vals {
                        let correct = last == next;
                        if !correct {
                            return false;
                        }
                        last = next;
                    }
                    true
                })
                .into()
            }
        }
    }
}

pub static NEXT_ID: AtomicU32 = AtomicU32::new(10000);

#[derive(PartialEq, Debug)]
pub struct EvalTree {
    pub id: Id,
    pub kind: EvalKind,
}

impl EvalTree {
    pub fn new_generated(kind: EvalKind) -> Self {
        Self {
            id: Id::new(NEXT_ID.fetch_add(1, atomic::Ordering::Relaxed)),
            kind,
        }
    }

    /// This is unsafe because ids passed
    /// to `PooledVec` should never collide.
    ///
    /// Users have to ensure that the EvalKind
    /// passed doesn't create any `PooledVec`s.
    pub unsafe fn new_zeroed(kind: EvalKind) -> Self {
        Self {
            id: Id::zeroed(),
            kind,
        }
    }
}

#[derive(PartialEq, Debug)]
pub enum EvalKind {
    Ident(UserIdent),
    Number(f64),
    Point {
        x: EvalExpr,
        y: EvalExpr,
    },
    List(Vec<EvalExpr>),
    ListRange {
        before: Vec<EvalExpr>,
        after: Vec<EvalExpr>,
    },
    ListComprehension {
        expr: EvalExpr,
        defs: Vec<VarDef>,
    },
    AbsoluteValue(EvalExpr),
    Fraction {
        top: EvalExpr,
        bottom: EvalExpr,
    },
    Root {
        nth: f64,
        expr: EvalExpr,
    },
    FunctionCall {
        func: Ident,
        prime_count: u32,
        params: Vec<EvalExpr>,
        power: Option<EvalExpr>,
    },
    Power {
        base: EvalExpr,
        power: EvalExpr,
    },
    IntPower {
        base: EvalExpr,
        power: i32,
    },
    ListIndexing {
        list: EvalExpr,
        index: EvalExpr,
    },
    ListFiltering {
        list: EvalExpr,
        filter: Conditional,
    },
    ElementAccess {
        expr: EvalExpr,
        element: Element,
    },
    Multiply(Vec<EvalExpr>),
    Differentiate(EvalExpr),
    SumProd {
        kind: SumOrProduct,
        expr: EvalExpr,
        counter: UserIdent,
        from: EvalExpr,
        to: EvalExpr,
    },
    AddSub {
        kinds: Vec<AddOrSub>,
        exprs: Vec<EvalExpr>,
    },
    IfElse {
        cond: Conditional,
        yes: EvalExpr,
        no: EvalExpr,
    },
}
