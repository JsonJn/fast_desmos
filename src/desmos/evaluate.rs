use std::cell::OnceCell;

use once_cell::sync::Lazy;

pub use context::{Functions, FunctionsBuilder, ValueContext};
pub use convert::ToEval;
pub use convert::{IdentifierStorer, IDENTIFIERS};
pub use linear::*;
pub use tree::*;

use crate::desmos::evaluate::pervasive_applies::{
    pervasive_apply_comp_known, pervasive_apply_known, pervasive_apply_non_prim_known,
    pervasive_apply_variadic,
};
use crate::desmos::parsing::{AddOrSub, Element, SumOrProduct};
pub use crate::desmos::value::*;
use crate::pooled_vec::{PooledVec, VecPool};

pub use dependency::{reorder_inplace, topological_indices, topological_sort, CanDepend};

mod constness;
mod context;
mod convert;
mod dependency;
mod linear;
mod pervasive_applies;
mod tree;

pub static POOL_NUMBER: Lazy<VecPool<f64>> = Lazy::new(VecPool::default);
pub static POOL_POINT: Lazy<VecPool<Point>> = Lazy::new(VecPool::default);
pub static POOL_COLOR: Lazy<VecPool<Color>> = Lazy::new(VecPool::default);
pub static POOL_POLYGON: Lazy<VecPool<Polygon>> = Lazy::new(VecPool::default);

pub static POOL_PRIMITIVE: Lazy<VecPool<Primitive>> = Lazy::new(VecPool::default);
pub static POOL_VAR_VALUE: Lazy<VecPool<VarValue>> = Lazy::new(VecPool::default);

pub static POOL_SORT_PAIRS: Lazy<VecPool<(usize, f64)>> = Lazy::new(VecPool::default);
pub static POOL_INDICES: Lazy<VecPool<usize>> = Lazy::new(VecPool::default);
pub static POOL_BOOL: Lazy<VecPool<bool>> = Lazy::new(VecPool::default);

pub trait Evaluable {
    fn evaluate(&self, functions: &Functions, context: &mut ValueContext) -> VarValue;
}

impl Evaluable for EvalExpr {
    fn evaluate(&self, functions: &Functions, context: &mut ValueContext) -> VarValue {
        if let Some(cached) = self.get_cached() {
            cached
        } else {
            let value = self.expr.evaluate(functions, context);
            if self.can_cache() {
                self.set_cache(&value);
            }
            value
        }
    }
}

impl Evaluable for EvalTree {
    fn evaluate(&self, functions: &Functions, context: &mut ValueContext) -> VarValue {
        let id = self.id;
        match &self.kind {
            &EvalKind::Ident(id) => context.get_value(id),
            &EvalKind::Number(num) => VarValue::number(num),
            EvalKind::Point { x, y } => pervasive_apply_known(
                id,
                [
                    x.evaluate(functions, context),
                    y.evaluate(functions, context),
                ],
                |[i, j]| {
                    let (Ok(x), Ok(y)) = (i.try_into(), j.try_into()) else {
                        unreachable!("Point must have number as coordinates")
                    };
                    Primitive::point(Point(x, y))
                },
            ),
            EvalKind::List(list) => {
                let values = PooledVec::from_iter(
                    &POOL_VAR_VALUE,
                    id,
                    list.iter().map(|v| v.evaluate(functions, context)),
                );

                VarValue::List(values.into())
            }
            EvalKind::ListRange { before, after } => {
                if before.is_empty() || after.is_empty() {
                    unreachable!("Half empty list range")
                }

                let [before, after]: [Vec<f64>; 2] = [before, after].map(|list| {
                    list.iter()
                        .map(|v| {
                            let VarValue::Prim(x) = v.evaluate(functions, context) else {
                                unreachable!("No nested lists!")
                            };
                            x.try_into().expect("List range must only be numbers")
                        })
                        .collect()
                });

                let delta = if before.len() == 1 {
                    1.0
                } else {
                    before[1] - before[0]
                };

                let end = after.last().copied().unwrap();

                let mut vec = PooledVec::new(&POOL_NUMBER, id);

                let mut x = before[0];
                while x < end {
                    vec.push(x);
                    x += delta;
                }
                vec.push(x);

                VarValue::num_list(vec)
            }
            EvalKind::ListComprehension { expr, defs } => {
                let (idents, values): (Vec<_>, Vec<_>) = defs
                    .iter()
                    .map(|v| {
                        let VarDef { var, expr } = v;
                        (*var, expr.evaluate(functions, context))
                    })
                    .unzip();

                let lengths: Vec<_> = values
                    .iter()
                    .map(|v| {
                        v.len()
                            .expect("List comprehension must have lists as input")
                    })
                    .collect();

                let end_length = lengths.iter().copied().product();

                let scanned: Vec<_> = lengths
                    .iter()
                    .scan(1, |a, &b| {
                        let tmp = *a;
                        *a *= b;
                        Some(tmp)
                    })
                    .collect();

                let result = PooledVec::from_iter(
                    &POOL_VAR_VALUE,
                    id,
                    (0..end_length).map(|x| {
                        let indices = (0..lengths.len()).map(|i| (x / scanned[i]) % lengths[i]);

                        let (ids, values): (Vec<_>, Vec<_>) = idents
                            .iter()
                            .copied()
                            .zip(values.iter())
                            .zip(indices)
                            .map(|((id, list), index)| (id, VarValue::Prim(list.get_cloned(index))))
                            .unzip();

                        let old_vals: Vec<_> = ids
                            .iter()
                            .zip(values)
                            .map(|(&id, val)| context.set_value(id, val))
                            .collect();

                        let e = expr.evaluate(functions, context);
                        for (id, old) in ids.into_iter().zip(old_vals) {
                            context.un_or_set(id, old);
                        }
                        e
                    }),
                );

                VarValue::List(result.into())
            }
            EvalKind::AbsoluteValue(expr) => pervasive_apply_comp_known(
                id,
                [expr.evaluate(functions, context).try_into().unwrap()],
                |[i]| match i {
                    CompPrim::Number(x) => Primitive::number(x.abs()),
                    CompPrim::Point(x) => Primitive::number(x.length()),
                },
            ),
            EvalKind::Fraction { top, bottom } => {
                let top: Result<Computable, _> = top.evaluate(functions, context).try_into();
                let bottom: Result<Computable, _> = bottom.evaluate(functions, context).try_into();

                let (Ok(top), Ok(bottom)) = (top, bottom) else {
                    unreachable!("Can only divide computables");
                };

                (top / bottom).into()
            }
            EvalKind::Root { nth, expr } => {
                let value: Computable = expr
                    .evaluate(functions, context)
                    .try_into()
                    .expect("Root can only take numbers");
                let nth = (*nth).recip();

                pervasive_apply_comp_known(id, [value], |[i]| match i {
                    CompPrim::Number(x) => Primitive::number(x.powf(nth)),
                    CompPrim::Point(_) => unreachable!("Root can only take numbers"),
                })
            }
            EvalKind::FunctionCall {
                func,
                prime_count: _,
                params,
                power,
            } => {
                let p_vals: Vec<_> = params
                    .iter()
                    .map(|v| v.evaluate(functions, context))
                    .collect();
                match *func {
                    Ident::User(func) => {
                        if context.is_initialized(func) {
                            let value = context.get_value(func);
                            let computable: Computable = value
                                .try_into()
                                .expect("Call-like multiplication must be by a computable");
                            match p_vals.len() {
                                1 => {
                                    let [param] = p_vals.try_into().unwrap();
                                    let param: Computable = param
                                        .try_into()
                                        .expect("Call-like multiplication must be by a computable");
                                    computable * param
                                }
                                2 => {
                                    let [x, y] = p_vals.try_into().unwrap();
                                    let [x, y] = [x, y].map(|v| v.try_into().unwrap());
                                    let points: Computable =
                                        pervasive_apply_comp_known(id, [x, y], |[x, y]| {
                                            let (CompPrim::Number(x), CompPrim::Number(y)) = (x, y)
                                            else {
                                                unreachable!(
                                                    "Point must have number as coordinates"
                                                )
                                            };
                                            Primitive::Computable(CompPrim::Point(Point(x, y)))
                                        })
                                        .try_into()
                                        .unwrap();
                                    computable * points
                                }
                                _ => unreachable!("Cannot call a variable that is not a function"),
                            }
                            .into()
                        } else {
                            let Function { params, expr } = functions.get_function(func);

                            if params.len() != p_vals.len() {
                                unreachable!("Parameter count does not match function definition")
                            }

                            // println!("Calling {func:?}");
                            // println!("With parameters: {p_vals:?}");

                            let old_vals: Vec<_> = params
                                .iter()
                                .zip(p_vals)
                                .map(|(&id, val)| context.set_value(id, val))
                                .collect();

                            let v = expr.evaluate(functions, context);

                            // println!("Returned: {v:?}");

                            for (&id, old_val) in params.iter().zip(old_vals) {
                                context.un_or_set(id, old_val)
                            }

                            v
                        }
                    }
                    Ident::Builtin(b) => b.evaluate(
                        id,
                        p_vals,
                        power.as_ref().map(|v| v.evaluate(functions, context)),
                    ),
                }
            }
            EvalKind::Power { base, power } => {
                let [base, power]: [Result<Computable, _>; 2] =
                    [base, power].map(|i| i.evaluate(functions, context).try_into());
                let (Ok(base), Ok(power)) = (base, power) else {
                    unreachable!("Power must use computable");
                };

                pervasive_apply_comp_known(id, [base, power], |[base, power]| match (base, power) {
                    (CompPrim::Number(base), CompPrim::Number(power)) => {
                        // if power.fract() == 0.0 {
                        //     Primitive::number(base.powi(power as i32))
                        // } else {
                        Primitive::number(base.powf(power))
                        // }
                    }
                    _ => unreachable!("Power must be two numbers"),
                })
            }
            EvalKind::IntPower { base, power } => {
                let base: Result<Computable, _> = base.evaluate(functions, context).try_into();
                let Ok(base) = base else {
                    unreachable!("Power must use computable");
                };
                pervasive_apply_comp_known(id, [base], |[k]| {
                    let CompPrim::Number(x) = k else {
                        unreachable!("Power base must be a number");
                    };

                    Primitive::number(x.powi(*power))
                })
            }
            EvalKind::ListIndexing { list, index } => {
                let index: Computable = index
                    .evaluate(functions, context)
                    .try_into()
                    .expect("Indexing must be a number");

                let list = list.evaluate(functions, context);

                match list {
                    VarValue::Prim(_) => unreachable!("Indexing can only index lists"),
                    VarValue::List(list) => match index {
                        Computable::Prim(CompPrim::Number(x)) => {
                            VarValue::Prim(list.get_cloned((x.floor() as usize).wrapping_sub(1)))
                        }
                        Computable::List(CompList::Number(xs)) => VarValue::List(
                            xs.map_dif(&POOL_PRIMITIVE, |v| {
                                let one_index = v.floor() as usize;
                                list.get_cloned(one_index.wrapping_sub(1))
                            })
                            .into(),
                        ),
                        _ => unreachable!("Indexing must be a number"),
                    },
                }
            }
            EvalKind::ListFiltering { list, filter } => {
                let VarValue::List(list) = list.evaluate(functions, context) else {
                    unreachable!("List filtering can only applied to lists");
                };
                let filter: Computable = filter.evaluate(functions, context).into();

                let length = filter.len().unwrap_or(usize::MAX).min(list.len());

                let vec = PooledVec::from_iter(
                    &POOL_PRIMITIVE,
                    id,
                    (0..length).filter_map(|i| {
                        let filter_at = filter.get(i);
                        (filter_at == CompPrim::Number(1.0)).then(|| list.get_cloned(i))
                    }),
                );

                VarValue::List(vec.into())
            }
            EvalKind::ElementAccess { expr, element } => {
                let expr = expr.evaluate(functions, context);

                pervasive_apply_comp_known(
                    id,
                    [expr.try_into().expect("Elem access can only be computable")],
                    |[x]| match x {
                        CompPrim::Number(_) => unreachable!("Elem access must be point"),
                        CompPrim::Point(p) => Primitive::number(match element {
                            Element::X => p.0,
                            Element::Y => p.1,
                        }),
                    },
                )
            }
            EvalKind::Multiply(xs) => {
                let vals: Vec<_> = xs.iter().map(|e| e.evaluate(functions, context)).collect();
                pervasive_apply_variadic(id, vals, |xs| {
                    let comps = xs.map(|v| -> CompPrim { v.try_into().unwrap() });
                    let prod: CompPrim = comps.into_iter().product();
                    Primitive::Computable(prod)
                })
            }
            EvalKind::SumProd {
                expr,
                counter,
                from,
                kind,
                to,
            } => {
                let counter = *counter;

                let from = from.evaluate(functions, context);
                let to = to.evaluate(functions, context);

                pervasive_apply_non_prim_known(id, [from, to], |pieces| {
                    let [from, to]: [Result<f64, _>; 2] = pieces.map(|i| i.try_into());
                    let (Ok(from), Ok(to)) = (from, to) else {
                        unreachable!("SumProd must take number as bounds");
                    };
                    let [from, to] = [from, to].map(|i| i.round());

                    let mut result = None;

                    let mut current_counter = from;
                    let old_val = OnceCell::new();
                    while current_counter <= to {
                        let old = context.set_value(counter, VarValue::number(current_counter));
                        let _ = old_val.set(old);

                        let vv = expr.evaluate(functions, context);
                        // println!("{vv:?}");

                        let value: Computable =
                            vv.try_into().expect("Cannot SumProd noncomputable");

                        result = match kind {
                            SumOrProduct::Sum => Some(if let Some(x) = result {
                                x + value
                            } else {
                                value
                            }),
                            SumOrProduct::Product => Some(if let Some(x) = result {
                                x * value
                            } else {
                                value
                            }),
                        };

                        current_counter += 1.0;
                    }

                    context.un_or_set(counter, old_val.into_inner().flatten());

                    result.unwrap_or(Computable::number(0.0)).into()
                })
            }
            EvalKind::AddSub { exprs, kinds } => {
                debug_assert_eq!(exprs.len(), kinds.len());

                exprs
                    .iter()
                    .map(|v| {
                        v.evaluate(functions, context)
                            .try_into()
                            .expect("Can only add/sub computables.")
                    })
                    .zip(kinds)
                    .map(|(v, k): (Computable, _)| match k {
                        AddOrSub::Add => v,
                        AddOrSub::Sub => -v,
                    })
                    .sum::<Computable>()
                    .into()
            }
            EvalKind::IfElse { cond, yes, no } => {
                let cond = cond.evaluate(functions, context);

                fn default_of(mut yes: VarValue) -> VarValue {
                    match &mut yes {
                        VarValue::Prim(Primitive::Computable(CompPrim::Number(_))) => {
                            VarValue::number(f64::NAN)
                        }
                        VarValue::Prim(Primitive::Computable(CompPrim::Point(_))) => {
                            VarValue::point(Point(f64::NAN, f64::NAN))
                        }
                        VarValue::Prim(Primitive::NonComputable(NonCompPrim::Color(_))) => {
                            VarValue::color(Color(0, 0, 0))
                        }
                        VarValue::Prim(Primitive::NonComputable(NonCompPrim::Polygon(_))) => {
                            VarValue::polygon(Polygon(Vec::new()))
                        }
                        VarValue::List(x) => {
                            match x {
                                PrimList::Computable(CompList::Number(x)) => x.clear(),
                                PrimList::Computable(CompList::Point(x)) => x.clear(),
                                PrimList::NonComputable(NonCompList::Color(x)) => x.clear(),
                                PrimList::NonComputable(NonCompList::Polygon(x)) => x.clear(),
                            }
                            yes
                        }
                    }
                }

                match cond {
                    CondOutput::Prim(cond) => {
                        if cond {
                            yes.evaluate(functions, context)
                        } else {
                            match no.as_ref() {
                                None => default_of(yes.evaluate(functions, context)),
                                Some(no) => no.evaluate(functions, context),
                            }
                        }
                    }
                    CondOutput::List(bools) => {
                        let yes = yes.evaluate(functions, context);
                        let no = no.as_ref().map_or_else(
                            || default_of(yes.clone()),
                            |no| no.evaluate(functions, context),
                        );
                        let x = PooledVec::from_iter(
                            &POOL_PRIMITIVE,
                            id,
                            bools.iter().copied().enumerate().map(|(i, b)| {
                                match b {
                                    true => &yes,
                                    false => &no,
                                }
                                .get_cloned(i)
                            }),
                        );

                        VarValue::List(x.into())
                    }
                }
            }
        }
    }
}
