pub use value::*;

use crate::desmos::evaluate::constness::CanConst;
use crate::desmos::evaluate::{Conditional, EvalExpr, EvalKind};
use crate::desmos::parsing::AddOrSub;

mod value;

pub trait CanLinear {
    fn is_linear(&self) -> LinType;
    fn as_linear(&self) -> LinearVal;
}

impl CanLinear for EvalExpr {
    fn is_linear(&self) -> LinType {
        self.expr.kind.is_linear()
    }

    fn as_linear(&self) -> LinearVal {
        self.expr.kind.as_linear()
    }
}

fn all_linear<T: CanLinear>(vec: &[T]) -> LinType {
    vec.iter()
        .fold(LinType::Constant, |acc, t| acc + t.is_linear())
}

fn one_linear_else_const<T: CanLinear>(vec: &[T]) -> LinType {
    let mut lin_type = LinType::Constant;
    for expr in vec {
        let linear = expr.is_linear();
        match linear {
            LinType::NonLinear => return LinType::NonLinear,
            LinType::Constant => {}
            LinType::LinearIn(_) => match lin_type {
                LinType::Constant => lin_type = linear,
                LinType::LinearIn(_) => return LinType::NonLinear,
                LinType::NonLinear => unreachable!("`lin_type` cannot get set to this value."),
            },
        }
    }
    lin_type
}

impl CanLinear for EvalKind {
    fn is_linear(&self) -> LinType {
        match self {
            &EvalKind::Ident(ident) => LinType::LinearIn(ident),
            EvalKind::Number(_) => LinType::Constant,
            EvalKind::Point { .. } => LinType::NonLinear,
            EvalKind::List(_) => LinType::NonLinear,
            EvalKind::ListRange { .. } => LinType::NonLinear,
            EvalKind::ListComprehension { .. } => LinType::NonLinear,
            EvalKind::AbsoluteValue(_) => LinType::NonLinear,
            EvalKind::Fraction { top, bottom } => top.is_linear() & bottom.is_const(),
            EvalKind::Root { .. } => LinType::NonLinear,
            EvalKind::FunctionCall { .. } => LinType::NonLinear,
            EvalKind::Power { .. } => LinType::NonLinear,
            EvalKind::IntPower { power, base } => (*power == 1) & base.is_linear(),
            EvalKind::ListIndexing { .. } => LinType::NonLinear,
            EvalKind::ListFiltering { .. } => LinType::NonLinear,
            EvalKind::ElementAccess { .. } => LinType::NonLinear,
            EvalKind::Multiply(x) => one_linear_else_const(x),
            EvalKind::SumProd { .. } => LinType::NonLinear,
            EvalKind::AddSub { exprs, kinds: _ } => all_linear(exprs),
            EvalKind::IfElse { cond, yes, no } => {
                cond.is_const()
                    & yes.is_linear()
                        + no.as_ref()
                            .map_or_else(|| LinType::NonLinear, |x| x.is_linear())
            }
        }
    }

    fn as_linear(&self) -> LinearVal {
        macro_rules! not_linear {
            () => {
                unreachable!("Not a linear item.")
            };
        }

        macro_rules! not_const {
            () => {
                unreachable!("Not a const item.")
            };
        }

        fn expr_const_eval(eval: &EvalExpr) -> f64 {
            const_evaluate(&eval.expr.kind)
        }

        fn const_evaluate(eval: &EvalKind) -> f64 {
            match eval {
                &EvalKind::Number(x) => x,
                EvalKind::AddSub { kinds, exprs } => kinds
                    .iter()
                    .zip(exprs.iter())
                    .map(|(kind, expr)| {
                        let x = expr_const_eval(expr);
                        match kind {
                            AddOrSub::Add => x,
                            AddOrSub::Sub => -x,
                        }
                    })
                    .sum(),
                EvalKind::AbsoluteValue(x) => expr_const_eval(x).abs(),
                EvalKind::Fraction { top, bottom } => {
                    expr_const_eval(top) / expr_const_eval(bottom)
                }
                EvalKind::Root { nth, expr } => expr_const_eval(expr).powf(*nth),
                EvalKind::Power { power, base } => {
                    expr_const_eval(base).powf(expr_const_eval(power))
                }
                EvalKind::IntPower { power, base } => expr_const_eval(base).powi(*power),
                EvalKind::Multiply(exprs) => {
                    exprs.iter().map(|expr| expr_const_eval(expr)).product()
                }
                _ => not_const!(),
            }
        }

        fn cond_evaluate(cond: &Conditional) -> bool {
            match cond {
                Conditional::Inequality { id: _, exprs, comp } => {
                    let values: Vec<_> = exprs.iter().map(expr_const_eval).collect();
                    for (vals, comp) in values.windows(2).zip(comp.iter()) {
                        let [x, y] = [0, 1].map(|i| vals[i]);
                        if !x.partial_cmp(&y).is_some_and(|x| comp.matches(x)) {
                            return false;
                        }
                    }
                    true
                }
                Conditional::Equality { exprs, id: _ } => {
                    let values: Vec<_> = exprs.iter().map(expr_const_eval).collect();
                    for vals in values.windows(2) {
                        let [x, y] = [0, 1].map(|i| vals[i]);
                        if x != y {
                            return false;
                        }
                    }
                    true
                }
            }
        }

        match self {
            &EvalKind::Ident(x) => LinearVal::linear(x, 1.0, 0.0),
            &EvalKind::Number(value) => LinearVal::constant(value),
            EvalKind::Point { .. } => not_linear!(),
            EvalKind::List(_) => not_linear!(),
            EvalKind::ListRange { .. } => not_linear!(),
            EvalKind::ListComprehension { .. } => not_linear!(),
            EvalKind::AbsoluteValue(_) => not_linear!(),
            EvalKind::Fraction { top, bottom } => top.as_linear() / bottom.as_linear(),
            EvalKind::Root { .. } => not_linear!(),
            EvalKind::FunctionCall { .. } => not_linear!(),
            EvalKind::Power { .. } => not_linear!(),
            EvalKind::IntPower { base, power: _ } => base.as_linear(),
            EvalKind::ListIndexing { .. } => not_linear!(),
            EvalKind::ListFiltering { .. } => not_linear!(),
            EvalKind::ElementAccess { .. } => not_linear!(),
            EvalKind::Multiply(exprs) => exprs
                .iter()
                .map(|expr| {
                    let kind = expr.is_linear();
                    match kind {
                        LinType::Constant => LinearVal::constant(expr_const_eval(expr)),
                        LinType::LinearIn(_) => expr.as_linear(),
                        LinType::NonLinear => not_linear!(),
                    }
                })
                .product(),
            EvalKind::SumProd { .. } => not_linear!(),
            EvalKind::AddSub { exprs, kinds } => exprs
                .iter()
                .zip(kinds.iter())
                .map(|(expr, kind)| {
                    let linear = expr.as_linear();
                    match kind {
                        AddOrSub::Add => linear,
                        AddOrSub::Sub => -linear,
                    }
                })
                .sum(),
            EvalKind::IfElse { cond, yes, no } => {
                if cond_evaluate(cond) {
                    yes.as_linear()
                } else {
                    no.as_ref().unwrap().as_linear()
                }
            }
        }
    }
}
