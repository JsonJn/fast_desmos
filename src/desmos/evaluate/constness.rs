use crate::desmos::evaluate::{Builtins, Conditional, EvalExpr, EvalKind, EvalTree, Ident, VarDef};
use crate::vecs::{iter_concat, vec_concat};

/// The only purpose of this system's existence is `random`
///
/// The `CanDepend` system works well for dependencies,
/// and as such is used for dependency analysis in evaluation
/// ordering.
///
/// It's also used to evaluate whether an `EvalExpr` can be
/// cached. This breaks on `random` because `random` has no
/// dependencies in evaluation but requires re-evaluation
/// every time.

pub trait CanConst {
    fn get_const_deps(&self) -> Vec<usize>;
}

impl CanConst for EvalExpr {
    fn get_const_deps(&self) -> Vec<usize> {
        self.expr.kind.get_const_deps()
    }
}

impl CanConst for EvalTree {
    fn get_const_deps(&self) -> Vec<usize> {
        self.kind.get_const_deps()
    }
}

impl<T: CanConst> CanConst for Vec<T> {
    fn get_const_deps(&self) -> Vec<usize> {
        self.iter().flat_map(CanConst::get_const_deps).collect()
    }
}

impl CanConst for VarDef {
    fn get_const_deps(&self) -> Vec<usize> {
        self.expr.get_const_deps()
    }
}

impl CanConst for Ident {
    fn get_const_deps(&self) -> Vec<usize> {
        match self {
            Ident::User(id) => vec![id.0],
            // The literal only difference.
            Ident::Builtin(Builtins::Random) => vec![usize::MAX],
            Ident::Builtin(_) => vec![],
        }
    }
}

impl CanConst for Conditional {
    fn get_const_deps(&self) -> Vec<usize> {
        match self {
            Conditional::Inequality {
                id: _,
                exprs,
                comp: _,
            } => exprs.get_const_deps(),
            Conditional::Equality { id: _, exprs } => exprs.get_const_deps(),
        }
    }
}

impl CanConst for EvalKind {
    fn get_const_deps(&self) -> Vec<usize> {
        match &self {
            EvalKind::Ident(i) => vec![i.0],
            EvalKind::Number(_) => vec![],
            EvalKind::Point { x, y } => vec_concat(x.get_const_deps(), y.get_const_deps()),
            EvalKind::List(l) => l.get_const_deps(),
            EvalKind::ListRange { before, after } => {
                vec_concat(before.get_const_deps(), after.get_const_deps())
            }
            EvalKind::ListComprehension { defs, expr } => {
                let mut deps = expr.get_const_deps();
                for def in defs {
                    if let Some(i) = deps.iter().position(|&v| v == def.var.0) {
                        deps.swap_remove(i);
                    }
                }

                vec_concat(deps, defs.get_const_deps())
            }
            EvalKind::AbsoluteValue(x) => x.get_const_deps(),
            EvalKind::Fraction { top, bottom } => {
                vec_concat(top.get_const_deps(), bottom.get_const_deps())
            }
            EvalKind::Root { expr, nth: _ } => expr.get_const_deps(),
            EvalKind::FunctionCall {
                func,
                power,
                params,
                prime_count: _,
            } => vec_concat(
                iter_concat(
                    params.get_const_deps(),
                    power
                        .iter()
                        .map(CanConst::get_const_deps)
                        .flat_map(|v| v.into_iter()),
                ),
                func.get_const_deps(),
            ),
            EvalKind::Power { base, power } => {
                vec_concat(base.get_const_deps(), power.get_const_deps())
            }
            EvalKind::IntPower { base, power: _ } => base.get_const_deps(),
            EvalKind::ListIndexing { list, index } => {
                vec_concat(list.get_const_deps(), index.get_const_deps())
            }
            EvalKind::ListFiltering { list, filter } => {
                vec_concat(list.get_const_deps(), filter.get_const_deps())
            }
            EvalKind::ElementAccess { expr, element: _ } => expr.get_const_deps(),
            EvalKind::Multiply(exprs) => exprs.get_const_deps(),
            EvalKind::Differentiate(_) => todo!("Won't implement before evaluation is done"),
            EvalKind::SumProd {
                expr,
                from,
                to,
                kind: _,
                counter,
            } => {
                let counter = counter.0;
                let mut deps = expr.get_const_deps();
                while let Some(x) = deps.iter().position(|&v| v == counter) {
                    deps.swap_remove(x);
                }

                vec_concat(vec_concat(deps, from.get_const_deps()), to.get_const_deps())
            }
            EvalKind::AddSub { exprs, kinds: _ } => exprs.get_const_deps(),
            EvalKind::IfElse { cond, yes, no } => vec_concat(
                vec_concat(cond.get_const_deps(), yes.get_const_deps()),
                no.get_const_deps(),
            ),
        }
    }
}
