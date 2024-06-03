use crate::desmos::evaluate::{Builtins, Conditional, EvalExpr, EvalKind, EvalTree, Ident, VarDef};
use crate::vecs::{iter_concat, vec_concat};

pub trait CanConst {
    fn is_const(&self) -> bool;
    fn get_const_deps(&self) -> Vec<usize>;
}

impl CanConst for EvalExpr {
    fn is_const(&self) -> bool {
        self.expr.kind.is_const()
    }

    fn get_const_deps(&self) -> Vec<usize> {
        self.expr.kind.get_const_deps()
    }
}

impl CanConst for EvalTree {
    fn is_const(&self) -> bool {
        self.kind.is_const()
    }

    fn get_const_deps(&self) -> Vec<usize> {
        self.kind.get_const_deps()
    }
}

impl<T: CanConst> CanConst for Vec<T> {
    fn is_const(&self) -> bool {
        self.iter().all(|i| i.is_const())
    }

    fn get_const_deps(&self) -> Vec<usize> {
        self.iter().flat_map(CanConst::get_const_deps).collect()
    }
}

impl CanConst for VarDef {
    fn is_const(&self) -> bool {
        self.expr.is_const()
    }

    fn get_const_deps(&self) -> Vec<usize> {
        self.expr.get_const_deps()
    }
}

impl CanConst for Ident {
    fn is_const(&self) -> bool {
        match self {
            Ident::User(_) => false,
            // The literal only difference.
            Ident::Builtin(Builtins::Random) => false,
            Ident::Builtin(_) => true,
        }
    }

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
    fn is_const(&self) -> bool {
        match self {
            Conditional::Inequality {
                id: _,
                exprs,
                comp: _,
            } => exprs.is_const(),
            Conditional::Equality { id: _, exprs } => exprs.is_const(),
        }
    }

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
    fn is_const(&self) -> bool {
        match self {
            EvalKind::Ident(_) => false,
            EvalKind::Number(_) => true,
            EvalKind::Point { x, y } => x.is_const() && y.is_const(),
            EvalKind::List(xs) => xs.is_const(),
            EvalKind::ListRange { before, after } => before.is_const() && after.is_const(),
            EvalKind::ListComprehension { defs, expr } => {
                defs.is_const() && {
                    let defined: Vec<_> = defs.iter().map(|x| x.var.0).collect();
                    expr.get_const_deps()
                        .into_iter()
                        .all(|x| defined.contains(&x))
                }
            }
            EvalKind::AbsoluteValue(x) => x.is_const(),
            EvalKind::Fraction { top, bottom } => top.is_const() && bottom.is_const(),
            EvalKind::Root { nth: _, expr } => expr.is_const(),
            EvalKind::FunctionCall {
                func,
                prime_count: _,
                power: _,
                params,
            } => func.is_const() && params.is_const(),
            EvalKind::Power { base, power } => base.is_const() && power.is_const(),
            EvalKind::IntPower { base, power: _ } => base.is_const(),
            EvalKind::ListIndexing { list, index } => list.is_const() && index.is_const(),
            EvalKind::ListFiltering { list, filter } => list.is_const() && filter.is_const(),
            EvalKind::ElementAccess { expr, element: _ } => expr.is_const(),
            EvalKind::Multiply(mult) => mult.is_const(),
            EvalKind::SumProd {
                expr,
                from,
                to,
                counter,
                kind: _,
            } => {
                from.is_const() && to.is_const() && {
                    expr.get_const_deps().iter().all(|&dep| dep == counter.0)
                }
            }
            EvalKind::AddSub { kinds: _, exprs } => exprs.is_const(),
            EvalKind::IfElse { cond, yes, no } => {
                cond.is_const()
                    && yes.is_const()
                    && match no {
                        None => true,
                        Some(x) => x.is_const(),
                    }
            }
        }
    }

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
                no.iter().flat_map(|x| x.get_const_deps()).collect(),
            ),
        }
    }
}
