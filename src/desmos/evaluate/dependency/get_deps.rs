use crate::desmos::evaluate::{
    Conditional, EvalExpr, EvalKind, EvalTree, Ident, UserIdent, VarDef,
};
use crate::desmos::execute::actions::{ActExpr, ActIdent, ActTree};
use crate::vecs::{iter_concat, vec_concat};

pub trait CanDepend {
    fn get_deps(&self) -> Vec<usize>;
}

impl CanDepend for EvalExpr {
    fn get_deps(&self) -> Vec<usize> {
        self.expr.kind.get_deps()
    }
}

impl<T: CanDepend> CanDepend for Vec<T> {
    fn get_deps(&self) -> Vec<usize> {
        self.iter().flat_map(CanDepend::get_deps).collect()
    }
}

impl CanDepend for VarDef {
    fn get_deps(&self) -> Vec<usize> {
        self.expr.get_deps()
    }
}

impl CanDepend for Conditional {
    fn get_deps(&self) -> Vec<usize> {
        match self {
            Conditional::Inequality {
                id: _,
                exprs,
                comp: _,
            } => exprs,
            Conditional::Equality { id: _, exprs } => exprs,
        }
        .get_deps()
    }
}

impl CanDepend for Ident {
    fn get_deps(&self) -> Vec<usize> {
        match self {
            &Ident::User(x) => vec![x.0],
            Ident::Builtin(_) => vec![],
        }
    }
}
impl CanDepend for UserIdent {
    fn get_deps(&self) -> Vec<usize> {
        vec![self.0]
    }
}

impl CanDepend for EvalTree {
    fn get_deps(&self) -> Vec<usize> {
        self.kind.get_deps()
    }
}

impl CanDepend for EvalKind {
    fn get_deps(&self) -> Vec<usize> {
        match &self {
            EvalKind::Ident(i) => i.get_deps(),
            EvalKind::Number(_) => vec![],
            EvalKind::Point { x, y } => vec_concat(x.get_deps(), y.get_deps()),
            EvalKind::List(l) => l.get_deps(),
            EvalKind::ListRange { before, after } => {
                vec_concat(before.get_deps(), after.get_deps())
            }
            EvalKind::ListComprehension { defs, expr } => {
                let mut deps = expr.get_deps();
                for def in defs {
                    while let Some(i) = deps.iter().position(|&v| v == def.var.0) {
                        deps.swap_remove(i);
                    }
                }

                vec_concat(deps, defs.get_deps())
            }
            EvalKind::AbsoluteValue(x) => x.get_deps(),
            EvalKind::Fraction { top, bottom } => vec_concat(top.get_deps(), bottom.get_deps()),
            EvalKind::Root { expr, nth: _ } => expr.get_deps(),
            EvalKind::FunctionCall {
                func,
                power,
                params,
                prime_count: _,
            } => vec_concat(
                iter_concat(
                    params.get_deps(),
                    power
                        .iter()
                        .map(CanDepend::get_deps)
                        .flat_map(|v| v.into_iter()),
                ),
                func.get_deps(),
            ),
            EvalKind::Power { base, power } => vec_concat(base.get_deps(), power.get_deps()),
            EvalKind::IntPower { base, power: _ } => base.get_deps(),
            EvalKind::ListIndexing { list, index } => vec_concat(list.get_deps(), index.get_deps()),
            EvalKind::ListFiltering { list, filter } => {
                vec_concat(list.get_deps(), filter.get_deps())
            }
            EvalKind::ElementAccess { expr, element: _ } => expr.get_deps(),
            EvalKind::Multiply(exprs) => exprs.get_deps(),
            EvalKind::SumProd {
                expr,
                from,
                to,
                kind: _,
                counter,
            } => {
                let counter = counter.0;
                let mut deps = expr.get_deps();
                while let Some(x) = deps.iter().position(|&v| v == counter) {
                    deps.swap_remove(x);
                }

                vec_concat(vec_concat(deps, from.get_deps()), to.get_deps())
            }
            EvalKind::AddSub { exprs, kinds: _ } => exprs.get_deps(),
            EvalKind::IfElse { cond, yes, no } => vec_concat(
                vec_concat(cond.get_deps(), yes.get_deps()),
                no.iter().flat_map(|x| x.get_deps()).collect(),
            ),
        }
    }
}

impl CanDepend for ActExpr {
    fn get_deps(&self) -> Vec<usize> {
        self.0.get_deps()
    }
}

impl CanDepend for ActTree {
    fn get_deps(&self) -> Vec<usize> {
        match self {
            ActTree::Many(xs) => xs.get_deps(),
            ActTree::IfElse { cond: _, yes, no } => vec_concat(yes.get_deps(), no.get_deps()),
            ActTree::Ident(id) => id.get_deps(),
            ActTree::Raw { ident: _, expr: _ } => vec![],
            ActTree::Call { ident, params: _ } => ident.get_deps(),
        }
    }
}

impl CanDepend for ActIdent {
    fn get_deps(&self) -> Vec<usize> {
        vec![self.0]
    }
}
