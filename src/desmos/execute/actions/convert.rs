use once_cell::sync::Lazy;

use super::tree::{ActExpr, ActTree};
use crate::desmos::evaluate::{ToEval, IDENTIFIERS};
use crate::desmos::parsing;
use identifiers::ActIdentifierStorer;

mod identifiers;

pub static ACT_IDENTS: Lazy<ActIdentifierStorer> = Lazy::new(ActIdentifierStorer::new);

pub trait ToActExpr {
    fn to_act_expr(self) -> ActExpr;
}

impl ToActExpr for parsing::ActExpr {
    fn to_act_expr(self) -> ActExpr {
        ActExpr::new(ActTree::Many(
            self.actions.into_iter().map(|v| v.to_act_expr()).collect(),
        ))
    }
}

impl ToActExpr for parsing::Action {
    fn to_act_expr(self) -> ActExpr {
        match self {
            Self::Raw(x) => x.to_act_expr(),
            Self::IfElse(if_else) => if_else.to_act_expr(),
            Self::Grouping(parsing::ActGrouping { expr }) => expr.to_act_expr(),
            Self::Ident(id) => ActExpr::new(ActTree::Ident(ACT_IDENTS.name_to_int(&id.0))),
            Self::Call(call) => call.to_act_expr(),
        }
    }
}

impl ToActExpr for parsing::ActFuncCall {
    fn to_act_expr(self) -> ActExpr {
        let Self { func, params } = self;
        ActExpr::new(ActTree::Call {
            ident: ACT_IDENTS.name_to_int(&func.0),
            params: params.into_iter().map(|v| v.to_eval()).collect(),
        })
    }
}

impl ToActExpr for parsing::RawAction {
    fn to_act_expr(self) -> ActExpr {
        let parsing::RawAction { ident, expr } = self;

        ActExpr::new(ActTree::Raw {
            ident: IDENTIFIERS.name_to_int(&ident.0),
            expr: expr.to_eval(),
        })
    }
}

impl ToActExpr for parsing::ActIfElse {
    fn to_act_expr(self) -> ActExpr {
        let parsing::ActIfElse { cond, yes, no } = self;
        ActExpr::new(ActTree::IfElse {
            cond: cond.into(),
            yes: yes.to_act_expr(),
            no: no
                .map(ToActExpr::to_act_expr)
                .unwrap_or(ActExpr::new(ActTree::Many(vec![]))),
        })
    }
}
