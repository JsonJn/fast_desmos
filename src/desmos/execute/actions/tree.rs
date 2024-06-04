use crate::desmos::evaluate::{Conditional, EvalExpr, UserIdent};
use crate::vec_map::AsIndex;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct ActIdent(pub usize);

impl AsIndex for ActIdent {
    fn as_index(&self) -> usize {
        self.0
    }
}

impl From<ActIdent> for usize {
    fn from(value: ActIdent) -> Self {
        value.0
    }
}

#[derive(Debug)]
pub struct ActExpr(pub Box<ActTree>);

#[derive(Debug)]
pub enum ActTree {
    Many(Vec<ActExpr>),
    IfElse {
        cond: Conditional,
        yes: ActExpr,
        no: ActExpr,
    },
    Ident(ActIdent),
    Raw {
        ident: UserIdent,
        expr: EvalExpr,
    },
    Call {
        ident: ActIdent,
        params: Vec<EvalExpr>,
    },
}

impl ActExpr {
    pub fn new(tree: ActTree) -> Self {
        Self(Box::new(tree))
    }
}
