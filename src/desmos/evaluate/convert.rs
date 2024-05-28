use once_cell::sync::Lazy;

pub use identifiers::IdentifierStorer;

use crate::desmos::evaluate::{tree, EvalKind};
use crate::desmos::parsing::{
    AbsoluteValue, AddSub, Differentiate, ElementAccess, Equality, Everything, EverythingElse,
    Expression, Fraction, FunctionCall, Grouping, IfElse, IfElseBranches, Inequality,
    ListComprehension, ListContents, ListFiltering, ListIndexing, ListLiteral, ListRange, Multiply,
    MultiplyOrBelow, Number, Point, PostfixOrBelow, Power, Root, SumProd, VariableDef,
};

use super::super::parsing;
use super::tree::{EvalExpr, VarDef};

mod identifiers;

pub static IDENTIFIERS: Lazy<IdentifierStorer> = Lazy::new(IdentifierStorer::new);

pub trait ToEval {
    fn to_eval(self) -> EvalExpr;
}

impl ToEval for Expression {
    fn to_eval(self) -> EvalExpr {
        self.expr.to_eval()
    }
}

impl ToEval for Everything {
    fn to_eval(self) -> EvalExpr {
        match self {
            Everything::AddSub(AddSub { exprs, types }) => {
                EvalExpr::new_generated(EvalKind::AddSub {
                    exprs: exprs.into_iter().map(|v| v.to_eval()).collect(),
                    kinds: types,
                })
            }
            Everything::Below(x) => x.to_eval(),
        }
    }
}

impl ToEval for MultiplyOrBelow {
    fn to_eval(self) -> EvalExpr {
        match self {
            MultiplyOrBelow::Below(x) => x.to_eval(),
            MultiplyOrBelow::Multiply(Multiply { exprs }) => EvalExpr::new_generated(
                EvalKind::Multiply(exprs.into_iter().map(|v| v.to_eval()).collect()),
            ),
        }
    }
}

impl ToEval for PostfixOrBelow {
    fn to_eval(self) -> EvalExpr {
        match self {
            PostfixOrBelow::Below(x) => x.to_eval(),
            PostfixOrBelow::Power(Power { power, base }) => {
                EvalExpr::new_generated(EvalKind::Power {
                    base: base.to_eval(),
                    power: power.to_eval(),
                })
            }
            PostfixOrBelow::Indexing(ListIndexing { list, index }) => {
                EvalExpr::new_generated(EvalKind::ListIndexing {
                    list: list.to_eval(),
                    index: index.to_eval(),
                })
            }
            PostfixOrBelow::Filtering(ListFiltering { filter, list }) => {
                EvalExpr::new_generated(EvalKind::ListFiltering {
                    filter: convert_cond(filter),
                    list: list.to_eval(),
                })
            }
            PostfixOrBelow::Element(ElementAccess { expr, element }) => {
                EvalExpr::new_generated(EvalKind::ElementAccess {
                    element,
                    expr: expr.to_eval(),
                })
            }
        }
    }
}

impl From<parsing::Conditional> for tree::Conditional {
    fn from(value: parsing::Conditional) -> Self {
        convert_cond(value)
    }
}

fn convert_cond(filter: parsing::Conditional) -> tree::Conditional {
    match filter {
        parsing::Conditional::Equality(Equality { exprs }) => tree::Conditional::Equality {
            exprs: exprs.into_iter().map(ToEval::to_eval).collect(),
        },
        parsing::Conditional::Inequality(Inequality { exprs, kinds }) => {
            tree::Conditional::Inequality {
                comp: kinds,
                exprs: exprs.into_iter().map(ToEval::to_eval).collect(),
            }
        }
    }
}

impl ToEval for EverythingElse {
    fn to_eval(self) -> EvalExpr {
        match self {
            EverythingElse::Grouping(Grouping { expr }) => expr.to_eval(),
            EverythingElse::Point(Point { x, y }) => EvalExpr::new_generated(EvalKind::Point {
                x: x.to_eval(),
                y: y.to_eval(),
            }),
            EverythingElse::List(l) => EvalExpr::new_generated(match l {
                ListContents::Literal(ListLiteral { parts }) => {
                    EvalKind::List(parts.into_iter().map(ToEval::to_eval).collect())
                }
                ListContents::Range(ListRange { before, after }) => EvalKind::ListRange {
                    before: before.into_iter().map(ToEval::to_eval).collect(),
                    after: after.into_iter().map(ToEval::to_eval).collect(),
                },
                ListContents::Comprehension(ListComprehension { expr, statements }) => {
                    EvalKind::ListComprehension {
                        expr: expr.to_eval(),
                        defs: statements
                            .into_iter()
                            .map(|VariableDef { ident, expr }| VarDef {
                                var: IDENTIFIERS.name_to_int(&ident.0),
                                expr: expr.to_eval(),
                            })
                            .collect(),
                    }
                }
            }),
            EverythingElse::IfElse(IfElse { cond, branches }) => {
                let (yes, no) = if let Some(IfElseBranches { yes, no }) = branches {
                    (
                        yes.to_eval(),
                        no.map_or_else(
                            || EvalExpr::new_generated(EvalKind::Number(f64::NAN)),
                            ToEval::to_eval,
                        ),
                    )
                } else {
                    (
                        EvalExpr::new_generated(EvalKind::Number(1.0)),
                        EvalExpr::new_generated(EvalKind::Number(f64::NAN)),
                    )
                };

                EvalExpr::new_generated(EvalKind::IfElse {
                    cond: convert_cond(cond),
                    yes,
                    no,
                })
            }
            EverythingElse::Abs(AbsoluteValue { expr }) => {
                EvalExpr::new_generated(EvalKind::AbsoluteValue(expr.to_eval()))
            }
            EverythingElse::Fraction(Fraction { top, bottom }) => {
                EvalExpr::new_generated(EvalKind::Fraction {
                    top: top.to_eval(),
                    bottom: bottom.to_eval(),
                })
            }
            EverythingElse::Root(Root { nth, expr }) => EvalExpr::new_generated(EvalKind::Root {
                nth,
                expr: expr.to_eval(),
            }),
            EverythingElse::Call(FunctionCall {
                func,
                prime_count,
                params,
                power,
            }) => EvalExpr::new_generated(EvalKind::FunctionCall {
                func: IDENTIFIERS.convert_ident(func),
                prime_count,
                params: params.into_iter().map(ToEval::to_eval).collect(),
                power: power.map(ToEval::to_eval),
            }),
            EverythingElse::Number(Number(num)) => EvalExpr::new_generated(EvalKind::Number(num)),
            EverythingElse::Ident(id) => {
                EvalExpr::new_generated(EvalKind::Ident(IDENTIFIERS.name_to_int(&id.0)))
            }
            EverythingElse::Differentiate(Differentiate { expr }) => {
                EvalExpr::new_generated(EvalKind::Differentiate(expr.to_eval()))
            }
            EverythingElse::SumProd(SumProd {
                expr,
                kind,
                counter,
                from,
                to,
            }) => EvalExpr::new_generated(EvalKind::SumProd {
                expr: expr.to_eval(),
                kind,
                counter: IDENTIFIERS.name_to_int(&counter.0),
                from: from.to_eval(),
                to: to.to_eval(),
            }),
        }
    }
}