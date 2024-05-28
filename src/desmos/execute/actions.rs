pub use tree::{ActExpr, ActIdent, ActTree};
pub use value::ActValue;

use crate::desmos::evaluate::{CondOutput, Evaluable, Functions, ValueContext};

mod context;
mod convert;
mod tree;
mod value;

use crate::pooled_vec::Id;
pub use context::{ActContext, ActFuncBuilder, ActFunction, ActFunctions};
#[allow(unused_imports)]
pub use convert::{ToActExpr, ACT_IDENTS};

pub trait ActEvaluable {
    fn evaluate(
        &self,
        act_functions: &ActFunctions,
        act_context: &mut ActContext,
        functions: &Functions,
        context: &mut ValueContext,
    ) -> ActValue;
}

impl ActEvaluable for ActExpr {
    fn evaluate(
        &self,
        act_functions: &ActFunctions,
        act_context: &mut ActContext,
        functions: &Functions,
        context: &mut ValueContext,
    ) -> ActValue {
        self.0
            .evaluate(act_functions, act_context, functions, context)
    }
}

impl ActEvaluable for ActTree {
    fn evaluate(
        &self,
        act_functions: &ActFunctions,
        act_context: &mut ActContext,
        functions: &Functions,
        context: &mut ValueContext,
    ) -> ActValue {
        match self {
            ActTree::Many(many) => many
                .into_iter()
                .map(|v| v.evaluate(act_functions, act_context, functions, context))
                .sum(),
            ActTree::IfElse { cond, yes, no } => {
                // SAFETY: The id cannot cause a overlapping vector because it would panic anyway
                let CondOutput::Prim(b) = cond.evaluate(Id::zeroed(), functions, context) else {
                    unreachable!("Actions cannot be placed in a list")
                };

                if b {
                    yes.evaluate(act_functions, act_context, functions, context)
                } else {
                    no.evaluate(act_functions, act_context, functions, context)
                }
            }
            ActTree::Ident(id) => act_context.get_value(*id),
            ActTree::Raw { ident, expr } => {
                ActValue::new(vec![(*ident, expr.evaluate(functions, context))])
            }
            ActTree::Call { ident, params } => {
                let ActFunction {
                    params: idents,
                    expr,
                } = act_functions.get_function(*ident);
                assert_eq!(
                    params.len(),
                    idents.len(),
                    "Function call parameters do not match function parameters"
                );

                for (&id, param) in idents.iter().zip(params) {
                    let value = param.evaluate(functions, context);
                    context.set_value(id, value);
                }

                let v = expr.evaluate(act_functions, act_context, functions, context);

                for &id in idents {
                    context.unset(id);
                }

                v
            }
        }
    }
}
