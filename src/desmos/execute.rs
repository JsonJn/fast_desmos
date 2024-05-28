use std::cell::RefCell;
use std::convert::identity;

use actions::{ActContext, ActEvaluable, ActExpr, ActFunction, ActIdent, ToActExpr, ACT_IDENTS};
pub use dependency::CanDepend;
use dependency::{topological_indices, topological_sort};

use crate::desmos::evaluate::{
    CompList, CompPrim, EvalExpr, EvalKind, EvalTree, Evaluable, Function, Functions,
    FunctionsBuilder, IdentifierStorer, NonCompList, NonCompPrim, PrimList, Primitive, ToEval,
    UserIdent, ValueContext, VarValue, IDENTIFIERS,
};
use crate::desmos::execute::actions::{ActFuncBuilder, ActFunctions, ActValue};
use crate::desmos::execute::dependency::reorder_inplace;
use crate::desmos::rendering::drawables::{
    DrawColor, DrawPoints, DrawPolygons, Drawable, DrawableList, DrawableType, ExplicitEq,
    ExplicitType, ParametricDomain, ParametricEq, Points, Polygons,
};
use crate::desmos::{parsing, Clickable, DesmosCell, Options};

pub mod actions;
mod dependency;

#[derive(Debug, Clone)]
pub enum SpecialDeps<'a> {
    Constant(Constant<'a>),
    None,
    ExplicitX,
    ExplicitY,
    Parametric,
}

impl From<Vec<usize>> for SpecialDeps<'_> {
    fn from(value: Vec<usize>) -> Self {
        if value.is_empty() {
            Self::Constant(Constant::default())
        } else {
            let x = value.contains(&IdentifierStorer::IDENT_X.0);
            let y = value.contains(&IdentifierStorer::IDENT_Y.0);
            let t = value.contains(&IdentifierStorer::IDENT_T.0);

            match (x, y, t) {
                (false, false, false) => Self::None,
                (true, false, false) => Self::ExplicitX,
                (false, true, false) => Self::ExplicitY,
                (false, false, true) => Self::Parametric,
                _ => unreachable!("Invalid combination of x,y,t dependencies: {x:?} {y:?} {t:?}"),
            }
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct Constant<'a> {
    drawable: RefCell<Option<Drawable<'a>>>,
}

#[derive(Debug)]
pub struct Statement<'a> {
    ident: Option<UserIdent>,
    expr: EvalExpr,
    deps: SpecialDeps<'a>,
    drawable_options: DrawableOptions,
}

impl Statement<'_> {
    pub fn new(
        ident: Option<UserIdent>,
        expr: EvalExpr,
        drawable_options: DrawableOptions,
    ) -> Self {
        Self {
            ident,
            deps: expr.get_deps().into(),
            expr,
            drawable_options,
        }
    }
}

#[derive(Debug)]
pub struct DrawableOptions {
    options: Options,
    draw_index: usize,
    draw_color: DrawColor,
    domain: Option<ParametricDomain>,
    clickable: Option<Clickable>,
}

#[derive(Debug)]
pub struct ActStatement {
    ident: Option<ActIdent>,
    expr: ActExpr,
}

fn vv_to_drawable<'a>(
    value: &VarValue,
    drawable_options: &'a DrawableOptions,
) -> Option<Drawable<'a>> {
    let DrawableOptions {
        options:
            Options {
                hidden,
                line_options,
                fill_options,
                point_options,
                point_on_off: points,
                line_on_off: lines,
                fill_on_off: fills,
            },
        draw_index,
        draw_color: color,
        domain,
        clickable,
    } = drawable_options;
    let draw_index = *draw_index;
    let clickable = clickable.as_ref();

    if !hidden {
        Some(match value {
            VarValue::Prim(Primitive::Computable(CompPrim::Point(point))) => Drawable {
                draw_index,
                color,
                kind: DrawableType::Points(DrawPoints {
                    points: Points::One(*point),
                    line_options: lines.on_then(line_options),
                    point_options: points.on_or_default_then(point_options),
                }),
                clickable,
            },
            VarValue::Prim(Primitive::NonComputable(NonCompPrim::Polygon(polygon))) => Drawable {
                draw_index,
                color,
                kind: DrawableType::Polygons(DrawPolygons {
                    polygons: Polygons::One(polygon.clone()),
                    line_options: lines.on_or_default_then(line_options),
                    fill_options: fills.on_or_default_then(fill_options),
                }),
                clickable,
            },
            VarValue::List(PrimList::Computable(CompList::Point(ps))) => Drawable {
                draw_index,
                color,
                kind: DrawableType::Points(DrawPoints {
                    points: Points::Many(ps.clone()),
                    line_options: lines.on_then(line_options),
                    point_options: points.on_or_default_then(point_options),
                }),
                clickable,
            },
            VarValue::List(PrimList::NonComputable(NonCompList::Polygon(polys))) => Drawable {
                draw_index,
                color,
                kind: DrawableType::Polygons(DrawPolygons {
                    polygons: Polygons::Many(polys.clone()),
                    line_options: lines.on_or_default_then(line_options),
                    fill_options: fills.on_or_default_then(fill_options),
                }),
                clickable,
            },
            _ => return None,
        })
    } else {
        None
    }
}

impl Statement<'_> {
    pub fn execute<'a>(
        &'a self,
        functions: &Functions,
        context: &mut ValueContext,
    ) -> Option<Drawable<'a>> {
        let Statement {
            ident,
            expr,
            deps,
            drawable_options,
        } = self;
        let DrawableOptions {
            options,
            clickable,
            draw_index,
            domain,
            draw_color: color,
        } = drawable_options;
        let (draw_index, ident) = (*draw_index, *ident);
        let clickable = clickable.as_ref();

        match deps {
            SpecialDeps::Constant(con) => {
                let con = con.drawable.borrow_mut();

                let drawable = if con.is_none() {
                    let value = expr.evaluate(functions, context);
                    let drawable = vv_to_drawable(&value, drawable_options);
                    if let Some(ident) = ident {
                        context.set_value(ident, value.clone());
                    }
                    drawable
                } else {
                    con.clone()
                };
                return drawable;
            }
            SpecialDeps::None => {
                let value = expr.evaluate(functions, context);
                let drawable = vv_to_drawable(&value, drawable_options);
                if let Some(id) = ident {
                    context.set_value(id, value.clone());
                }
                drawable
            }
            SpecialDeps::ExplicitX => {
                if matches!(ident, Some(IdentifierStorer::IDENT_Y) | None) {
                    if !options.hidden {
                        Some(Drawable {
                            draw_index,
                            color,
                            kind: DrawableType::Explicit(ExplicitEq {
                                kind: ExplicitType::YFromX,
                                line_options: options
                                    .line_on_off
                                    .on_or_default_then(&options.line_options),
                                equation: expr,
                            }),
                            clickable,
                        })
                    } else {
                        None
                    }
                } else {
                    let value = expr.evaluate(functions, context);
                    let drawable = vv_to_drawable(&value, drawable_options);
                    if let Some(id) = ident {
                        context.set_value(id, value.clone());
                    }
                    drawable
                }
            }
            SpecialDeps::ExplicitY => {
                if let Some(IdentifierStorer::IDENT_X) = ident {
                    if !options.hidden {
                        Some(Drawable {
                            draw_index,
                            color,
                            kind: DrawableType::Explicit(ExplicitEq {
                                kind: ExplicitType::XFromY,
                                line_options: options
                                    .line_on_off
                                    .on_or_default_then(&options.line_options),
                                equation: expr,
                            }),
                            clickable,
                        })
                    } else {
                        None
                    }
                } else {
                    let value = expr.evaluate(functions, context);
                    let drawable = vv_to_drawable(&value, drawable_options);
                    if let Some(id) = ident {
                        context.set_value(id, value.clone());
                    }
                    drawable
                }
            }
            SpecialDeps::Parametric => {
                if context.is_initialized(IdentifierStorer::IDENT_T) {
                    let value = expr.evaluate(functions, context);
                    let drawable = vv_to_drawable(&value, drawable_options);
                    if let Some(id) = ident {
                        context.set_value(id, value.clone());
                    }
                    drawable
                } else if !options.hidden {
                    Some(Drawable {
                        draw_index,
                        color,
                        kind: DrawableType::Parametric(ParametricEq {
                            equation: expr,
                            line_options: options
                                .line_on_off
                                .on_or_default_then(&options.line_options),
                            domain: domain.as_ref().unwrap(),
                        }),
                        clickable,
                    })
                } else {
                    None
                }
            }
        }
    }
}

pub struct Statements<'a> {
    statements: Vec<Statement<'a>>,
    act_statements: Vec<ActStatement>,
}

impl Statements<'_> {
    pub fn cycle(&self, context: &mut AllContext) -> Vec<Drawable> {
        let mut drawables = DrawableList::new();

        for stmt in &self.statements {
            drawables.insert_option(stmt.execute(&context.functions, &mut context.context));
        }

        for act_stmt in &self.act_statements {
            let ActStatement { ident, expr } = act_stmt;
            if let &Some(ident) = ident {
                let value = context.evaluate_act(expr);
                context.act_context.set_value(ident, value);
            }
        }

        drawables.into()
    }

    /// Removes the expression associated with this
    /// statement whilst preserving rendering
    pub fn remove_calculations(&mut self, idents: impl IntoIterator<Item = UserIdent>) {
        for ident in idents.into_iter() {
            self.statements
                .iter_mut()
                .filter(|v| v.ident == Some(ident))
                .for_each(|stmt| {
                    // This is called repeatedly on every frame,
                    // so zeroed is necessary.
                    stmt.expr = EvalExpr::new(unsafe {
                        // This is safe since `EvalKind::Ident` doesn't create any `PooledVec`s
                        EvalTree::new_zeroed(EvalKind::Ident(stmt.ident.unwrap()))
                    })
                });
        }
    }
}

pub fn convert_cells(stmts: Vec<DesmosCell>) -> (AllContext, Statements<'static>) {
    let mut var_defs = vec![];
    let mut act_var_defs = vec![];
    let mut exprs = vec![];
    let mut act_exprs = vec![];
    let mut func_idents = vec![];
    let mut act_funcs = ActFuncBuilder::default();
    let mut funcs = FunctionsBuilder::default();

    for (draw_index, cell) in stmts.into_iter().enumerate() {
        let DesmosCell {
            statement,
            options,
            draw_color,
            domain,
            clickable,
        } = cell;

        match statement {
            parsing::Statement::Function(parsing::FunctionDef { func, params, expr }) => {
                let func = IDENTIFIERS
                    .convert_ident(func)
                    .to_user()
                    .expect("Function name must be user-ident");
                let params = params
                    .into_iter()
                    .map(|v| {
                        IDENTIFIERS
                            .convert_ident(v)
                            .to_user()
                            .expect("Function params must be user-ident")
                    })
                    .collect();
                func_idents.push(func);
                funcs.add_function(
                    func,
                    Function {
                        params,
                        expr: expr.to_eval(),
                    },
                );
            }
            parsing::Statement::Variable(parsing::VariableDef { ident, expr }) => {
                let x = IDENTIFIERS
                    .convert_ident(ident)
                    .to_user()
                    .expect("Variable name must be user-ident");
                var_defs.push((
                    x,
                    expr.to_eval(),
                    options,
                    draw_index,
                    draw_color,
                    domain,
                    clickable,
                ));
            }
            parsing::Statement::ActFunction(parsing::ActFuncDef { func, params, expr }) => {
                let x = ACT_IDENTS.name_to_int(&func.0);
                act_funcs.add_function(
                    x,
                    ActFunction {
                        params: params
                            .into_iter()
                            .map(|v| {
                                IDENTIFIERS
                                    .convert_ident(v)
                                    .to_user()
                                    .expect("Act function must")
                            })
                            .collect(),
                        expr: expr.to_act_expr(),
                    },
                );
            }
            parsing::Statement::ActVar(parsing::ActVarDef { ident, expr }) => {
                let ident = ACT_IDENTS.name_to_int(&ident.0);
                act_var_defs.push((ident, expr.to_act_expr()));
            }
            parsing::Statement::Expression(expr) => {
                exprs.push(Statement::new(
                    None,
                    expr.to_eval(),
                    DrawableOptions {
                        options,
                        draw_index,
                        draw_color,
                        domain,
                        clickable,
                    },
                ));
            }
            parsing::Statement::ActExpr(expr) => act_exprs.push(expr.to_act_expr()),
        }
    }

    println!("{:?}", IDENTIFIERS.idents);

    let indices = topological_sort(dbg!(var_defs
        .iter()
        .map(|(ident, expr, _, _, _, _, _)| {
            let mut deps = expr.get_deps();

            for UserIdent(x) in IdentifierStorer::RESERVED_IDENTS {
                while let Some(i) = deps.iter().position(|&v| v == x) {
                    deps.swap_remove(i);
                }
            }

            for &UserIdent(x) in &func_idents {
                while let Some(i) = deps.iter().position(|&v| v == x) {
                    deps.swap_remove(i);
                }
            }

            ((*ident).into(), deps)
        })
        .collect::<Vec<_>>()));

    reorder_inplace(&mut var_defs, indices);
    let mut statements: Vec<_> = var_defs
        .into_iter()
        .map(
            |(ident, expr, options, draw_index, draw_color, domain, clickable)| {
                Statement::new(
                    Some(ident),
                    expr,
                    DrawableOptions {
                        options,
                        draw_index,
                        draw_color,
                        domain,
                        clickable,
                    },
                )
            },
        )
        .collect();

    statements.extend(exprs);

    let indices = topological_indices(&act_var_defs, identity);
    reorder_inplace(&mut act_var_defs, indices);
    let mut act_statements: Vec<_> = act_var_defs
        .into_iter()
        .map(|(ident, expr)| ActStatement {
            ident: Some(ident),
            expr,
        })
        .collect();

    act_statements.extend(
        act_exprs
            .into_iter()
            .map(|expr| ActStatement { ident: None, expr }),
    );

    println!("{:?}", IDENTIFIERS.idents);

    (
        AllContext {
            act_functions: act_funcs.build(),
            functions: funcs.build(),
            context: ValueContext::default(),
            act_context: ActContext::default(),
        },
        Statements {
            statements,
            act_statements,
        },
    )
}

pub struct AllContext {
    functions: Functions,
    context: ValueContext,
    act_functions: ActFunctions,
    act_context: ActContext,
}

impl AllContext {
    pub fn apply_act_value(&mut self, act_value: ActValue) {
        for (id, value) in act_value.pairs {
            self.context.set_value(id, value);
        }
    }

    pub fn evaluate<T: Evaluable>(&mut self, expr: &T) -> VarValue {
        expr.evaluate(&self.functions, &mut self.context)
    }

    pub fn evaluate_with<T: Evaluable>(
        &mut self,
        expr: &T,
        id: UserIdent,
        val: VarValue,
    ) -> VarValue {
        self.context.set_value(id, val);
        let value = self.evaluate(expr);
        self.context.unset(id);
        value
    }

    pub fn evaluate_act<T: ActEvaluable>(&mut self, expr: &T) -> ActValue {
        expr.evaluate(
            &self.act_functions,
            &mut self.act_context,
            &self.functions,
            &mut self.context,
        )
    }
}