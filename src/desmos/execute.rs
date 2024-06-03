use std::cell::RefCell;
use std::convert::identity;

pub use crate::desmos::evaluate::CanDepend;
use crate::desmos::evaluate::{reorder_inplace, topological_indices, topological_sort};
use actions::{ActContext, ActEvaluable, ActExpr, ActFunction, ActIdent, ToActExpr, ACT_IDENTS};

use crate::desmos::evaluate::{
    CompList, CompPrim, EvalExpr, EvalKind, EvalTree, Evaluable, Function, Functions,
    FunctionsBuilder, IdentifierStorer, NonCompList, NonCompPrim, PrimList, Primitive, ToEval,
    UserIdent, ValueContext, VarValue, IDENTIFIERS,
};
use crate::desmos::evaluate::{Points, Polygons};
use crate::desmos::execute::actions::{ActFuncBuilder, ActFunctions, ActValue};
use crate::desmos::rendering::drawables::{
    DrawColor, DrawPoints, DrawPolygons, Drawable, DrawableList, DrawableType, ExplicitEq,
    ExplicitType, ParametricDomain, ParametricEq, DOMAIN_DEFAULT,
};
use crate::desmos::{parsing, Clickable, DesmosCell, Options};
use crate::pooled_vec::Id;

pub mod actions;

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
    drawable: RefCell<Option<(usize, Drawable<'a>)>>,
}

#[derive(Debug, Copy, Clone)]
pub enum DragIdent {
    Point {
        ident: UserIdent,
        x: bool,
        y: bool,
    },
    XY {
        x: Option<UserIdent>,
        y: Option<UserIdent>,
    },
}

impl DragIdent {
    pub fn xy(x: Option<UserIdent>, y: Option<UserIdent>) -> Self {
        Self::XY { x, y }
    }

    pub fn should_xy(&self) -> (bool, bool) {
        match self {
            DragIdent::Point { x, y, .. } => (*x, *y),
            DragIdent::XY { x, y } => (x.is_some(), y.is_some()),
        }
    }
}

#[derive(Debug)]
pub struct Statement<'a> {
    ident: Option<UserIdent>,
    expr: EvalExpr,
    deps: SpecialDeps<'a>,
    drag_mode: Option<DragIdent>,
    drawable_options: DrawableOptions,
}

impl Statement<'_> {
    pub fn new(
        ident: Option<UserIdent>,
        expr: EvalExpr,
        drawable_options: DrawableOptions,
    ) -> Self {
        let drag_mode = drawable_options.options.drag_mode.map(|x| (x.x, x.y));
        //FIXME: I hate this too
        let drag_mode = match drag_mode {
            Some((false, false)) => None,
            should_be @ (None | Some((_, _))) => {
                let result = (|| {
                    if let EvalKind::Point { x, y } = &expr.expr.kind {
                        match (&x.expr.kind, &y.expr.kind) {
                            (EvalKind::Number(_), EvalKind::Number(_)) => {
                                if let Some(ident) = ident {
                                    return Some(DragIdent::Point {
                                        ident,
                                        x: true,
                                        y: true,
                                    });
                                }
                            }
                            (&EvalKind::Ident(x), &EvalKind::Ident(y)) => {
                                return Some(DragIdent::xy(Some(x), Some(y)))
                            }
                            (&EvalKind::Ident(x), _) => return Some(DragIdent::xy(Some(x), None)),
                            (_, &EvalKind::Ident(y)) => return Some(DragIdent::xy(None, Some(y))),
                            _ => {}
                        }
                    }
                    None
                })();

                fn align(should: bool, is: bool) -> bool {
                    match (should, is) {
                        (true, false) => unreachable!("Cannot drag undraggable point"),
                        (false, false) => false,
                        (false, true) => false,
                        (true, true) => true,
                    }
                }

                fn align_opt<T>(should: bool, is: Option<T>) -> Option<T> {
                    match (should, is) {
                        (true, None) => unreachable!("Cannot drag undraggable point"),
                        (false, None) => None,
                        (false, Some(x)) => Some(x),
                        (true, Some(x)) => Some(x),
                    }
                }

                match should_be {
                    None => result,
                    Some((sx, sy)) => Some(
                        match result
                            .unwrap_or_else(|| unreachable!("Cannot drag undraggable point"))
                        {
                            DragIdent::Point { ident, x, y } => DragIdent::Point {
                                ident,
                                x: align(sx, x),
                                y: align(sy, y),
                            },
                            DragIdent::XY { x, y } => {
                                DragIdent::xy(align_opt(sx, x), align_opt(sy, y))
                            }
                        },
                    ),
                }
            }
        };

        Self {
            ident,
            deps: expr.get_deps().into(),
            expr,
            drawable_options,
            drag_mode,
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
    draggable: Option<DragIdent>,
    id: Id,
    value: &VarValue,
    drawable_options: &'a DrawableOptions,
) -> Option<(usize, Drawable<'a>)> {
    let DrawableOptions {
        options:
            Options {
                hidden,
                line_options,
                fill_options,
                point_options,
                drag_mode: _,
            },
        draw_index,
        draw_color: color,
        domain: _,
        clickable,
    } = drawable_options;
    let draw_index = *draw_index;
    let clickable = clickable.as_ref();

    if !hidden {
        Some(match value {
            VarValue::Prim(Primitive::Computable(CompPrim::Point(point))) => (
                draw_index,
                Drawable {
                    id,
                    color,
                    kind: DrawableType::Points(DrawPoints {
                        points: Points::One(*point),
                        line_options: line_options.as_ref().on(),
                        point_options: point_options.as_ref().on_or_default(),
                        draggable,
                    }),
                    clickable,
                },
            ),
            VarValue::Prim(Primitive::NonComputable(NonCompPrim::Polygon(polygon))) => (
                draw_index,
                Drawable {
                    color,
                    id,
                    kind: DrawableType::Polygons(DrawPolygons {
                        polygons: Polygons::One(polygon.clone()),
                        line_options: line_options.as_ref().on_or_default(),
                        fill_options: fill_options.as_ref().on_or_default(),
                    }),
                    clickable,
                },
            ),
            VarValue::List(PrimList::Computable(CompList::Point(ps))) => (
                draw_index,
                Drawable {
                    id,
                    color,
                    kind: DrawableType::Points(DrawPoints {
                        points: Points::Many(ps.clone()),
                        line_options: line_options.as_ref().on(),
                        point_options: point_options.as_ref().on_or_default(),
                        draggable,
                    }),
                    clickable,
                },
            ),
            VarValue::List(PrimList::NonComputable(NonCompList::Polygon(polys))) => (
                draw_index,
                Drawable {
                    id,
                    color,
                    kind: DrawableType::Polygons(DrawPolygons {
                        polygons: Polygons::Many(polys.clone()),
                        line_options: line_options.as_ref().on_or_default(),
                        fill_options: fill_options.as_ref().on_or_default(),
                    }),
                    clickable,
                },
            ),
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
    ) -> Option<(usize, Drawable<'a>)> {
        let Statement {
            ident,
            expr,
            deps,
            drawable_options,
            drag_mode,
        } = self;
        let DrawableOptions {
            options,
            clickable,
            draw_index,
            domain,
            draw_color: color,
        } = drawable_options;
        let (draw_index, ident, drag_mode) = (*draw_index, *ident, *drag_mode);
        let clickable = clickable.as_ref();
        let id = expr.expr.id;

        match deps {
            SpecialDeps::Constant(con) => {
                let con = con.drawable.borrow_mut();

                let drawable = if con.is_none() {
                    let value = expr.evaluate(functions, context);
                    let drawable = vv_to_drawable(drag_mode, id, &value, drawable_options);
                    if let Some(ident) = ident {
                        let _ = context.set_value(ident, value.clone());
                    }
                    drawable
                } else {
                    con.clone()
                };
                return drawable;
            }
            SpecialDeps::None => {
                let value = expr.evaluate(functions, context);
                let drawable = vv_to_drawable(drag_mode, id, &value, drawable_options);
                if let Some(id) = ident {
                    let _ = context.set_value(id, value.clone());
                }
                drawable
            }
            SpecialDeps::ExplicitX => {
                if matches!(ident, Some(IdentifierStorer::IDENT_Y) | None) {
                    if !options.hidden {
                        Some((
                            draw_index,
                            Drawable {
                                id,
                                color,
                                kind: DrawableType::Explicit(ExplicitEq {
                                    kind: ExplicitType::YFromX,
                                    line_options: options.line_options.as_ref().on_or_default(),
                                    equation: expr,
                                }),
                                clickable,
                            },
                        ))
                    } else {
                        None
                    }
                } else {
                    let value = expr.evaluate(functions, context);
                    let drawable = vv_to_drawable(drag_mode, id, &value, drawable_options);
                    if let Some(id) = ident {
                        let _ = context.set_value(id, value.clone());
                    }
                    drawable
                }
            }
            SpecialDeps::ExplicitY => {
                if matches!(ident, Some(IdentifierStorer::IDENT_X)) {
                    if !options.hidden {
                        Some((
                            draw_index,
                            Drawable {
                                id,
                                color,
                                kind: DrawableType::Explicit(ExplicitEq {
                                    kind: ExplicitType::XFromY,
                                    line_options: options.line_options.as_ref().on_or_default(),
                                    equation: expr,
                                }),
                                clickable,
                            },
                        ))
                    } else {
                        None
                    }
                } else {
                    let value = expr.evaluate(functions, context);
                    let drawable = vv_to_drawable(drag_mode, id, &value, drawable_options);
                    if let Some(id) = ident {
                        let _ = context.set_value(id, value.clone());
                    }
                    drawable
                }
            }
            SpecialDeps::Parametric => {
                if context.is_initialized(IdentifierStorer::IDENT_T) {
                    let value = expr.evaluate(functions, context);
                    let drawable = vv_to_drawable(drag_mode, id, &value, drawable_options);
                    if let Some(id) = ident {
                        let _ = context.set_value(id, value.clone());
                    }
                    drawable
                } else if !options.hidden {
                    Some((
                        draw_index,
                        Drawable {
                            id,
                            color,
                            kind: DrawableType::Parametric(ParametricEq {
                                equation: expr,
                                line_options: options.line_options.as_ref().on_or_default(),
                                domain: domain.as_ref().unwrap_or_else(|| &DOMAIN_DEFAULT),
                            }),
                            clickable,
                        },
                    ))
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
            let draw = stmt.execute(&context.functions, &mut context.context);
            if let Some((index, drawable)) = draw {
                drawables.insert(index, drawable);
            }
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

    let indices = topological_sort(
        var_defs
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
            .collect::<Vec<_>>(),
    );

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
            let _ = self.context.set_value(id, value);
        }
    }

    pub fn iter_values(&self) -> impl Iterator<Item = (&UserIdent, &VarValue)> {
        self.context.iter()
    }

    pub fn len_values(&self) -> usize {
        self.context.len()
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
        let old = self.context.set_value(id, val);
        let value = self.evaluate(expr);
        self.context.un_or_set(id, old);
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

    pub fn evaluate_act_with<T: ActEvaluable>(
        &mut self,
        expr: &T,
        id: UserIdent,
        val: VarValue,
    ) -> ActValue {
        let old = self.context.set_value(id, val);
        let val = expr.evaluate(
            &self.act_functions,
            &mut self.act_context,
            &self.functions,
            &mut self.context,
        );
        self.context.un_or_set(id, old);
        val
    }

    pub fn eval_act_with_opt<T: ActEvaluable>(
        &mut self,
        expr: &T,
        id: UserIdent,
        val: Option<VarValue>,
    ) -> ActValue {
        let old = val.map(|v| self.context.set_value(id, v));
        let val = expr.evaluate(
            &self.act_functions,
            &mut self.act_context,
            &self.functions,
            &mut self.context,
        );
        old.map(|o| self.context.un_or_set(id, o));
        val
    }
}
