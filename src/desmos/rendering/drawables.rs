use std::collections::VecDeque;

use once_cell::sync::Lazy;

pub use drawable_list::DrawableList;

use crate::desmos::evaluate::{
    Color, EvalExpr, EvalKind, EvalTree, Ident, Point, Points, Polygons, UserIdent, IDENTIFIERS,
};
use crate::desmos::{parsing, Clickable};

mod drawable_list;

fn number_expr(num: f64) -> EvalExpr {
    // This is safe because `EvalKind::Number` doesn't create any `PooledVec`s
    EvalExpr::new(unsafe { EvalTree::new_zeroed(EvalKind::Number(num)) })
}

#[derive(Debug)]
pub struct LineOptions {
    pub thickness: EvalExpr,
    pub opacity: EvalExpr,
    pub kind: LineType,
}

impl Default for LineOptions {
    fn default() -> Self {
        Self::from_options(None, None, None)
    }
}

impl LineOptions {
    pub fn from_options(
        thickness: Option<EvalExpr>,
        opacity: Option<EvalExpr>,
        kind: Option<LineType>,
    ) -> Self {
        Self {
            thickness: thickness.unwrap_or(number_expr(2.5)),
            opacity: opacity.unwrap_or(number_expr(1.0)),
            kind: kind.unwrap_or_default(),
        }
    }
}

#[derive(Default, Debug, Copy, Clone)]
pub enum LineType {
    #[default]
    Solid,
    Dashed,
    Dotted,
}

#[derive(Debug)]
pub struct PointOptions {
    pub diameter: EvalExpr,
    pub opacity: EvalExpr,
    pub label: Option<Label>,
    pub kind: PointType,
}

impl Default for PointOptions {
    fn default() -> Self {
        Self::from_options(None, None, None, None)
    }
}

impl PointOptions {
    pub fn from_options(
        diameter: Option<EvalExpr>,
        opacity: Option<EvalExpr>,
        label: Option<Option<Label>>,
        kind: Option<PointType>,
    ) -> Self {
        Self {
            diameter: diameter.unwrap_or(number_expr(8.0)),
            opacity: opacity.unwrap_or(number_expr(1.0)),
            label: label.flatten(),
            kind: kind.unwrap_or_default(),
        }
    }
}

#[derive(Default, Debug, Copy, Clone)]
pub enum PointType {
    #[default]
    Solid,
    Hollow,
    Cross,
}

#[derive(Debug)]
pub struct FillOptions {
    pub opacity: EvalExpr,
}

impl Default for FillOptions {
    fn default() -> Self {
        Self::from_options(None)
    }
}

impl FillOptions {
    pub fn from_options(opacity: Option<EvalExpr>) -> Self {
        Self {
            opacity: opacity.unwrap_or(number_expr(0.4)),
        }
    }
}

#[derive(Debug)]
pub struct Label {
    pub format: Vec<String>,
    pub idents: Vec<UserIdent>,
    pub size: EvalExpr,
    pub angle: EvalExpr,
}

impl Default for Label {
    fn default() -> Self {
        Self::from_options(None, None, None)
    }
}

impl Label {
    pub fn new(raw_label: String, size: EvalExpr, angle: EvalExpr) -> Self {
        let mut format = Vec::new();
        let mut idents = Vec::new();

        let mut chars: VecDeque<_> = raw_label.chars().collect();
        let mut this_string = String::new();
        while !chars.is_empty() {
            let next = chars.pop_front().unwrap();
            if next == '{' && chars.front().is_some_and(|&v| v == '{') {
                format.push(this_string);
                this_string = String::new();

                chars.pop_front(); // Remove '{'

                let next_split = chars
                    .iter()
                    .position(|&v| v == '}')
                    .expect("Unclosed variable interpolation");
                let new_chars = chars.split_off(next_split);
                let ident: String = chars.into_iter().collect();
                let ident = IDENTIFIERS.convert_ident(parsing::Ident(ident));
                let Ident::User(ui) = ident else {
                    unreachable!("Can't use builtins in label interpolation");
                };
                idents.push(ui);

                chars = new_chars;
                chars.pop_front(); // Remove '}'
            } else {
                this_string.push(next);
            }
        }
        if !this_string.is_empty() {
            format.push(this_string);
        }

        Self {
            format,
            idents,
            size,
            angle,
        }
    }

    pub fn from_options(
        raw_label: Option<String>,
        size: Option<EvalExpr>,
        angle: Option<EvalExpr>,
    ) -> Self {
        Self::new(
            raw_label.unwrap_or_default(),
            size.unwrap_or(number_expr(1.0)),
            angle.unwrap_or(number_expr(0.0)),
        )
    }
}

#[derive(Debug)]
pub struct DrawColor {
    pub color: Color,
    pub latex: Option<EvalExpr>,
}

impl Default for DrawColor {
    fn default() -> Self {
        Self {
            color: Color(0, 0, 0),
            latex: None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Drawable<'a> {
    pub draw_index: usize,
    pub color: &'a DrawColor,
    pub clickable: Option<&'a Clickable>,
    pub kind: DrawableType<'a>,
}

#[derive(Debug, Clone)]
pub enum DrawableType<'a> {
    Parametric(ParametricEq<'a>),
    Explicit(ExplicitEq<'a>),
    Points(DrawPoints<'a>),
    Polygons(DrawPolygons<'a>),
}

#[derive(Debug)]
pub struct ParametricDomain {
    pub min: EvalExpr,
    pub max: EvalExpr,
}

impl Default for ParametricDomain {
    fn default() -> Self {
        Self::from_options(None, None)
    }
}

pub static DOMAIN_DEFAULT: Lazy<ParametricDomain> = Lazy::new(ParametricDomain::default);

impl ParametricDomain {
    pub fn from_options(min: Option<EvalExpr>, max: Option<EvalExpr>) -> Self {
        Self {
            min: min.unwrap_or(number_expr(0.0)),
            max: max.unwrap_or(number_expr(1.0)),
        }
    }
}

/// Expressions in terms of t
#[derive(Debug, Clone)]
pub struct ParametricEq<'a> {
    pub equation: &'a EvalExpr,
    pub domain: &'a ParametricDomain,
    pub line_options: Option<&'a LineOptions>,
}

/// Statements of the form y=f(x) or f(x)=expr or expr or x=f(y)
#[derive(Debug, Clone)]
pub struct ExplicitEq<'a> {
    pub kind: ExplicitType,
    pub equation: &'a EvalExpr,
    pub line_options: Option<&'a LineOptions>,
}

#[derive(Debug, Copy, Clone)]
pub enum ExplicitType {
    YFromX,
    XFromY,
}

impl ExplicitType {
    pub fn get_point(&self, src: f64, dst: f64) -> Point {
        match self {
            ExplicitType::YFromX => Point(src, dst),
            ExplicitType::XFromY => Point(dst, src),
        }
    }
}

/// Statement or expression evaluating to point or point list
#[derive(Debug, Clone)]
pub struct DrawPoints<'a> {
    pub points: Points,
    pub point_options: Option<&'a PointOptions>,
    pub line_options: Option<&'a LineOptions>,
}

/// Statement or expression evaluating to polygon or polygon list
#[derive(Debug, Clone)]
pub struct DrawPolygons<'a> {
    pub polygons: Polygons,
    pub line_options: Option<&'a LineOptions>,
    pub fill_options: Option<&'a FillOptions>,
}
