use std::num::NonZeroUsize;

use raylib::consts::{MouseButton, MouseCursor};
use raylib::drawing::{RaylibDrawHandle, RaylibMode2D};
use raylib::prelude as rl_prelude;
use raylib::prelude::{Camera2D, KeyboardKey, RaylibDraw, RaylibFont, RaylibMode2DExt, Vector2};
use raylib::{check_collision_point_poly, ffi};

use crate::desmos::evaluate::{
    Color, Colors, Conditional, EvalKind, IdentifierStorer, LinearFunc, Numbers, Point, Points,
    Polygon, Primitive, VarValue, IDENTIFIERS,
};
use crate::desmos::execute::actions::ActValue;
use crate::desmos::execute::{convert_cells, AllContext, DragIdent};
use crate::desmos::parsing::InequalityType;
use crate::desmos::rendering::circles::{CIRCLE_16, CIRCLE_32, CIRCLE_8};
use crate::desmos::rendering::drawables::{
    DrawPoints, DrawPolygons, Drawable, DrawableType, ExplicitEq, ExplicitType, FillOptions,
    LineOptions, ParametricDomain, ParametricEq, PointOptions,
};
use crate::desmos::{Clickable, DesmosPage, Viewport};
use crate::pooled_vec::Id;

mod circles;
pub mod drawables;

impl From<Point> for ffi::Vector2 {
    fn from(value: Point) -> Self {
        Self {
            x: value.0 as f32,
            y: -value.1 as f32,
        }
    }
}

impl From<Point> for Vector2 {
    fn from(value: Point) -> Self {
        Self {
            x: value.0 as f32,
            y: -value.1 as f32,
        }
    }
}

impl Color {
    pub fn with_opacity(self, opacity: f64) -> ffi::Color {
        ffi::Color {
            a: (opacity * 256.0).clamp(0.0, 255.0) as u8,
            r: self.0,
            g: self.1,
            b: self.2,
        }
    }
}
const WIDTH: i32 = 1000;
const HEIGHT: i32 = 800;

const KEY_OFFSETS: [(KeyboardKey, Vector2); 4] = [
    (KeyboardKey::KEY_W, Vector2::new(0.0, -1.0)),
    (KeyboardKey::KEY_A, Vector2::new(-1.0, 0.0)),
    (KeyboardKey::KEY_S, Vector2::new(0.0, 1.0)),
    (KeyboardKey::KEY_D, Vector2::new(1.0, 0.0)),
];
const MOVE_SPEED: f32 = 1000.0;
const SCALE_SPEED: f32 = 1.1;

struct Processor<'a, 'b, 'c> {
    camera: Camera2D,
    mouse_pos: Vector2,
    zoom: f32,
    context: &'a mut AllContext,
    drawing: &'a mut RaylibMode2D<'b, RaylibDrawHandle<'c>>,
}

enum ClickableInfo<'a> {
    Hovered,
    Dragged {
        id: Id,
        ident: DragIdent,
        index: Option<NonZeroUsize>,
        points: Points,
    },
    Clicked {
        clickable: &'a Clickable,
        index: Option<NonZeroUsize>,
    },
}

struct PointDrawResult {
    index: Option<usize>,
    dragged: Option<(DragIdent, Id, Points)>,
}

impl From<Option<usize>> for PointDrawResult {
    fn from(value: Option<usize>) -> Self {
        Self {
            index: value,
            dragged: None,
        }
    }
}

impl<'a, 'b, 'c> Processor<'a, 'b, 'c> {
    pub fn new(
        camera2d: Camera2D,
        mouse_pos: Vector2,
        zoom: f32,
        context: &'a mut AllContext,
        drawing: &'a mut RaylibMode2D<'b, RaylibDrawHandle<'c>>,
    ) -> Self {
        Self {
            camera: camera2d,
            mouse_pos,
            zoom,
            context,
            drawing,
        }
    }

    fn process<'r>(
        &mut self,
        drawable: Drawable<'r>,
        dragging: Option<Id>,
    ) -> Option<ClickableInfo<'r>> {
        let Drawable {
            id,
            color,
            clickable,
            kind,
        } = drawable;

        let color: Colors = if let Some(latex) = &color.latex {
            self.context.evaluate(latex).try_into().unwrap()
        } else {
            Colors::One(color.color)
        };

        let check_click = clickable.is_some();

        let click_result = match kind {
            DrawableType::Parametric(parametric) => {
                self.draw_parametric(parametric, color);
                None
            }
            DrawableType::Explicit(explicit) => {
                self.draw_explicit(explicit, color);
                None
            }
            DrawableType::Points(points) => self.process_points(
                id,
                dragging.is_some_and(|v| v == id),
                points,
                color,
                check_click,
            ),
            DrawableType::Polygons(polygons) => self
                .draw_polygons(polygons, color, check_click)
                .map(From::from),
        };

        click_result.map(|result| {
            let PointDrawResult { dragged, index } = result;

            match dragged {
                None => {
                    match self
                        .drawing
                        .is_mouse_button_pressed(MouseButton::MOUSE_BUTTON_LEFT)
                    {
                        true => ClickableInfo::Clicked {
                            index: index.map(|x| unsafe { NonZeroUsize::new_unchecked(x + 1) }),
                            clickable: clickable.unwrap(),
                        },
                        false => ClickableInfo::Hovered,
                    }
                }
                Some((dragged, id, points)) => {
                    match self
                        .drawing
                        .is_mouse_button_down(MouseButton::MOUSE_BUTTON_LEFT)
                    {
                        true => ClickableInfo::Dragged {
                            id,
                            index: index.map(|x| unsafe { NonZeroUsize::new_unchecked(x + 1) }),
                            ident: dragged,
                            points,
                        },
                        false => ClickableInfo::Hovered,
                    }
                }
            }
        })
    }

    fn process_points(
        &mut self,
        id: Id,
        dragging: bool,
        points: DrawPoints,
        color: Colors,
        check_clickable: bool,
    ) -> Option<PointDrawResult> {
        let DrawPoints {
            points,
            point_options,
            line_options,
            draggable,
        } = points;

        let mut click_index = None;
        if let Some(PointOptions {
            opacity,
            kind: _, //TODO: Point kind
            diameter,
            label: _, //TODO: Label
        }) = point_options
        {
            let diameter: Numbers = self
                .context
                .evaluate(diameter)
                .try_into()
                .expect("Diameter must be a number");

            let opacity: Numbers = self
                .context
                .evaluate(opacity)
                .try_into()
                .expect("Opacity must be a number");

            for (index, (&p, ((&diameter, &opacity), &color))) in points
                .iter()
                .zip(
                    diameter
                        .iter_rep()
                        .zip(opacity.iter_rep())
                        .zip(color.iter_rep()),
                )
                .enumerate()
            {
                let radius = diameter as f32 / 2.0 / self.zoom;
                let visual_radius = radius * self.camera.zoom;

                let center: Vector2 = p.into();
                let Vector2 { x: px, y: py } = center;

                let circle_points: &[(f32, f32)] = if visual_radius < 10.0 {
                    &CIRCLE_8
                } else if visual_radius < 100.0 {
                    &CIRCLE_16
                } else {
                    &CIRCLE_32
                };

                if (draggable.is_some() || check_clickable)
                    && (dragging
                        || center.distance_to(self.mouse_pos)
                            <= if draggable.is_some() {
                                radius * 2.0
                            } else {
                                radius
                            })
                {
                    click_index = Some(points.is_many().then_some(index));
                }

                self.drawing.draw_triangle_fan(
                    &circle_points
                        .iter()
                        .map(|(x, y)| Vector2::new(px + x * radius, py + y * radius))
                        .collect::<Vec<_>>(),
                    color.with_opacity(opacity),
                );

                if draggable.is_some() {
                    self.drawing.draw_triangle_fan(
                        &circle_points
                            .iter()
                            .map(|(x, y)| {
                                Vector2::new(px + x * radius * 2.0, py + y * radius * 2.0)
                            })
                            .collect::<Vec<_>>(),
                        color.with_opacity(opacity * 0.35),
                    );
                }
            }
        }

        if let Some(LineOptions {
            opacity,
            thickness,
            kind: _, //TODO: Line kind
        }) = line_options
        {
            let opacity: Numbers = self
                .context
                .evaluate(opacity)
                .try_into()
                .expect("Opacity must be a number");

            let thickness: Numbers = self
                .context
                .evaluate(thickness)
                .try_into()
                .expect("Thickness must be a number");

            for (ps, ((&opacity, &thickness), color)) in points.windows(2).zip(
                opacity
                    .iter_rep()
                    .zip(thickness.iter_rep())
                    .zip(color.into_iter_rep()),
            ) {
                let line_color = color.with_opacity(opacity);
                let [this, next]: [Point; 2] = ps.try_into().unwrap();
                self.drawing
                    .draw_line_ex(this, next, thickness as f32 / self.zoom, line_color);
            }
        }

        click_index.map(|index| PointDrawResult {
            index,
            dragged: draggable.map(|ident| (ident, id, points)),
        })
    }

    fn draw_polygons(
        &mut self,
        polygons: DrawPolygons,
        color: Colors,
        check_clickable: bool,
    ) -> Option<Option<usize>> {
        let mut clickable = None;

        let DrawPolygons {
            polygons,
            fill_options,
            line_options,
        } = polygons;

        if let Some(FillOptions { opacity }) = fill_options {
            let opacity: Numbers = self
                .context
                .evaluate(opacity)
                .try_into()
                .expect("Opacity must be a number");

            for (index, ((Polygon(points), &opacity), &color)) in polygons
                .iter()
                .zip(opacity.iter_rep())
                .zip(color.iter_rep())
                .enumerate()
            {
                if points.len() > 2 {
                    let fill_color = color.with_opacity(opacity);

                    let mut vecs = points
                        .into_iter()
                        .map(|&v| -> Vector2 { v.into() })
                        .collect::<Vec<_>>();

                    let origin = vecs[0];
                    let last1 = vecs[vecs.len() - 1];
                    let last2 = vecs[vecs.len() - 2];
                    let offset1 = last1 - origin;
                    let offset2 = last2 - origin;
                    let cross_prod = offset1.x * offset2.y - offset1.y * offset2.x;

                    if cross_prod.is_sign_negative() {
                        vecs.reverse();
                    }

                    if check_clickable && check_collision_point_poly(self.mouse_pos, &vecs) {
                        clickable = Some(polygons.is_many().then_some(index));
                    }
                    self.drawing.draw_triangle_fan(&vecs, fill_color);
                }
            }
        }

        if let Some(LineOptions {
            opacity,
            kind: _, //TODO: Line Type
            thickness,
        }) = line_options
        {
            let opacity: Numbers = self
                .context
                .evaluate(opacity)
                .try_into()
                .expect("Opacity must be a number");

            let thickness: Numbers = self
                .context
                .evaluate(thickness)
                .try_into()
                .expect("Opacity must be a number");

            for (Polygon(points), ((&opacity, &thickness), &color)) in polygons.into_iter().zip(
                opacity
                    .iter_rep()
                    .zip(thickness.iter_rep())
                    .zip(color.iter_rep()),
            ) {
                let line_color = color.with_opacity(opacity);
                for i in 0..points.len() {
                    let this = points[i];
                    let next = points[(i + 1) % points.len()];
                    self.drawing
                        .draw_line_ex(this, next, thickness as f32 / self.zoom, line_color);
                }
            }
        }
        clickable
    }

    fn draw_explicit(&mut self, explicit: ExplicitEq, color: Colors) {
        let ExplicitEq {
            kind,
            line_options,
            equation,
        } = explicit;

        if let Some(LineOptions {
            opacity,
            kind: _, //TODO: Line type
            thickness,
        }) = line_options
        {
            let opacity: Numbers = self.context.evaluate(opacity).try_into().unwrap();
            let thickness: Numbers = self.context.evaluate(thickness).try_into().unwrap();

            let src_ident = match kind {
                ExplicitType::YFromX => IdentifierStorer::IDENT_X,
                ExplicitType::XFromY => IdentifierStorer::IDENT_Y,
            };

            let src_expr = EvalKind::Ident(src_ident);

            //FIXME: I hate this omg 🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮🤮
            let ((min_bound, max_bound), expr) = (|| {
                let equation_clone = equation.expr.as_ref();
                if let EvalKind::Multiply(exprs) = &equation_clone.kind {
                    if exprs.len() == 2 {
                        let first = &exprs[0];
                        let constraint = &exprs[1];
                        let (first, constraint) = (first.expr.as_ref(), constraint.expr.as_ref());
                        if let EvalKind::IfElse {
                            cond: Conditional::Inequality { id: _, exprs, comp },
                            yes: _,
                            no: _,
                        } = &constraint.kind
                        {
                            if 2 <= exprs.len() && exprs.len() <= 3 {
                                let all_lt = comp.iter().all(|&c| {
                                    c == InequalityType::LessThan
                                        || c == InequalityType::LessOrEqual
                                });

                                let all_gt = comp.iter().all(|&c| {
                                    c == InequalityType::MoreThan
                                        || c == InequalityType::MoreOrEqual
                                });

                                if all_lt || all_gt {
                                    if let Some(ind) =
                                        exprs.iter().position(|e| (*e.expr).kind == src_expr)
                                    {
                                        let left = ind.checked_sub(1);
                                        let right = ind + 1;
                                        let right = (right < exprs.len()).then_some(right);

                                        let [left, right]: [Option<Numbers>; 2] = [left, right]
                                            .map(|i| {
                                                i.map(|i| {
                                                    self.context
                                                        .evaluate(&exprs[i])
                                                        .try_into()
                                                        .unwrap()
                                                })
                                            });

                                        return (
                                            if all_lt {
                                                (left, right)
                                            } else if all_gt {
                                                (right, left)
                                            } else {
                                                unreachable!()
                                            },
                                            first,
                                        );
                                    }
                                }
                            }
                        }
                    }
                }
                ((None, None), equation.expr.as_ref())
            })();
            let (min_bound, max_bound) = (
                min_bound.unwrap_or_else(|| {
                    let corner = Vector2::new(0.0, 0.0);
                    let undone = self.drawing.get_screen_to_world2D(corner, self.camera);

                    Numbers::One(match kind {
                        ExplicitType::YFromX => undone.x,
                        ExplicitType::XFromY => undone.y,
                    } as f64)
                }),
                max_bound.unwrap_or_else(|| {
                    let corner = Vector2::new(WIDTH as f32, HEIGHT as f32);
                    let undone = self.drawing.get_screen_to_world2D(corner, self.camera);

                    Numbers::One(match kind {
                        ExplicitType::YFromX => undone.x,
                        ExplicitType::XFromY => undone.y,
                    } as f64)
                }),
            );
            match (min_bound, max_bound) {
                (Numbers::One(min), Numbers::One(max)) => {
                    let [min_v, max_v] = [min, max].map(VarValue::number);
                    let [min_dest, max_dest]: [Numbers; 2] = [min_v, max_v].map(|v| {
                        self.context
                            .evaluate_with(expr, src_ident, v)
                            .try_into()
                            .expect("Explicit function must be numbers")
                    });

                    for ((min_dest, max_dest), ((thickness, opacity), color)) in
                        min_dest.into_iter().zip(max_dest.into_iter()).zip(
                            thickness
                                .into_iter_rep()
                                .zip(opacity.into_iter_rep())
                                .zip(color.into_iter_rep()),
                        )
                    {
                        self.drawing.draw_line_ex(
                            kind.get_point(min, min_dest),
                            kind.get_point(max, max_dest),
                            thickness as f32 / self.zoom,
                            color.with_opacity(opacity),
                        );
                    }
                }
                (_min_bound, _max_bound) => todo!("Variable bounds"),
            }
        }
    }

    fn draw_parametric(&mut self, parametric: ParametricEq, color: Colors) {
        let ParametricEq {
            equation,
            line_options,
            domain,
        } = parametric;

        if let Some(LineOptions {
            opacity,
            kind: _, //TODO: Parametric line type
            thickness,
        }) = line_options
        {
            let opacity: Numbers = self
                .context
                .evaluate(opacity)
                .try_into()
                .expect("Opacity must be numbers");
            let thickness: Numbers = self
                .context
                .evaluate(thickness)
                .try_into()
                .expect("Thickness must be numbers");

            let ParametricDomain { min, max } = domain;
            let [min, max]: [f64; 2] = [min, max].map(|expr| {
                let comp: Primitive = self
                    .context
                    .evaluate(expr)
                    .try_into()
                    .expect("Parametric bounds must be number");
                comp.try_into().expect("Parametric bounds must be number")
            });

            let [start, end]: [Points; 2] = [min, max].map(|num| {
                self.context
                    .evaluate_with(equation, IdentifierStorer::IDENT_T, VarValue::number(num))
                    .try_into()
                    .expect("Parametric drawing requires points!")
            });

            //TODO Draw curved parametric equations
            for ((start, end), (thick, (color, opacity))) in
                start.into_iter().zip(end.into_iter()).zip(
                    thickness
                        .into_iter_rep()
                        .zip(color.into_iter_rep().zip(opacity.into_iter_rep())),
                )
            {
                self.drawing.draw_line_ex(
                    start,
                    end,
                    thick as f32 / self.zoom,
                    color.with_opacity(opacity),
                )
            }
        }
    }
}

const FONT_SIZE: f32 = 20.0;
const SCROLL_SPEED: f32 = 1.0;

fn render_eval_view<Font: RaylibFont>(
    drawing: &mut RaylibDrawHandle,
    font: &Font,
    context: &AllContext,
    scroll_amt: Vector2,
) {
    const HEIGHT_PAD: f32 = 5.0;
    const SPACING: f32 = 1.0;
    const TEXT_OFFSET: Vector2 = Vector2::new(5.0, 0.0);

    let mut positions = Vec::with_capacity(context.len_values());
    let mut position = scroll_amt;
    let mut max_width = f32::NAN;
    for (ident, _) in context.iter_values() {
        let name = IDENTIFIERS.int_to_name(ident).unwrap();
        let Vector2 { x, y } = font.measure_text(&name, FONT_SIZE, SPACING);
        max_width = max_width.max(x);
        positions.push(position);
        drawing.draw_text_ex(
            &font,
            &name,
            position + TEXT_OFFSET,
            FONT_SIZE,
            SPACING,
            rl_prelude::Color::BLACK,
        );
        position.y += y + HEIGHT_PAD;
    }

    for ((_, value), pos) in context.iter_values().zip(positions) {
        let string = {
            let mut s = value.to_string();
            s.insert_str(0, " = ");
            s
        };
        drawing.draw_text_ex(
            &font,
            &string,
            pos + Vector2::new(max_width, 0.0),
            FONT_SIZE,
            SPACING,
            rl_prelude::Color::BLACK,
        );
    }
}

#[allow(unused)]
pub fn window(params: DesmosPage) {
    let DesmosPage {
        title,
        min_steps_ms,
        cells,
        viewport,
        ticker,
    } = params;

    let (mut context, mut statements, bounds) = convert_cells(cells);

    let (mut rl, thread) = raylib::init()
        .size(WIDTH, HEIGHT)
        .title(&title)
        .msaa_4x()
        .build();

    rl.set_exit_key(None);

    let fps = min_steps_ms.map(|v| 1000.0 / v).unwrap_or(60.0);
    rl.set_target_fps(fps as u32);

    let Viewport { x_range, y_range } = viewport;

    let [tx, ty] = [x_range, y_range].map(|(min, max)| (min + max) as f32 / 2.0);
    let width_req = (x_range.1 - x_range.0) as f32;
    let width_ratio = WIDTH as f32 / width_req;

    let mut camera = Camera2D {
        zoom: width_ratio,
        target: Vector2::new(tx, -ty),
        offset: Vector2::new((WIDTH / 2) as f32, (HEIGHT / 2) as f32),
        ..Default::default()
    };

    let mut dragging = None;
    let mut eval_open = false;
    let mut eval_scroll_amt = Vector2::zero();
    let mut run_ticker = false;
    let mut scale = width_ratio;

    let font = rl
        .load_font_ex(&thread, "TimesNewRoman.ttf", FONT_SIZE as i32, None)
        .unwrap();

    while !rl.window_should_close() {
        // println!("FRAME START");

        let drawables = statements.cycle(&mut context);
        let dt = rl.get_frame_time();
        let act_val = run_ticker
            .then(|| {
                ticker.as_ref().map(|func| {
                    context.evaluate_act_with(
                        &func.expr,
                        IdentifierStorer::IDENT_DT,
                        VarValue::number(1000.0 * dt as f64),
                    )
                })
            })
            .flatten();
        let mouse_pos_world = rl.get_screen_to_world2D(rl.get_mouse_position(), camera);

        for (key, vec) in KEY_OFFSETS {
            if rl.is_key_down(key) {
                camera.target += vec * MOVE_SPEED * dt / camera.zoom;
            }
        }

        if rl.is_key_pressed(KeyboardKey::KEY_F3) {
            eval_open = !eval_open;
        }
        if rl.is_key_pressed(KeyboardKey::KEY_SPACE) {
            run_ticker = !run_ticker;
        }

        let wheel: Vector2 = rl.get_mouse_wheel_move_v().into();
        if eval_open {
            eval_scroll_amt += wheel * FONT_SIZE * SCROLL_SPEED;
        } else {
            if rl.is_key_down(KeyboardKey::KEY_LEFT_CONTROL) {
                scale *= SCALE_SPEED.powf(wheel.y);
            } else {
                camera.zoom *= SCALE_SPEED.powf(wheel.y);
            }
        }

        {
            let mut d = rl.begin_drawing(&thread);

            d.clear_background(raylib::prelude::Color::WHITE);

            let clickable_val = {
                let mut d2 = d.begin_mode2D(camera);
                let mut drawer =
                    Processor::new(camera, mouse_pos_world, scale, &mut context, &mut d2);

                let mut clickable_result = None;
                for drawable in drawables {
                    let this_result = drawer.process(drawable, dragging);
                    if let Some(result) = this_result {
                        clickable_result = Some(result);
                    }
                }

                d2.set_mouse_cursor(match clickable_result {
                    Some(ClickableInfo::Hovered) => MouseCursor::MOUSE_CURSOR_POINTING_HAND,
                    Some(ClickableInfo::Dragged { .. }) => MouseCursor::MOUSE_CURSOR_RESIZE_ALL,
                    _ => MouseCursor::MOUSE_CURSOR_DEFAULT,
                });

                if let Some(ClickableInfo::Dragged { id, .. }) = clickable_result {
                    dragging = Some(id);
                } else {
                    dragging = None;
                }

                match clickable_result {
                    Some(ClickableInfo::Clicked {
                        clickable: Clickable { expr },
                        index,
                    }) => Some(context.eval_act_with_opt(
                        expr,
                        IdentifierStorer::IDENT_INDEX,
                        index.map(|i| VarValue::number(usize::from(i) as f64)),
                    )),

                    Some(ClickableInfo::Dragged {
                        id,
                        index,
                        ident,
                        points,
                    }) => {
                        fn apply_if_some(func: Option<LinearFunc>, num: f64) -> f64 {
                            match func {
                                None => num,
                                Some(func) => func.inverse_f64(num),
                            }
                        }

                        let new_pos = d2.get_screen_to_world2D(d2.get_mouse_position(), camera);
                        let new_pos = (new_pos.x as f64, -new_pos.y as f64);
                        let (sx, sy) = ident.should_xy();
                        let (map_x, map_y) = match ident {
                            DragIdent::Point { .. } => (None, None),
                            DragIdent::XY { x, y } => (x, y),
                        };

                        let new_value = match (index, points) {
                            (Some(index), Points::Many(mut points)) => {
                                let index = index.get();
                                if sx {
                                    points[index].0 = apply_if_some(map_x, new_pos.0);
                                }
                                if sy {
                                    points[index].1 = apply_if_some(map_y, new_pos.1);
                                }
                                Points::Many(points)
                            }
                            (None, Points::One(point)) => Points::One(Point(
                                if sx {
                                    apply_if_some(map_x, new_pos.0)
                                } else {
                                    point.0
                                },
                                if sy {
                                    apply_if_some(map_y, new_pos.1)
                                } else {
                                    point.1
                                },
                            )),
                            _ => unreachable!("Misaligned index and points"),
                        };

                        let pairs = match ident {
                            DragIdent::Point { ident, x, y } => {
                                vec![(ident, VarValue::from(new_value))]
                            }
                            DragIdent::XY { x, y } => {
                                let (vx, vy) = new_value.to_coords();
                                let mut result = Vec::with_capacity(2);
                                result.extend(x.map(|id| (id.param(), VarValue::from(vx))));
                                result.extend(y.map(|id| (id.param(), VarValue::from(vy))));
                                result
                            }
                        };

                        Some(ActValue::new(pairs))
                    }
                    _ => None,
                }
            };
            if eval_open {
                render_eval_view(&mut d, &font, &context, eval_scroll_amt);
            }

            d.draw_fps(0, 0);

            if let Some(act_val) = clickable_val {
                // println!("{act_val:?}");

                statements.remove_calculations(act_val.pairs.iter().map(|v| v.0));
                context.apply_act_value(act_val);
            }
        };

        if let Some(act_val) = act_val {
            // println!("{act_val:?}");

            statements.remove_calculations(act_val.pairs.iter().map(|v| v.0));
            context.apply_act_value(act_val);
        }

        context.apply_bounds(&bounds);
    }
}
