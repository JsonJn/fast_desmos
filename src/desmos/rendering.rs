use std::time::Instant;

use raylib::ffi;
use raylib::prelude::{Camera2D, KeyboardKey, RaylibDraw, RaylibMode2DExt, Vector2};

use crate::desmos::evaluate::{
    Color, Colors, Conditional, EvalKind, IdentifierStorer, Numbers, Point, Polygon, VarValue,
};
use crate::desmos::execute::convert_cells;
use crate::desmos::parsing::InequalityType;
use crate::desmos::rendering::drawables::{
    DrawColor, DrawPoints, DrawPolygons, Drawable, DrawableType, ExplicitEq, ExplicitType,
    FillOptions, LineOptions, PointOptions,
};
use crate::desmos::{Desmos, Viewport};

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

#[allow(unused)]
pub fn window(params: Desmos) {
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

    let Desmos {
        title,
        min_steps_ms,
        cells,
        viewport,
        ticker,
    } = params;

    let (mut context, mut statements) = convert_cells(cells);

    let (mut rl, thread) = raylib::init()
        .size(WIDTH, HEIGHT)
        .title(&title)
        .msaa_4x()
        .build();

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

    let mut frame_counter: u128 = 0;
    let last_frame_start = Instant::now();
    while !rl.window_should_close() {
        frame_counter += 1;
        if frame_counter % 30 == 0 {
            let took = last_frame_start.elapsed().as_secs_f32();
            let fps = (frame_counter as f32) / took;
            println!("fps: {}", fps.round());
        }

        // let execute_start = Instant::now();
        let drawables = statements.cycle(&mut context);
        if frame_counter % 1000 == 0 {
            println!("{} drawables on frame {frame_counter}", drawables.len());
        }
        // let execute_elapsed = execute_start.elapsed();
        // println!("Execute took {execute_elapsed:?}");

        let act_val = ticker.as_ref().map(|func| context.evaluate_act(&func.expr));

        let dt = rl.get_frame_time();
        for (key, vec) in KEY_OFFSETS {
            if rl.is_key_down(key) {
                camera.target += vec * MOVE_SPEED * dt / camera.zoom;
            }
        }

        camera.zoom *= SCALE_SPEED.powf(rl.get_mouse_wheel_move());
        {
            let mut d = rl.begin_drawing(&thread);

            d.clear_background(raylib::prelude::Color::WHITE);
            {
                let mut d2 = d.begin_mode2D(camera);

                let drawing_start = Instant::now();

                for drawable in drawables {
                    let Drawable {
                        color,
                        kind,
                        clickable,
                        draw_index,
                    } = drawable;

                    let DrawColor { color, latex } = color;

                    let color = if let Some(latex) = latex {
                        context.evaluate(latex).try_into().unwrap()
                    } else {
                        Colors::One(*color)
                    };

                    #[allow(unused_variables)]
                    match kind {
                        DrawableType::Parametric(_) => todo!("parametric drawing"),
                        DrawableType::Explicit(ExplicitEq {
                            line_options,
                            equation,
                            kind,
                        }) => {
                            if let Some(LineOptions {
                                opacity,
                                kind: line_kind,
                                thickness,
                            }) = line_options
                            {
                                let opacity: Numbers =
                                    context.evaluate(opacity).try_into().unwrap();
                                let thickness: Numbers =
                                    context.evaluate(thickness).try_into().unwrap();

                                let src_ident = match kind {
                                    ExplicitType::YFromX => IdentifierStorer::IDENT_X,
                                    ExplicitType::XFromY => IdentifierStorer::IDENT_Y,
                                };

                                let dst_ident = match kind {
                                    ExplicitType::YFromX => IdentifierStorer::IDENT_Y,
                                    ExplicitType::XFromY => IdentifierStorer::IDENT_X,
                                };

                                let src_expr = EvalKind::Ident(src_ident);

                                let ((min_bound, max_bound), expr) = (|| {
                                    let equation_clone = equation.expr.as_ref();
                                    if let EvalKind::Multiply(exprs) = &equation_clone.kind {
                                        if exprs.len() == 2 {
                                            let first = &exprs[0];
                                            let constraint = &exprs[1];
                                            let (first, constraint) =
                                                (first.expr.as_ref(), constraint.expr.as_ref());
                                            if let EvalKind::IfElse {
                                                cond: Conditional::Inequality { exprs, comp },
                                                yes,
                                                no,
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
                                                            exprs.iter().position(|e| {
                                                                (*e.expr).kind == src_expr
                                                            })
                                                        {
                                                            let left = ind.checked_sub(1);
                                                            let right = ind + 1;
                                                            let right = (right < exprs.len())
                                                                .then_some(right);

                                                            let [left, right]: [Option<Numbers>;
                                                                2] = [left, right].map(|i| {
                                                                i.map(|i| {
                                                                    context
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
                                })(
                                );
                                let (min_bound, max_bound) = (
                                    min_bound.unwrap_or_else(|| {
                                        let corner = Vector2::new(0.0, 0.0);
                                        let undone = d2.get_screen_to_world2D(corner, camera);

                                        Numbers::One(match kind {
                                            ExplicitType::YFromX => undone.x,
                                            ExplicitType::XFromY => undone.y,
                                        }
                                            as f64)
                                    }),
                                    max_bound.unwrap_or_else(|| {
                                        let corner = Vector2::new(WIDTH as f32, HEIGHT as f32);
                                        let undone = d2.get_screen_to_world2D(corner, camera);

                                        Numbers::One(match kind {
                                            ExplicitType::YFromX => undone.x,
                                            ExplicitType::XFromY => undone.y,
                                        }
                                            as f64)
                                    }),
                                );
                                match (min_bound, max_bound) {
                                    (Numbers::One(min), Numbers::One(max)) => {
                                        let [min_v, max_v] = [min, max].map(VarValue::number);
                                        let [min_dest, max_dest]: [Numbers; 2] = [min_v, max_v]
                                            .map(|v| {
                                                context
                                                    .evaluate_with(expr, src_ident, v)
                                                    .try_into()
                                                    .expect("Explicit function must be numbers")
                                            });

                                        match (min_dest, max_dest) {
                                            (Numbers::One(min_dest), Numbers::One(max_dest)) => d2
                                                .draw_line_ex(
                                                    kind.get_point(min, min_dest),
                                                    kind.get_point(max, max_dest),
                                                    thickness.first() as f32 / camera.zoom,
                                                    color.first().with_opacity(opacity.first()),
                                                ),
                                            (min_dest, max_dest) => {
                                                for (
                                                    (min_dest, max_dest),
                                                    ((thickness, opacity), color),
                                                ) in min_dest.into_iter().zip(max_dest).zip(
                                                    thickness.into_iter().zip(opacity).zip(color),
                                                ) {
                                                    d2.draw_line_ex(
                                                        kind.get_point(min, min_dest),
                                                        kind.get_point(max, max_dest),
                                                        thickness as f32 / camera.zoom,
                                                        color.with_opacity(opacity),
                                                    );
                                                }
                                            }
                                        }
                                    }
                                    (min_bound, max_bound) => todo!("Variable bounds"),
                                }
                            }
                        }
                        DrawableType::Points(ps) => {
                            let DrawPoints {
                                points,
                                point_options,
                                line_options,
                            } = ps;

                            if let Some(PointOptions {
                                opacity,
                                kind,
                                diameter,
                                label,
                            }) = point_options
                            {
                                let diameter: Numbers = context
                                    .evaluate(diameter)
                                    .try_into()
                                    .expect("Diameter must be a number");

                                let opacity: Numbers = context
                                    .evaluate(opacity)
                                    .try_into()
                                    .expect("Opacity must be a number");

                                for (&p, ((&diameter, &opacity), &color)) in points
                                    .iter()
                                    .zip(diameter.iter().zip(opacity.iter()).zip(color.iter()))
                                {
                                    const A: f32 = 0.923_879_5;
                                    const B: f32 = std::f32::consts::FRAC_1_SQRT_2;
                                    const C: f32 = 0.382_683_43;
                                    const POINTS_16: [(f32, f32); 16] = [
                                        (1.0, 0.0),
                                        (A, -C),
                                        (B, -B),
                                        (C, -A),
                                        (0.0, -1.0),
                                        (-C, -A),
                                        (-B, -B),
                                        (-A, -C),
                                        (-1.0, 0.0),
                                        (-A, C),
                                        (-B, B),
                                        (-C, A),
                                        (0.0, 1.0),
                                        (C, A),
                                        (B, B),
                                        (A, C),
                                    ];
                                    const POINTS_8: [(f32, f32); 8] = [
                                        (1.0, 0.0),
                                        (B, -B),
                                        (0.0, -1.0),
                                        (-B, -B),
                                        (-1.0, 0.0),
                                        (-B, B),
                                        (0.0, 1.0),
                                        (B, B),
                                    ];

                                    let radius = diameter as f32 / 2.0 / camera.zoom;

                                    let ffi::Vector2 { x: px, y: py } = p.into();

                                    let points: &[(f32, f32)] = if diameter < 5.0 {
                                        &POINTS_8
                                    } else {
                                        &POINTS_16
                                    };

                                    d2.draw_triangle_fan(
                                        &points
                                            .iter()
                                            .map(|(x, y)| {
                                                Vector2::new(px + x * radius, py + y * radius)
                                            })
                                            .collect::<Vec<_>>(),
                                        color.with_opacity(opacity),
                                    );
                                }
                                //TODO: labels
                            }

                            if let Some(LineOptions {
                                opacity,
                                thickness,
                                kind,
                            }) = line_options
                            {
                                let opacity: Numbers = context
                                    .evaluate(opacity)
                                    .try_into()
                                    .expect("Opacity must be a number");

                                let thickness: Numbers = context
                                    .evaluate(thickness)
                                    .try_into()
                                    .expect("Opacity must be a number");

                                //TODO: line type

                                for (ps, ((&opacity, &thickness), color)) in points
                                    .windows(2)
                                    .zip(opacity.iter().zip(thickness.iter()).zip(color))
                                {
                                    let line_color = color.with_opacity(opacity);
                                    let [this, next]: [Point; 2] = ps.try_into().unwrap();
                                    d2.draw_line_ex(
                                        this,
                                        next,
                                        thickness as f32 / camera.zoom,
                                        line_color,
                                    );
                                }
                            }
                        }
                        DrawableType::Polygons(polys) => {
                            let DrawPolygons {
                                polygons,
                                fill_options,
                                line_options,
                            } = polys;

                            if let Some(FillOptions { opacity }) = fill_options {
                                let opacity: Numbers = context
                                    .evaluate(opacity)
                                    .try_into()
                                    .expect("Opacity must be a number");

                                for ((points, &opacity), &color) in
                                    polygons.iter().zip(opacity.iter()).zip(color.iter())
                                {
                                    let fill_color = color.with_opacity(opacity);
                                    let Polygon(points) = points;
                                    let mut vecs = points
                                        .iter()
                                        .map(|&v| -> Vector2 { v.into() })
                                        .collect::<Vec<_>>();

                                    d2.draw_triangle_fan(&vecs, fill_color);
                                    vecs.reverse();
                                    d2.draw_triangle_fan(&vecs, fill_color);
                                }
                            }

                            if let Some(LineOptions {
                                opacity,
                                kind,
                                thickness,
                            }) = line_options
                            {
                                //TODO: line type
                                let opacity: Numbers = context
                                    .evaluate(opacity)
                                    .try_into()
                                    .expect("Opacity must be a number");

                                let thickness: Numbers = context
                                    .evaluate(thickness)
                                    .try_into()
                                    .expect("Opacity must be a number");

                                for (Polygon(points), ((&opacity, &thickness), &color)) in polygons
                                    .into_iter()
                                    .zip(opacity.iter().zip(thickness.iter()).zip(color.iter()))
                                {
                                    let line_color = color.with_opacity(opacity);
                                    for i in 0..points.len() {
                                        let this = points[i];
                                        let next = points[(i + 1) % points.len()];
                                        d2.draw_line_ex(
                                            this,
                                            next,
                                            thickness as f32 / camera.zoom,
                                            line_color,
                                        );
                                    }
                                }
                            }
                        }
                    }
                }

                let drawing_elapsed = drawing_start.elapsed();
                // println!("Drawing took {drawing_elapsed:?}");
            }

            // d.draw_fps(0, 0);
        }

        if let Some(act_val) = act_val {
            statements.remove_calculations(act_val.pairs.iter().map(|v| v.0));
            context.apply_act_value(act_val);
        }
    }
}
