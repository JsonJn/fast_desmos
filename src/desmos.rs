use std::fs;
use std::io::{stdout, Write};
use std::path::Path;
use std::sync::mpsc::Sender;

use eyre::{ContextCompat, OptionExt, Result, WrapErr};
use regex::Regex;
use serde::{Deserialize, Serialize};
use ureq;
use ureq::Response;
use url::Url;

use crate::desmos::evaluate::{Color, EvalExpr, ToEval};
use crate::desmos::execute::actions::{ActExpr, ToActExpr};
use crate::desmos::parsing::{Lexer, Parser, Statement};
use crate::desmos::rendering::drawables::{
    DrawColor, FillOptions, Label, LineOptions, ParametricDomain, PointOptions,
};

pub mod evaluate;
pub mod execute;
pub mod parsing;
pub mod rendering;
mod value;

pub struct OnOff<T> {
    state: OnOffState,
    data: T,
}

mod sealed {
    use std::fmt::{Debug, Formatter};

    use super::*;

    #[allow(dead_code)]
    #[derive(Debug)]
    struct OnOffDebug<'a, T: Debug> {
        state: &'a OnOffState,
        data: &'a T,
    }

    impl<T: Debug> Debug for OnOff<T> {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            OnOffDebug {
                state: &self.state,
                data: &self.data,
            }
            .fmt(f)
        }
    }
}

#[derive(Debug, Copy, Clone, Default)]
pub enum OnOffState {
    #[default]
    Default,
    On,
    Off,
}

#[allow(dead_code)]
impl<T> OnOff<T> {
    pub fn new(state: OnOffState, data: T) -> Self {
        Self { state, data }
    }

    pub fn as_ref(&self) -> OnOff<&T> {
        OnOff::new(self.state, &self.data)
    }

    pub fn on(self) -> Option<T> {
        match self.state {
            OnOffState::On => Some(self.data),
            _ => None,
        }
    }

    pub fn off(self) -> Option<T> {
        match self.state {
            OnOffState::Off => Some(self.data),
            _ => None,
        }
    }

    pub fn on_or_default(self) -> Option<T> {
        match self.state {
            OnOffState::On | OnOffState::Default => Some(self.data),
            _ => None,
        }
    }

    pub fn off_or_default(self) -> Option<T> {
        match self.state {
            OnOffState::Off | OnOffState::Default => Some(self.data),
            _ => None,
        }
    }
}

impl From<bool> for OnOffState {
    fn from(value: bool) -> Self {
        match value {
            true => Self::On,
            false => Self::Off,
        }
    }
}

impl From<Option<bool>> for OnOffState {
    fn from(value: Option<bool>) -> Self {
        value.map_or_else(Self::default, Self::from)
    }
}

#[derive(Serialize, Deserialize)]
pub struct WebCache {
    pub id: String,
    pub data: CacheData,
}

#[derive(Serialize, Deserialize)]
pub struct CacheData {
    pub page: String,
    pub title: String,
}

impl WebCache {
    const PATH: &'static str = "web_cache.json";

    pub fn cached() -> Option<Self> {
        let path = Path::new(Self::PATH);
        path.exists()
            .then_some(())
            .and_then(|_| serde_json::from_str(&fs::read_to_string(path).ok()?).ok())
    }

    pub fn cached_or_else(id: &str, or: impl FnOnce() -> Option<CacheData>) -> Option<Self> {
        let x = Self::cached();
        if x.as_ref().is_some_and(|cache| cache.id == id) {
            Some(x.unwrap())
        } else {
            let value = or();
            if let Some(or) = value {
                Some(Self {
                    id: id.to_string(),
                    data: or,
                })
            } else {
                None
            }
        }
    }
}

impl Drop for WebCache {
    fn drop(&mut self) {
        let string = serde_json::to_string(&self).unwrap();
        fs::write(Self::PATH, string.into_bytes()).unwrap();
    }
}

#[derive(Default, Debug, Copy, Clone)]
pub struct StoredDragMode {
    x: bool,
    y: bool,
}

#[derive(Debug)]
pub struct SliderBounds {
    pub min: Option<EvalExpr>,
    pub max: Option<EvalExpr>,
    pub step: Option<EvalExpr>,
}

#[derive(Debug)]
pub struct DesmosCell {
    pub statement: Statement,
    pub draw_color: DrawColor,
    pub options: Options,
    pub domain: Option<ParametricDomain>,
    pub clickable: Option<Clickable>,
    pub slider: Option<SliderBounds>,
}

#[derive(Debug)]
pub struct Options {
    pub hidden: bool,
    pub point_options: OnOff<PointOptions>,
    pub line_options: OnOff<LineOptions>,
    pub fill_options: OnOff<FillOptions>,
    pub drag_mode: Option<StoredDragMode>,
}

#[derive(Debug)]
pub struct DesmosPage {
    pub cells: Vec<DesmosCell>,
    pub title: String,
    pub min_steps_ms: Option<f64>,
    pub viewport: Viewport,
    pub ticker: Option<Ticker>,
}

#[derive(Debug, Clone)]
pub struct Viewport {
    pub x_range: (f64, f64),
    pub y_range: (f64, f64),
}

#[derive(Debug)]
pub struct Ticker {
    pub expr: ActExpr,
}

#[derive(Debug)]
pub struct Clickable {
    pub expr: ActExpr,
}

fn parse_expr_v(s: &serde_json::Value) -> Result<EvalExpr> {
    let text = s.as_str().wrap_err("Not a string")?.to_string();
    // println!("{text}");
    let text = Lexer::lex(text).wrap_err("Lexing failed")?.0;
    Parser::parse_expr(text).map(ToEval::to_eval)
}

fn parse_stmt_v(s: &serde_json::Value) -> Result<Statement> {
    let string = s.as_str().wrap_err("Not a string")?.to_string();
    // if string.len() < 1000 {
    //     println!("{string}");
    // } else {
    //     println!("too long!");
    // }
    // println!("{string}");
    let (lexed, _) = Lexer::lex(string).wrap_err("Lexing failed")?;
    // println!("{lexed:?}");
    Parser::parse(lexed)
}

fn parse_act_expr(s: &serde_json::Value) -> Result<ActExpr> {
    let s = s.as_str().wrap_err("Not a string")?.to_string();
    // println!("{s:?}");
    Parser::parse_action_expr(Lexer::lex(s).wrap_err("Lexing failed")?.0)
        .map(ToActExpr::to_act_expr)
}

impl DesmosPage {
    pub fn from_url(url: Url, updates: Option<Sender<String>>) -> Result<Self> {
        let send = |msg| {
            if let Some(updates) = &updates {
                let _ = updates.send(msg);
            }
        };

        send("Starting parse procedure.".to_string());

        fn try_get_page(url: &Url, send: impl Fn(String)) -> Option<Response> {
            let mut request_count = 0;
            loop {
                send(if request_count == 0 {
                    "Requesting...".to_string()
                } else {
                    format!("Retrying request, attempt {request_count}")
                });
                let _ = stdout().flush();
                let get = ureq::request_url("GET", url).call();

                request_count += 1;
                match get {
                    Ok(x) => {
                        send("Success!".to_string());
                        break Some(x);
                    }
                    Err(e) => {
                        send(format!("Network error: {e}"));
                    }
                }
            }
        }

        let ident = url
            .path_segments()
            .and_then(|x| x.rev().next())
            .ok_or_eyre("No last path segment was found.")?;

        send(format!("uid is {ident}"));

        let cached = &WebCache::cached_or_else(&ident, || {
            let json_url = format!("https://www.desmos.com/calc-states/production/{ident}");
            let json_url = Url::parse(&json_url).unwrap_or_else(|_| unreachable!());
            send("Downloading json data...".to_string());
            let page = try_get_page(&json_url, send)?.into_string().unwrap();

            let pattern = Regex::new(r#"<meta property="og:title" content="([^"]*)" />"#).unwrap();

            send("Downloading HTML for page title...".to_string());
            let html = try_get_page(&url, send)?.into_string().unwrap();
            let mat = pattern.captures(&html).expect("Title not found.");
            let title = mat.get(1).unwrap().as_str().to_string();

            Some(CacheData { page, title })
        })
        .wrap_err("Failed to obtain data.")?
        .data;

        let title = cached.title.clone();

        let json: serde_json::Value = serde_json::from_str(&cached.page).unwrap();
        let json = json.as_object().wrap_err("JSON wasn't an object")?;

        let viewport = json
            .get("graph")
            .wrap_err("JSON doesn't contain key `graph`")?
            .as_object()
            .wrap_err("`graph` isn't an object")?
            .get("viewport")
            .wrap_err("`graph` doesn't contain key `viewport`")?
            .as_object()
            .wrap_err("`viewport` isn't an object")?;
        let [x_min, x_max, y_min, y_max] = ["xmin", "xmax", "ymin", "ymax"].map(|key| {
            viewport
                .get(key)
                .unwrap()
                .as_number()
                .unwrap()
                .as_f64()
                .unwrap()
        });

        let viewport = Viewport {
            x_range: (x_min, x_max),
            y_range: (y_min, y_max),
        };

        let expressions = json
            .get("expressions")
            .wrap_err("JSON doesn't contain key `expressions`")?
            .as_object()
            .wrap_err("`expressions` isn't an object")?;
        let expr_list = expressions
            .get("list")
            .wrap_err("`expressions` doesn't contain key `list`")?
            .as_array()
            .wrap_err("`list` isn't an array")?;

        let ticker = expressions.get("ticker").and_then(|x| x.as_object());

        let min_steps_ms = ticker.and_then(|ticker| {
            Some(
                ticker
                    .get("minStepLatex")?
                    .as_str()
                    .unwrap()
                    .parse()
                    .unwrap(),
            )
        });

        /// Takes as input an expr returning `Option<eyre::Result<T>>`
        /// and returns the program, optionally wrapping, if it is
        /// `Some(Err(_))`
        macro_rules! fail {
            ($expr:expr, $msg:expr) => {{
                use eyre::WrapErr as _;
                if let Some(x) = $expr {
                    Some(x.wrap_err($msg)?)
                } else {
                    None
                }
            }};
            ($expr:expr) => {{
                if let Some(x) = $expr {
                    Some(x?)
                } else {
                    None
                }
            }};
        }

        let ticker = fail!(
            ticker.map(|ticker| ticker
                .get("handlerLatex")
                .wrap_err("`ticker` doesn't contain `handlerLatex`")
                .and_then(parse_act_expr)),
            "Ticker obtaining failed"
        )
        .map(|ticker| Ticker { expr: ticker });

        let mut cells = Vec::new();
        for expr in expr_list {
            let expr = expr.as_object().wrap_err("cell isn't an object")?;

            let kind = expr
                .get("type")
                .wrap_err("cell has no type")?
                .as_str()
                .wrap_err("cell type isn't a string")?;

            if kind == "expression" && expr.get("latex").is_some() {
                let cell = (|| -> Result<_> {
                    let statement = parse_stmt_v(
                        expr.get("latex")
                            .unwrap_or_else(|| unreachable!("Checked by outer if statement")),
                    )
                    .wrap_err("expr latex couldn't be parsed")?;

                    // Color
                    let color = Color::from_hex(
                        expr.get("color")
                            .wrap_err("expr has no color")?
                            .as_str()
                            .unwrap(),
                    );
                    let latex = fail!(
                        expr.get("colorLatex").map(parse_expr_v),
                        "expr colorLatex couldn't be parsed"
                    );
                    let draw_color = DrawColor { color, latex };

                    // Point options
                    let point_on_off = expr.get("points").map(|v| v.as_bool().unwrap()).into();
                    let point_options = {
                        let opacity = fail!(
                            expr.get("pointOpacity").map(parse_expr_v),
                            "pointOpacity couldn't be parsed"
                        );
                        let diameter = fail!(
                            expr.get("pointSize").map(parse_expr_v),
                            "pointSize couldn't be parsed"
                        );

                        let label = expr
                            .get("showLabel")
                            .map(|v| v.as_bool().unwrap())
                            .unwrap_or(false);

                        let label = if label {
                            let label = expr.get("label").map(|v| v.as_str().unwrap().to_string());
                            let label_size = fail!(
                                expr.get("labelSize").map(parse_expr_v),
                                "labelSize couldn't be parsed"
                            );
                            let label_angle = fail!(
                                expr.get("labelAngle").map(parse_expr_v),
                                "labelAngle couldn't be parsed"
                            );

                            label.map(|i| {
                                Label::from_options(Some(i.clone()), label_size, label_angle)
                            })
                        } else {
                            None
                        };

                        PointOptions::from_options(diameter, opacity, label.map(Some), None)
                    };
                    let point_options = OnOff::new(point_on_off, point_options);

                    // Line options
                    let line_on_off = expr.get("lines").map(|v| v.as_bool().unwrap()).into();

                    let line_options = {
                        let opacity = fail!(
                            expr.get("lineOpacity").map(parse_expr_v),
                            "lineOpacity couldn't be parsed"
                        );
                        let thickness = fail!(
                            expr.get("lineWidth").map(parse_expr_v),
                            "lineWidth couldn't be parsed"
                        );
                        LineOptions::from_options(thickness, opacity, None)
                    };
                    let line_options = OnOff::new(line_on_off, line_options);

                    // Fill options
                    let fill_on_off = expr.get("fill").map(|v| v.as_bool().unwrap()).into();
                    let fill_options = {
                        let opacity = fail!(
                            expr.get("fillOpacity").map(parse_expr_v),
                            "fillOpacity couldn't be parsed"
                        );
                        FillOptions::from_options(opacity)
                    };
                    let fill_options = OnOff::new(fill_on_off, fill_options);

                    // Parametric Domain
                    let domain = expr.get("parametricDomain").map(|v| {
                        let obj = v.as_object().unwrap();
                        let min = obj.get("min").and_then(|x| parse_expr_v(x).ok());
                        let max = obj.get("max").and_then(|x| parse_expr_v(x).ok());
                        ParametricDomain::from_options(min, max)
                    });

                    let hidden = expr
                        .get("hidden")
                        .map(|v| v.as_bool().unwrap())
                        .unwrap_or(false);

                    let clickable = fail!(expr.get("clickableInfo").map(|v| -> Result<_> {
                        let clickable = v.as_object().unwrap();
                        // println!("{clickable:?}");
                        Ok(Clickable {
                            expr: parse_act_expr(
                                clickable.get("latex").wrap_err("Clickable has no latex")?,
                            )
                            .wrap_err("Clickable latex couldn't be parsed")?,
                        })
                    }));

                    // Drag Mode
                    let drag_mode = expr.get("dragMode").map(|v| {
                        let name = v.as_str().unwrap();
                        match name {
                            "NONE" => StoredDragMode { x: false, y: false },
                            "X" => StoredDragMode { x: true, y: false },
                            "Y" => StoredDragMode { x: false, y: true },
                            "XY" => StoredDragMode { x: true, y: true },
                            _what => unreachable!("Unknown drag mode: {_what:?}"),
                        }
                    });

                    // Slider bounds
                    let slider = fail!(expr.get("slider").map(|v| -> Result<_> {
                        let obj = v.as_object().unwrap();

                        fn is_true_at(
                            x: &serde_json::Map<String, serde_json::Value>,
                            name: &str,
                        ) -> bool {
                            x.get(name).map(|v| v.as_bool().unwrap()).unwrap_or(false)
                        }

                        let step = fail!(
                            obj.get("step").map(parse_expr_v),
                            "slider step couldn't be parsed"
                        );
                        let min = fail!(
                            is_true_at(obj, "hardMin")
                                .then(|| obj.get("min").map(parse_expr_v))
                                .flatten(),
                            "slider min couldn't be parsed"
                        );
                        let max = fail!(
                            is_true_at(obj, "hardMax")
                                .then(|| obj.get("max").map(parse_expr_v))
                                .flatten(),
                            "slider max couldn't be parsed"
                        );

                        Ok(SliderBounds { min, max, step })
                    }));

                    Ok(DesmosCell {
                        statement,
                        draw_color,
                        options: Options {
                            hidden,
                            point_options,
                            line_options,
                            fill_options,
                            drag_mode,
                        },
                        domain,
                        clickable,
                        slider,
                    })
                })()
                .wrap_err_with(|| format!("expr couldn't be parsed: {expr:?}"))?;

                cells.push(cell);
            }
        }

        Ok(DesmosPage {
            cells,
            title,
            min_steps_ms,
            viewport,
            ticker,
        })
    }
}
