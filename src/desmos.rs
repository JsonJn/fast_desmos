use regex::Regex;
use std::fs;
use std::path::Path;

use reqwest::Url;
use serde::{Deserialize, Serialize};

use crate::desmos::evaluate::{Color, EvalExpr, ToEval};
use crate::desmos::execute::actions::{ActExpr, ToActExpr};
use crate::desmos::parsing::{Lexer, Parser, Statement};

pub mod evaluate;
pub mod execute;
pub mod parsing;
pub mod rendering;
mod value;

use crate::desmos::rendering::drawables::{
    DrawColor, FillOptions, Label, LineOptions, ParametricDomain, PointOptions,
};

pub struct OnOff<T> {
    state: OnOffState,
    data: T,
}

mod sealed {
    use super::*;
    use std::fmt::{Debug, Formatter};

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

    pub fn cached_or_else(id: &str, or: impl FnOnce() -> CacheData) -> Self {
        let x = Self::cached();
        if x.as_ref().is_some_and(|cache| cache.id == id) {
            x.unwrap()
        } else {
            Self {
                id: id.to_string(),
                data: or(),
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
pub struct DesmosCell {
    pub statement: Statement,
    pub draw_color: DrawColor,
    pub options: Options,
    pub domain: Option<ParametricDomain>,
    pub clickable: Option<Clickable>,
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

fn parse_expr(s: String) -> EvalExpr {
    Parser::parse_expr(Lexer::lex(s).unwrap().0)
        .unwrap()
        .to_eval()
}

fn parse_expr_v(s: &serde_json::Value) -> EvalExpr {
    let text = s.as_str().unwrap().to_string();
    // println!("{text}");
    let text = Lexer::lex(text).unwrap().0;
    Parser::parse_expr(text).unwrap().to_eval()
}

fn parse_expr_v_option(s: &serde_json::Value) -> Option<EvalExpr> {
    let text = s.as_str()?.to_string();
    // println!("{text}");
    let text = Lexer::lex(text)?.0;
    Some(Parser::parse_expr(text)?.to_eval())
}

fn parse_stmt_v(s: &serde_json::Value) -> Statement {
    let string = s.as_str().unwrap().to_string();
    if string.len() < 1000 {
        println!("{string}");
    } else {
        println!("too long!");
    }
    // println!("{string}");
    let (lexed, _) = Lexer::lex(string).expect("Statement lexing failed!");
    // println!("{lexed:?}");
    Parser::parse(lexed).expect("Statement parsing failed!")
}

fn parse_act_expr(s: &serde_json::Value) -> ActExpr {
    let s = s.as_str().unwrap().to_string();
    println!("{s:?}");
    Parser::parse_action_expr(Lexer::lex(s).unwrap().0)
        .expect("ActExpr parsing failed!")
        .to_act_expr()
}

impl DesmosPage {
    pub fn from_url(url: &str) -> Option<Self> {
        let url = Url::parse(url).unwrap();
        let ident = url
            .path_segments()
            .unwrap()
            .next_back()
            .unwrap()
            .to_string();

        let cached = &WebCache::cached_or_else(&ident, || {
            let json_url = format!("https://www.desmos.com/calc-states/production/{ident}");
            let page = reqwest::blocking::get(json_url).unwrap().text().unwrap();

            let pattern =
                Regex::new("<meta property=\"og:title\" content=\"([^\"]*)\" />").unwrap();

            let mut request_count = 0;
            let html = loop {
                request_count += 1;
                println!("Downloading webpage attempt {request_count}");
                let get = reqwest::blocking::get(url.clone());
                if let Ok(x) = get {
                    break x;
                } else if request_count >= 20 {
                    panic!("Too many retries: greater than 20.")
                }
            }
            .text()
            .unwrap();

            let mat = pattern.captures(&html).expect("Title not found.");
            let title = mat.get(1).unwrap().as_str().to_string();

            CacheData { page, title }
        })
        .data;

        let title = cached.title.clone();

        let json: serde_json::Value = serde_json::from_str(&cached.page).unwrap();
        let json = json.as_object()?;

        let viewport = json
            .get("graph")?
            .as_object()?
            .get("viewport")?
            .as_object()?;
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

        let expressions = json.get("expressions")?.as_object()?;
        let expr_list = expressions.get("list")?.as_array()?;

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

        let ticker = ticker.map(|ticker| Ticker {
            expr: parse_act_expr(ticker.get("handlerLatex").unwrap()),
        });

        let cells = expr_list
            .iter()
            .filter_map(|expr| {
                let expr = expr.as_object()?;

                let kind = expr.get("type")?.as_str()?;

                if kind == "expression" {
                    let statement = parse_stmt_v(expr.get("latex")?);

                    // Color
                    let color = Color::from_hex(expr.get("color")?.as_str().unwrap());
                    let latex = expr.get("colorLatex").map(parse_expr_v);
                    let draw_color = DrawColor { color, latex };

                    // Point options
                    let point_on_off = expr.get("points").map(|v| v.as_bool().unwrap()).into();
                    let point_options = {
                        let opacity = expr.get("pointOpacity").map(parse_expr_v);
                        let diameter = expr.get("pointSize").map(parse_expr_v);

                        let label = expr
                            .get("showLabel")
                            .map(|v| v.as_bool().unwrap())
                            .unwrap_or(false);

                        let label = label.then(|| {
                            let label = expr.get("label").map(|v| v.as_str().unwrap().to_string());
                            let label_size = expr.get("labelSize").map(parse_expr_v);
                            let label_angle = expr.get("labelAngle").map(parse_expr_v);

                            label.map(|i| Label::from_options(Some(i), label_size, label_angle))
                        });

                        PointOptions::from_options(diameter, opacity, label, None)
                    };
                    let point_options = OnOff::new(point_on_off, point_options);

                    // Line options
                    let line_on_off = expr.get("lines").map(|v| v.as_bool().unwrap()).into();

                    let line_options = {
                        let opacity = expr.get("lineOpacity").map(parse_expr_v);
                        let thickness = expr.get("lineWidth").map(parse_expr_v);
                        LineOptions::from_options(thickness, opacity, None)
                    };
                    let line_options = OnOff::new(line_on_off, line_options);

                    // Fill options
                    let fill_on_off = expr.get("fill").map(|v| v.as_bool().unwrap()).into();
                    let fill_options = {
                        let opacity = expr.get("fillOpacity").map(parse_expr_v);
                        FillOptions::from_options(opacity)
                    };
                    let fill_options = OnOff::new(fill_on_off, fill_options);

                    // Parametric Domain
                    let domain = expr.get("parametricDomain").map(|v| {
                        let obj = v.as_object().unwrap();
                        let min = obj.get("min").and_then(parse_expr_v_option);
                        let max = obj.get("max").and_then(parse_expr_v_option);
                        ParametricDomain::from_options(min, max)
                    });

                    let hidden = expr
                        .get("hidden")
                        .map(|v| v.as_bool().unwrap())
                        .unwrap_or(false);

                    let clickable = expr.get("clickableInfo").and_then(|v| {
                        let clickable = v.as_object().unwrap();
                        println!("{clickable:?}");
                        Some(Clickable {
                            expr: parse_act_expr(clickable.get("latex")?),
                        })
                    });

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

                    Some(DesmosCell {
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
                    })
                } else {
                    None
                }
            })
            .collect();

        Some(DesmosPage {
            cells,
            title,
            min_steps_ms,
            viewport,
            ticker,
        })
    }
}
