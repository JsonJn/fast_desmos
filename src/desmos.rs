use regex::Regex;
use std::collections::HashMap;
use std::fs;
use std::path::Path;

use reqwest::Url;

use crate::desmos::evaluate::{Color, EvalExpr, ToEval};
use crate::desmos::execute::actions::{ActExpr, ToActExpr};
use crate::desmos::parsing::{Lexer, Parser, Statement};

pub mod evaluate;
pub mod execute;
pub mod parsing;
pub mod rendering;

use crate::desmos::rendering::drawables::{
    DrawColor, FillOptions, Label, LineOptions, ParametricDomain, PointOptions,
};
use rendering::drawables;

#[derive(Debug, Copy, Clone, Default)]
pub enum OnOffState {
    #[default]
    Default,
    On,
    Off,
}

impl OnOffState {
    pub fn on_then<R>(self, value: R) -> Option<R> {
        match self {
            Self::On => Some(value),
            _ => None,
        }
    }

    pub fn off_then<R>(self, value: R) -> Option<R> {
        match self {
            Self::Off => Some(value),
            _ => None,
        }
    }

    pub fn on_or_default_then<R>(self, value: R) -> Option<R> {
        match self {
            Self::On | Self::Default => Some(value),
            _ => None,
        }
    }

    pub fn off_or_default_then<R>(self, value: R) -> Option<R> {
        match self {
            Self::Off | Self::Default => Some(value),
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
    pub point_on_off: OnOffState,
    pub point_options: PointOptions,
    pub line_on_off: OnOffState,
    pub line_options: LineOptions,
    pub fill_on_off: OnOffState,
    pub fill_options: FillOptions,
}

#[derive(Debug)]
pub struct Desmos {
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
    println!("{text}");
    let text = Lexer::lex(text).unwrap().0;
    Parser::parse_expr(text).unwrap().to_eval()
}

fn parse_expr_v_option(s: &serde_json::Value) -> Option<EvalExpr> {
    let text = s.as_str()?.to_string();
    println!("{text}");
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
    let (lexed, _) = Lexer::lex(string).unwrap();
    // println!("{lexed:?}");
    Parser::parse(lexed).unwrap()
}

fn parse_act_expr(s: &serde_json::Value) -> ActExpr {
    Parser::parse_action_expr(Lexer::lex(s.as_str().unwrap().to_string()).unwrap().0)
        .unwrap()
        .to_act_expr()
}

impl Desmos {
    pub fn from_url(url: &str) -> Option<Self> {
        const TITLE_CACHE: &str = "titles.json";
        const WEB_CACHE: &str = "web_cache";
        if !Path::new(WEB_CACHE).exists() {
            fs::create_dir(WEB_CACHE).unwrap();
        }

        let url = Url::parse(url).unwrap();
        let ident = url
            .path_segments()
            .unwrap()
            .next_back()
            .unwrap()
            .to_string();

        let cache_path_str = format!("{WEB_CACHE}/{ident}.json");
        let cache_path = Path::new(&cache_path_str);
        let page = if cache_path.exists() {
            fs::read_to_string(cache_path).unwrap()
        } else {
            let url = format!("https://www.desmos.com/calc-states/production/{ident}");
            let result = reqwest::blocking::get(url).unwrap().text().unwrap();
            fs::write(cache_path, result.clone()).unwrap();
            result
        };

        let mut titles: HashMap<String, String> = Path::new(&TITLE_CACHE)
            .exists()
            .then(|| serde_json::from_str(&fs::read_to_string(TITLE_CACHE).unwrap()).unwrap())
            .unwrap_or_default();

        let title = if titles.contains_key(&ident) {
            titles[&ident].clone()
        } else {
            let pattern =
                Regex::new("<meta property=\"og:title\" content=\"([^\"]*)\" />").unwrap();

            let html = reqwest::blocking::get(url).unwrap().text().unwrap();

            let mat = pattern.captures(&html).expect("Title not found.");
            let title = mat.get(1).unwrap().as_str().to_string();

            titles.insert(ident.to_string(), title.clone());
            let to_write = serde_json::to_string(&titles).unwrap();
            fs::write(TITLE_CACHE, to_write).unwrap();

            title
        };

        let value: serde_json::Value = serde_json::from_str(&page).unwrap();
        let value = value.as_object()?;

        let viewport = value
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

        let expressions = value.get("expressions")?.as_object()?;
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

                    // Line options
                    let line_on_off = expr.get("lines").map(|v| v.as_bool().unwrap()).into();

                    let line_options = {
                        let opacity = expr.get("lineOpacity").map(parse_expr_v);
                        let thickness = expr.get("lineWidth").map(parse_expr_v);
                        LineOptions::from_options(thickness, opacity, None)
                    };

                    // Fill options
                    let fill_on_off = expr.get("fill").map(|v| v.as_bool().unwrap()).into();
                    let fill_options = {
                        let opacity = expr.get("fillOpacity").map(parse_expr_v);
                        FillOptions::from_options(opacity)
                    };

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

                    let clickable = expr.get("clickableInfo").map(|v| Clickable {
                        expr: parse_act_expr(v.as_object().unwrap().get("latex").unwrap()),
                    });

                    Some(DesmosCell {
                        statement,
                        draw_color,
                        options: Options {
                            hidden,
                            point_options,
                            line_options,
                            fill_options,
                            point_on_off,
                            line_on_off,
                            fill_on_off,
                        },
                        domain,
                        clickable,
                    })
                } else {
                    None
                }
            })
            .collect();

        Some(Desmos {
            cells,
            title,
            min_steps_ms,
            viewport,
            ticker,
        })
    }
}
