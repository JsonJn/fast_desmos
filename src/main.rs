// #![allow(dead_code)]
#![warn(clippy::semicolon_if_nothing_returned, clippy::uninlined_format_args)]

mod desmos;
mod gamma;
mod ui;

mod iterator;
mod pooled_vec;
#[cfg(test)]
mod tests;
mod vec_map;
mod vecs;

macro_rules! urls {
    (
        $($name:ident = $str:literal);*
        $(;)?
    ) => {
        $(
        #[allow(dead_code)]
        const $name: &str = $str;
        )*
    };
}

fn main() {
    // use desmos::parsing::{Lexer, Parser};
    // let raw = r#"\left\{\left\{\left|p_{new}.x\right|>L\right\}\left\{v.xp_{new}.x>0\right\}=1:-1,1\right\}"#;
    // let (lexed, _left) = Lexer::lex(raw.to_string()).unwrap();
    // let parsed = Parser::parse(lexed).unwrap();
    // println!("{parsed:#?}");
    // return;

    // use desmos::evaluate::{CanLinear, EvalKind, ToEval};
    // use desmos::parsing::{Lexer, Parser, Statement};
    // let raw = r#"\left(-3,4+4R\right)"#;
    // let (lexed, _left) = Lexer::lex(raw.to_string()).unwrap();
    // let parsed = Parser::parse(lexed).unwrap();
    // let expr = take_pat!(parsed => x from Statement::Expression(x));
    // let eval = expr.to_eval();
    // let EvalKind::Point { x, y } = &eval.expr.kind else {
    //     unreachable!("Test case isn't a point");
    // };
    // let [lin_x, lin_y] = [x, y].map(CanLinear::is_linear);
    // println!("lin_x: {lin_x:?}");
    // println!("lin_y: {lin_y:?}");
    // return;

    // let url = args();
    // let parts: Vec<String> = url.collect();
    //
    // if parts.len() < 2 {
    //     println!("Must prove an argument!");
    //     return;
    // }

    // urls! {
    //     GRAVITY_IDEAL_GAS = "https://www.desmos.com/calculator/dwazp7zubl";
    //     IDEAL_GAS = "https://www.desmos.com/calculator/ht6vnqkrxx";
    //     ANALYTIC_IDEAL_GAS = "https://www.desmos.com/calculator/kjrqurcozt";
    //     SPINNY_CUBE = "https://www.desmos.com/calculator/epretb6vnj";
    //     SNUB_SQUARE = "https://www.desmos.com/calculator/jroixxyk1e";
    //     VECTOR_FIELD = "https://www.desmos.com/calculator/g25eqqco4m";
    //     TIC_TAC_TOE = "https://www.desmos.com/calculator/le77826l07";
    //     POINT_SIZES = "https://www.desmos.com/calculator/xaee0lvsnc";
    //     MANY_POINT_SIZES = "https://www.desmos.com/calculator/oaho9ryxo1";
    //     SPECIAL_CONNECT_4 = "https://www.desmos.com/calculator/2f5sqp3fok";
    //     FLICKERING_POINTS = "https://www.desmos.com/calculator/xbebofu2lk";
    //     BLINDMOKU = "https://www.desmos.com/calculator/w6dk818fbz";
    //     D20 = "https://www.desmos.com/calculator/y0s6d3xy4t";
    // }
    //
    let page = ui::input_url(2);
    desmos::rendering::window(page);

    // println!("{:?}", size_of::<EvalTree>());
    // pooled_tests();
}
