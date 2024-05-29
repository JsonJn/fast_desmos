// #![allow(dead_code)]
#![warn(clippy::semicolon_if_nothing_returned, clippy::uninlined_format_args)]

use crate::desmos::Desmos;

mod desmos;
mod gamma;

mod iterator;
mod pooled_vec;
#[cfg(test)]
mod tests;
mod vecs;

macro_rules! strings {
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
    // let raw = r#"\operatorname{polygon}\left(\left(0.5+S_{top},-0.5+P_{s}\left[I_{addLoc}\right].y\right),\left(S_{top}-0.5,-0.5+P_{s}\left[I_{addLoc}\right].y\right),\left(S_{top}-0.5,H+0.5\right),\left(S_{top}+0.5,H+0.5\right)\right)"#.to_string();
    // let (lexed, left) = Lexer::lex(raw).unwrap();
    // let parsed = Parser::parse(lexed).unwrap();
    // println!("{parsed:#?}");
    // return;

    // let url = args();
    // let parts: Vec<String> = url.collect();
    //
    // if parts.len() < 2 {
    //     println!("Must prove an argument!");
    //     return;
    // }

    strings! {
        GRAVITY_IDEAL_GAS = "https://www.desmos.com/calculator/dwazp7zubl";
        IDEAL_GAS = "https://www.desmos.com/calculator/ht6vnqkrxx";
        ANALYTIC_IDEAL_GAS = "https://www.desmos.com/calculator/kjrqurcozt";
        SPINNY_CUBE = "https://www.desmos.com/calculator/epretb6vnj";
        SNUB_SQUARE = "https://www.desmos.com/calculator/jroixxyk1e";
        VECTOR_FIELD = "https://www.desmos.com/calculator/g25eqqco4m";
        TIC_TAC_TOE = "https://www.desmos.com/calculator/le77826l07";
        POINT_SIZES = "https://www.desmos.com/calculator/xaee0lvsnc";
        SPECIAL_CONNECT_4 = "https://www.desmos.com/calculator/n1ezl89mto";
    }

    let desmos = Desmos::from_url(SPECIAL_CONNECT_4).unwrap();
    desmos::rendering::window(desmos);

    // println!("{:?}", size_of::<EvalTree>());
    // pooled_tests();
}
