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
    // let raw = r#"A_{cc}=0.01\sum_{n=1}^{N}\left\{n=I:\left(0,0\right),\frac{p\left[n\right]-p\left[I\right]}{\max\left(0.01,\left|p\left[n\right]-p\left[I\right]\right|^{3}\right)}\right\}"#.to_string();
    // let (lexed, left) = Lexer::lex(raw).unwrap();
    // let parsed = Parser::parse(lexed).unwrap();
    // println!("{parsed:#?}");

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
        SNUB_CUBE = "https://www.desmos.com/calculator/jroixxyk1e";
    }

    let desmos = Desmos::from_url(SPINNY_CUBE).unwrap();
    desmos::rendering::window(desmos);

    // println!("{:?}", size_of::<EvalTree>());
    // pooled_tests();
}
