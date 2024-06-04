use super::advancing_vec::{is_alphabet, is_digit, AdvVec};

macro_rules! one_of {
    ($self: ident; $( $wrapper: path = $func: ident,)* ) => {
        $(if let Some(_tmp) = $self.$func() {
            Some($wrapper(_tmp))
        } else)* {
            None
        }
    };
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Punct(Punctuation),
    Number(f64),
    Identifier(String),
}

impl Token {
    fn op_convert(op: String) -> Token {
        let s = op.as_str();
        match s {
            "sin" | "cos" | "tan" | "sec" | "csc" | "cot" | "cosh" | "sinh" | "tanh" | "sech"
            | "csch" | "coth" | "arcsin" | "arccos" | "arctan" | "arccosh" | "arcosh"
            | "arcsinh" | "arctanh" | "arcsech" | "arccsch" | "arcsch" | "arccoth" | "arcoth"
            | "sign" | "mod" | "floor" | "ceil" | "round" | "nCr" | "nPr" | "length" | "join"
            | "unique" | "sort" | "polygon" | "rgb" | "hsv" | "random" | "mean" | "total"
            | "max" | "min" => {
                let mut name = op;
                name.insert(0, '_');
                Token::Identifier(name)
            }

            "pi" | "infty" | "index" | "dt" => Token::Identifier(op),

            _ => Token::Punct(match s {
                "sqrt" => Punctuation::Sqrt,
                "frac" => Punctuation::Frac,
                "sum" => Punctuation::Sum,
                "prod" => Punctuation::Prod,
                "for" => Punctuation::For,
                "with" => Punctuation::With,
                "ge" => Punctuation::MoreOrEqual,
                "le" => Punctuation::LessOrEqual,
                "to" => Punctuation::SimArrow,
                "cdot" => Punctuation::Mult,
                _ => panic!("Unknown operator name: {s:?}"),
            }),
        }
    }
}

#[warn(dead_code)]
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Punctuation {
    LeftParen,
    RightParen,
    LeftSquare,
    RightSquare,
    LeftCurly,
    RightCurly,
    LeftLatexCurly,
    RightLatexCurly,
    LeftLatexSquare,
    RightLatexSquare,

    Subscript,

    Comma,
    Dot,
    Ellipsis,
    Colon,
    Prime,
    Add,
    Sub,
    Mult,
    Exp,
    LeftAbs,
    RightAbs,

    Equals,
    LessThan,
    MoreThan,
    LessOrEqual,
    MoreOrEqual,

    Sqrt,
    Frac,
    Sum,
    Prod,
    For,
    With,
    SimArrow,
}

pub struct Lexer {
    data: AdvVec<u8>,
}

impl Lexer {
    pub fn lex(code: String) -> Option<(Vec<Token>, String)> {
        let mut lexer = Self::new(code)?;
        let lexed = lexer.lex_self();
        let data = lexer.data.take_remaining();
        Some((lexed, String::from_utf8(data).unwrap()))
    }

    pub fn new(data: String) -> Option<Self> {
        Some(Self {
            data: AdvVec::from_ascii(data.replace("\\ ", ""))?,
        })
    }

    pub fn lex_self(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        while let Some(token) = self.lex_one() {
            // println!("Lexed {token:?}");
            // println!("State: {:?}", self.data);
            tokens.push(token);
        }
        tokens
    }

    fn lex_one(&mut self) -> Option<Token> {
        if let Some(b' ') = unsafe { self.data.peek() } {
            unsafe {
                self.data.advance();
            }
        }

        one_of! {
            self;
            Token::Punct = lex_punctuation,
            Token::Number = lex_number,
            Token::Identifier = lex_identifier,
            Token::op_convert = lex_operator,
        }
    }

    fn lex_identifier(&mut self) -> Option<String> {
        const ACCEPTED_NAMES: [&[u8]; 22] = [
            b"alpha", b"beta", b"gamma", b"delta", b"epsilon", b"zeta", b"eta", b"theta", b"iota",
            b"kappa", b"lambda", b"mu", b"nu", b"xi", b"rho", b"sigma", b"tau", b"upsilon", b"phi",
            b"chi", b"psi", b"omega",
        ];

        let mut data = self.data.guard_copy_denied();

        let mut ident = Vec::new();
        let next = data.advance()?;
        if next == b'\\' {
            let mut part = data.advance_while_unchecked(is_alphabet);
            if ACCEPTED_NAMES.iter().any(|&v| part == v) {
                ident.append(&mut part);
            } else {
                return None;
            }
        } else if is_alphabet(next) {
            ident.push(next);
        } else {
            return None;
        }

        if data.advance_if_eq(b'_') {
            data.must_equal(b'{');
            let mut left_count = 0;
            let mut name_part = data.advance_while_unchecked(|v| {
                if v == b'{' {
                    left_count += 1;
                }

                if v == b'}' {
                    if left_count == 0 {
                        false
                    } else {
                        left_count -= 1;
                        true
                    }
                } else {
                    true
                }
            });
            data.must_equal(b'}');
            ident.push(b'_');
            ident.append(&mut name_part);
        }

        data.accepted();
        Some(String::from_utf8(ident).unwrap())
    }
    fn lex_operator(&mut self) -> Option<String> {
        let mut data = self.data.guard_copy_denied();

        if data.advance_if_eq(b'\\') {
            if let Some(name) = data.advance_while(is_alphabet) {
                let name = if name == b"operatorname" {
                    data.must_equal(b'{');
                    let name = data.advance_while_unchecked(is_alphabet);
                    data.must_equal(b'}');
                    name
                } else {
                    name
                };

                data.accepted();
                return Some(String::from_utf8(name).unwrap());
            }
        }

        None
    }
    fn lex_number(&mut self) -> Option<f64> {
        let mut data = self.data.guard_copy_denied();

        let negative = if data.advance_if_eq(b'-') { -1. } else { 1. };

        if let Some(mut number) = data.advance_while(is_digit) {
            if data.peek() == Some(b'.') && data.peek_next().is_some_and(is_digit) {
                data.advance();
                let mut fractional = data.advance_while_unchecked(is_digit);
                number.push(b'.');
                number.append(&mut fractional);
            }

            let string = String::from_utf8(number).unwrap();
            let number: f64 = string.parse().ok()?;

            data.accepted();
            Some(number.copysign(negative))
        } else {
            None
        }
    }

    fn lex_punctuation(&mut self) -> Option<Punctuation> {
        let mut data = self.data.guard_copy_denied();

        let next = data.advance()?;

        let punct = match next {
            b'\\' => {
                let is_left = match data.advance()? {
                    b'l' => {
                        data.advance_if_eq(b'e').then_some(())?;
                        data.advance_if_eq(b'f').then_some(())?;
                        data.advance_if_eq(b't').then_some(())?;
                        true
                    }
                    b'r' => {
                        data.advance_if_eq(b'i').then_some(())?;
                        data.advance_if_eq(b'g').then_some(())?;
                        data.advance_if_eq(b'h').then_some(())?;
                        data.advance_if_eq(b't').then_some(())?;
                        false
                    }
                    _ => return None,
                };

                let next = data.advance()?;
                if is_left {
                    match next {
                        b'[' => Some(Punctuation::LeftSquare),
                        b'(' => Some(Punctuation::LeftParen),
                        b'|' => Some(Punctuation::LeftAbs),
                        b'\\' => data.advance_if_eq(b'{').then_some(Punctuation::LeftCurly),
                        _ => None,
                    }
                } else {
                    match next {
                        b']' => Some(Punctuation::RightSquare),
                        b')' => Some(Punctuation::RightParen),
                        b'|' => Some(Punctuation::RightAbs),
                        b'\\' => data.advance_if_eq(b'}').then_some(Punctuation::RightCurly),
                        _ => None,
                    }
                }
            }

            b'{' => Some(Punctuation::LeftLatexCurly),
            b'}' => Some(Punctuation::RightLatexCurly),
            b'[' => Some(Punctuation::LeftLatexSquare),
            b']' => Some(Punctuation::RightLatexSquare),

            b'_' => Some(Punctuation::Subscript),

            b',' => Some(Punctuation::Comma),
            b'.' => {
                if data.peek()? == b'.' && data.peek_next()? == b'.' {
                    data.advance();
                    data.advance();
                    Some(Punctuation::Ellipsis)
                } else {
                    Some(Punctuation::Dot)
                }
            }
            b':' => Some(Punctuation::Colon),
            b'\'' => Some(Punctuation::Prime),
            b'+' => Some(Punctuation::Add),
            b'-' => {
                if data.advance_if_eq(b'>') {
                    Some(Punctuation::SimArrow)
                } else {
                    Some(Punctuation::Sub)
                }
            }
            b'^' => Some(Punctuation::Exp),

            b'=' => Some(Punctuation::Equals),
            b'<' => Some(Punctuation::LessThan),
            b'>' => Some(Punctuation::MoreThan),

            _ => None,
        };

        if punct.is_some() {
            data.accepted();
        }

        punct
    }
}
