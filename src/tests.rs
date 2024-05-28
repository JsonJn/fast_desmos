#![allow(dead_code)]

use std::collections::hash_map::DefaultHasher;
use std::fmt::Debug;
use std::fs;
use std::fs::File;
use std::hash::{Hash, Hasher};
use std::io::Write;
use std::path::Path;
use std::time::{Duration, Instant};

use crate::desmos::evaluate::{Evaluable, Functions, Point, ToEval, ValueContext, VarValue};
use crate::desmos::parsing::{Lexer, Parser};

fn parse_value(b: &str) -> VarValue {
    let mut bytes = b.bytes();
    let first = bytes.next().unwrap();
    let text = bytes.collect::<Vec<_>>();
    let text = String::from_utf8(text).unwrap();
    match first {
        b'N' => VarValue::number(text.parse().unwrap()),
        b'P' => {
            let [a, b]: [f64; 2] = text
                .strip_prefix('(')
                .unwrap()
                .strip_suffix(')')
                .unwrap()
                .split(',')
                .map(|v| v.parse().unwrap())
                .collect::<Vec<_>>()
                .try_into()
                .unwrap();
            VarValue::point(Point(a, b))
        }
        b'L' => {
            let mut bytes = text.into_bytes();
            let remaining = String::from_utf8(bytes.split_off(1)).unwrap();
            let [only_left] = bytes.try_into().unwrap();
            match only_left {
                b'N' => {
                    let xs = remaining
                        .strip_prefix('[')
                        .unwrap()
                        .strip_suffix(']')
                        .unwrap()
                        .split(',')
                        .map(|v| v.parse().unwrap())
                        .collect::<Vec<_>>();
                    VarValue::num_list(xs)
                }
                _ => panic!("Invalid list character: {}", char::from(only_left)),
            }
        }
        _ => panic!("Invalid first character of value format"),
    }
}

fn hash_of<T: Hash>(obj: &T) -> u64 {
    let mut hasher = DefaultHasher::new();
    obj.hash(&mut hasher);
    hasher.finish()
}

fn process_chunk(lines: &[&str]) -> (String, String, VarValue) {
    let [a, b]: [&str; 2] = [0, 1].map(|i| lines[i]);
    let value = parse_value(b);
    let a = a.to_string();
    let display = a
        .clone()
        .replace("\\left", "")
        .replace("\\right", "")
        .replace("\\cdot", "*");

    (display, a, value)
}

#[test]
fn test_evaluation() {
    const TESTS_PATH: &str = "eval_tests.txt";
    const HASH_PATH: &str = "eval_tests.hash";
    const TEST_RESULT_PATH: &str = "results.txt";
    const PASSED_STR: &str = "ALL PASSED";
    const ERROR_STR: &str = "ERROR ON TEST ";

    let raw_string = fs::read_to_string(TESTS_PATH).expect("The tests aren't there!");

    let this_hash = hash_of(&raw_string);
    let before_hash = fs::read(HASH_PATH)
        .ok()
        .and_then(|bytes| Some(u64::from_ne_bytes(bytes.try_into().ok()?)));

    let checked_already = before_hash.map(|v| v == this_hash).unwrap_or(false) && {
        // if hash matches, check if write file doesn't exist
        Path::new(TEST_RESULT_PATH).exists()
    };

    fs::write(HASH_PATH, this_hash.to_ne_bytes()).expect("Hash write failed!");

    let lines = raw_string.lines().collect::<Vec<_>>();
    let mut line_chunks = lines.chunks(3).enumerate().collect::<Vec<_>>();

    if checked_already {
        let results_file = fs::read_to_string(TEST_RESULT_PATH).unwrap();
        if !results_file.trim_end().ends_with(PASSED_STR) {
            let mut last_lines = results_file.lines();
            let last_line = last_lines.next_back().unwrap();
            let p = last_line.split(' ').collect::<Vec<_>>()[2];
            let p: usize = p.strip_suffix("...").unwrap().parse().unwrap();

            let at = line_chunks.remove(p);
            line_chunks.insert(0, at);
        }
    }

    let new_thing: Vec<_> = line_chunks
        .into_iter()
        .map(|(a, b)| (a, process_chunk(b)))
        .collect();

    let mut results_file = File::create(TEST_RESULT_PATH).unwrap();

    for (i, (err_display, expr, val)) in new_thing {
        println!("Testing {err_display}...");
        write!(results_file, "Testing Test {i}... ").unwrap();
        results_file.flush().unwrap();
        let time = test_eval_pair(expr, &val);
        println!("Success: {time:?}");
        writeln!(results_file, "Success!").unwrap();
    }
    writeln!(results_file, "{PASSED_STR}").unwrap();
}

#[derive(Debug)]
struct TestResult {
    lexing: Duration,
    parsing: Duration,
    to_eval: Duration,
    evaluate: Duration,
}

fn test_eval_pair(expr: String, val: &VarValue) -> TestResult {
    let start = Instant::now();
    let Some((lexed, remaining)) = Lexer::lex(expr) else {
        panic!("Lexing failed");
    };

    let lexing = start.elapsed();

    if !remaining.is_empty() {
        panic!("Lexing incomplete with remaining {remaining}");
    }

    let start = Instant::now();
    let Some(parsed) = Parser::parse_expr(lexed) else {
        panic!("Parsing failed");
    };
    let parsing = start.elapsed();

    let mut context = ValueContext::default();
    let functions = Functions::default();

    let start = Instant::now();
    let to_evaled = parsed.to_eval();
    let to_eval = start.elapsed();

    let start = Instant::now();
    let value = to_evaled.evaluate(&functions, &mut context);
    let evaluate = start.elapsed();

    if value.ne(val) {
        panic!("Wrong eval result: \n{value:?}, should have been\n{val:?}");
    }

    TestResult {
        lexing,
        parsing,
        to_eval,
        evaluate,
    }
}
