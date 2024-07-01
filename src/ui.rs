use std::sync::mpsc;
use std::sync::mpsc::{Receiver, SendError};
use std::thread::JoinHandle;
use std::{process, thread};

use crate::desmos::DesmosPage;
use raylib::prelude::*;
use url::Url;

pub fn input_url(sc: i32) -> DesmosPage {
    const F_S: i32 = 20;
    const W: i32 = 600;
    const H: i32 = 100;

    let scale = |x| x * sc;

    let (mut rl, rt) = init().size(W * sc, H * sc).build();
    rl.set_target_fps(60);

    let mut url = String::new();

    let mut failed_timer = 0usize;
    let mut currently_parsing = false;
    let mut parse_thread: Option<JoinHandle<Result<(), SendError<eyre::Result<DesmosPage>>>>> =
        None;
    let mut parse_pipe: Option<Receiver<eyre::Result<DesmosPage>>> = None;
    let mut parse_log_recv: Option<Receiver<String>> = None;
    let mut parse_log: Vec<String> = Vec::new();
    let mut parse_keep_open = 0usize;

    let mut backspace_timer = 0usize;

    while !rl.window_should_close() {
        if let Some(c) = rl.get_char_pressed() {
            url.push(c);
        }
        if rl.is_key_pressed(KeyboardKey::KEY_BACKSPACE) {
            url.pop();
        }
        if rl.is_key_down(KeyboardKey::KEY_BACKSPACE) {
            backspace_timer += 1;
            if backspace_timer % 2 == 0 && backspace_timer > 20 {
                url.pop();
            }
        } else {
            backspace_timer = 0;
        }

        let mut submit = false;
        let mut paste = false;

        if rl.is_key_pressed(KeyboardKey::KEY_V) && rl.is_key_down(KeyboardKey::KEY_LEFT_CONTROL) {
            paste = true;
        }
        if rl.is_key_pressed(KeyboardKey::KEY_ENTER) {
            submit = true;
        }

        if let Some(result) = parse_pipe.as_ref().and_then(|recv| recv.try_recv().ok()) {
            match result {
                Ok(page) => return page,
                Err(rep) => {
                    parse_log.push(rep.to_string());
                    eprintln!("Something has gone wrong: {rep}");
                    parse_keep_open = 120;
                }
            }

            currently_parsing = false;
            let _ = parse_thread.unwrap().join();
            parse_thread = None;
            parse_log_recv = None;
            parse_pipe = None;
        }

        if let Some(recv) = &parse_log_recv {
            while let Ok(x) = recv.try_recv() {
                parse_log.push(x);
            }
        }

        {
            let mut draw = rl.begin_drawing(&rt);
            draw.clear_background(Color::WHITE);

            fn draw_rect(draw: &mut RaylibDrawHandle, [x, y, w, h]: [i32; 4], f: Color, s: Color) {
                draw.draw_rectangle(x, y, w, h, f);
                draw.draw_rectangle_lines(x, y, w, h, s);
            }

            if currently_parsing || parse_keep_open > 0 {
                const UFS: i32 = 10;

                parse_keep_open = parse_keep_open.saturating_sub(1);

                const M: i32 = 5;
                let height = scale(H - 2 * M);
                let rect = [M, M, W - 2 * M, H - 2 * M].map(scale);
                draw_rect(&mut draw, rect, Color::BLACK, Color::BLACK);

                const TM: i32 = 5;
                const LH: i32 = TM + UFS;
                let current_height = (parse_log.len() as i32) * scale(LH);
                let offset = height - current_height;
                let len = parse_log.len();
                for (y, s) in parse_log.iter().enumerate() {
                    draw.draw_text(
                        &s,
                        scale(M + TM),
                        offset + scale(LH) * y as i32,
                        scale(UFS),
                        if len - 1 == y && parse_keep_open > 0 {
                            Color::RED
                        } else {
                            Color::WHITE
                        },
                    );
                }
            } else {
                let mouse = draw.get_mouse_position();
                let mouse = (mouse.x as i32, mouse.y as i32);

                fn collide([x, y, w, h]: [i32; 4], (mx, my): (i32, i32)) -> bool {
                    let ox = mx - x;
                    let oy = my - y;
                    0 <= ox && ox < w && 0 < oy && oy < h
                }

                const IX: i32 = 5;
                const IY: i32 = 5;
                const IH: i32 = 30;
                let text_box = [IX, IY, W - IX * 2, IH].map(scale);
                draw_rect(&mut draw, text_box, Color::LIGHTGRAY, Color::GRAY);
                const TM: i32 = 5;
                let [tx, ty] = [IX + TM, IY + (IH - F_S) / 2].map(scale);
                if url.is_empty() {
                    const DT: &'static str = "https://www.desmos.com/calculator/xxxxxxxxxx";
                    draw.draw_text(DT, tx, ty, F_S * sc, Color::GRAY);
                } else {
                    const DT: &'static str = "https://www.desmos.com/calculator/";
                    draw.draw_text(DT, tx, ty, F_S * sc, Color::GRAY);
                    draw.draw_text(&url, tx, ty, F_S * sc, Color::BLACK);
                }

                const SX: i32 = 100;
                const SY: i32 = 40;
                const SH: i32 = 30;
                const SG: i32 = 10;
                let left_button = [SX - SG, SY, W / 2 - SX, SH].map(scale);
                let right_button = [W / 2 + SG, SY, W / 2 - SX, SH].map(scale);
                // const M: i32 = 5;
                // let down_button = [SX - SG, SY + SH + M, 2 * SG + W - 2 * SX, SH].map(scale);
                draw_rect(&mut draw, left_button, Color::LIGHTGRAY, Color::GRAY);
                draw_rect(&mut draw, right_button, Color::LIGHTGRAY, Color::GRAY);
                // draw_rect(&mut draw, down_button, Color::LIGHTGRAY, Color::GRAY);

                if collide(left_button, mouse) {
                    submit = true;
                }
                if collide(right_button, mouse) {
                    paste = true;
                }

                let (text, color) = if failed_timer == 0 {
                    ("Load", Color::BLACK)
                } else {
                    failed_timer -= 1;
                    ("Url Parse Failed", Color::RED)
                };
                let f_w = draw.measure_text(text, F_S);
                draw.draw_text(
                    text,
                    (SX - SG + (W / 2 - SX - f_w) / 2) * sc,
                    (SY + (SH - F_S) / 2) * sc,
                    F_S * sc,
                    color,
                );

                let text = "Paste Clipboard";
                let f_w = draw.measure_text(text, F_S);
                draw.draw_text(
                    text,
                    (W / 2 + SG + (W / 2 - SX - f_w) / 2) * sc,
                    (SY + (SH - F_S) / 2) * sc,
                    F_S * sc,
                    Color::BLACK,
                );

                // let text = "Use last cached";
                // let f_w = draw.measure_text(text, F_S);
                // draw.draw_text(
                //     text,
                //     ((W - f_w) / 2) * sc,
                //     (SY + SH + M + (SH - F_S) / 2) * sc,
                //     F_S * sc,
                //     Color::BLACK,
                // );
            }
        }

        if rl.is_mouse_button_pressed(MouseButton::MOUSE_BUTTON_LEFT) {
            if submit {
                let url = Url::parse(&url);
                if let Ok(s) = url {
                    currently_parsing = true;
                    let (page_send, page_recv) = mpsc::channel();
                    let (log_send, log_recv) = mpsc::channel();
                    let thread = thread::spawn(move || {
                        let page = DesmosPage::from_url(s, Some(log_send));
                        page_send.send(page)
                    });
                    parse_thread = Some(thread);
                    parse_pipe = Some(page_recv);
                    parse_log_recv = Some(log_recv);
                    parse_log.clear();
                } else {
                    failed_timer = 20;
                }
            }
            if paste {
                url = rl.get_clipboard_text().unwrap_or_else(|_| String::new());
            }
        }
    }
    process::exit(0);
}
