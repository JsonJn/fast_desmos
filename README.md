![logo](logo.png)

## Purpose

The [desmos graphing calculator](https://www.desmos.com/), besides being a good graphing calculator, can also be used
for simulation thanks to its feature of *actions*.

`fast_desmos` is a project to replicate desmos for faster simulation.

## Features

- Input a desmos URL and it'll run it
- 150x speedup from execution on Edge, 20x speedup from Chrome

### Supported desmos features

Filled tick box denotes already supported feature.

Empty tick box denotes planned support.

- [ ] Parametric equations
    - [x] Straight line parametric functions with arbitrary bounds
    - [ ] Curved parametrics
- [ ] Explicit Equations
    - [x] Straight line explicit equations with explicit bounds, like like `y = 1 {0 < x < 2}`
    - [ ] Straight line explicit equations without bounds
    - [ ] Curved explicit equations
- [ ] Polygons
    - [x] Polygons with custom fill, lines, **but not line type**
    - [ ] line type
- [ ] Points
    - [x] Points with custom sizes, opacity, **but not point type**
    - [x] draggable
    - [ ] point type
- [x] Custom colors (`rgb`, `hsv`)
- [x] All Builtin functions **except for distributions**
- [x] Most desmos syntax (if-else, sum, prod, fractions, etc) **except for calculus**
- [x] Actions
    - [x] Tickers without the ability to turn them off
    - [x] Clickable
        - [x] Points
        - [x] Polygons
- [ ] Custom control options
    - [ ] keyboard controlled variables / actions
    - [ ] panning points

## Installation

Have Rust installed, clone the repository, run `cargo run --release` in the project directory.

## Usage

Better ergonomics is planned for the future, but for now, go into `main.rs`, and change the URL used (should be
obvious).
