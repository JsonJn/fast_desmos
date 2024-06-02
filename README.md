![logo](logo.png)

## Purpose

The [desmos graphing calculator](https://www.desmos.com/), besides being a good graphing calculator, can also be used
for simulation thanks to its feature of *actions*.

`fast_desmos` is a project to replicate desmos for faster simulation, and make it potentially feasible for game dev.

## Features

- Input a desmos URL and it'll run it
- 150x speedup from execution on Edge, 20x speedup from Chrome

### Supported desmos features

Not all desmos features are supported. In fact, **most desmos features that makes a "graphing calculator" aren't
supported**, such as the ability to draw arbitrary curves, find curve intersections, etc.

What is supported however are features useful for simulation, such as tickers, actions, drawing points and lines and
simple parametrics, etc.

Below is a full list of supported and to-be-supported features:

#### Legend:

- [x] denotes already supported feature.
- [ ] denotes planned support.

#### List:

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
    - [ ] draggable
        - [x] draggable on both or no axis
        - [ ] draggable on only x-axs or y-axis
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

## Running the program

Have Rust installed, clone the repository, run `cargo run --release` in the project directory.

## Usage

Better ergonomics is planned for the future, but for now, go into `main.rs`, and change the URL used.

```Rust
fn main() {
    //                               This part is the URL
    //                                vvvvvvvvvvvvvvvvv
    let desmos = DesmosPage::from_url(SPECIAL_CONNECT_4).unwrap();
    //                                ^^^^^^^^^^^^^^^^^
}
```
