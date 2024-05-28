use std::f64::consts as f64c;

// Stolen from `statrs`

pub const TWO_SQRT_E_OVER_PI: f64 = 1.8603827342052657173362492472666631120594218414085755;
const GAMMA_DK: [f64; 11] = [
    2.48574089138753565546e-5,
    1.05142378581721974210,
    -3.45687097222016235469,
    4.51227709466894823700,
    -2.98285225323576655721,
    1.05639711577126713077,
    -1.95428773191645869583e-1,
    1.70970543404441224307e-2,
    -5.71926117404305781283e-4,
    4.63399473359905636708e-6,
    -2.71994908488607703910e-9,
];
const GAMMA_R: f64 = 10.900511;

pub fn gamma(x: f64) -> f64 {
    if x < 0.5 {
        let s = GAMMA_DK
            .iter()
            .enumerate()
            .skip(1)
            .fold(GAMMA_DK[0], |s, t| s + t.1 / (t.0 as f64 - x));

        f64c::PI
            / ((f64c::PI * x).sin()
                * s
                * TWO_SQRT_E_OVER_PI
                * ((0.5 - x + GAMMA_R) / f64c::E).powf(0.5 - x))
    } else {
        let s = GAMMA_DK
            .iter()
            .enumerate()
            .skip(1)
            .fold(GAMMA_DK[0], |s, t| s + t.1 / (x + t.0 as f64 - 1.0));

        s * TWO_SQRT_E_OVER_PI * ((x - 0.5 + GAMMA_R) / f64c::E).powf(x - 0.5)
    }
}
