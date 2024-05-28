raw = """
Sin,
    Cos,
    Tan,
    Sec,
    Csc,
    Cot,

    Cosh,
    Sinh,
    Tanh,
    Sech,
    Csch,
    Coth,

    ArcSin,
    ArcCos,
    ArcTan,
    ArcSec,
    ArcCsc,
    ArcCot,

    ArcCosh,
    ArcSinh,
    ArcTanh,
    ArcSech,
    ArcCsch,
    ArcCoth,

    Sign,
    Mod,
    Floor,
    Ceil,
    Round,

    Choose,
    Permutation,

    Length,
    Join,
    Unique,
    Sort,

    Polygon,
    Rgb,
    Hsv,
    
    Infinity,
    Pi,
"""

lines = [s[:-1] for x in raw.split("\n") if len(s := x.strip()) > 0]
print(lines)
[print(f"\"_{x.lower()}\" => Self::{x},") for x in lines]
