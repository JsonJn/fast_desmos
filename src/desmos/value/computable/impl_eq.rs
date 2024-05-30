use std::cmp::Ordering;

use crate::desmos::value::CompPrim;

impl CompPrim {
    pub fn cmp(self, other: Self) -> Option<Ordering> {
        match (self, other) {
            (Self::Number(a), Self::Number(b)) => a.partial_cmp(&b),
            _ => unreachable!("Can only compare number with number."),
        }
    }
}
