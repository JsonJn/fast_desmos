use crate::desmos::evaluate::{UserIdent, VarValue};
use crate::desmos::execute::actions::ActIdent;
use std::iter::Sum;
use std::ops::Add;

#[derive(Debug, Clone, Default)]
pub struct ActValue {
    pub pairs: Vec<(UserIdent, VarValue)>,
}

impl ActValue {
    pub fn new(pairs: Vec<(UserIdent, VarValue)>) -> Self {
        Self { pairs }
    }

    pub fn with_capacity(cap: usize) -> Self {
        Self::new(Vec::with_capacity(cap))
    }

    pub fn push(&mut self, pair: (UserIdent, VarValue)) {
        self.pairs.push(pair)
    }
}

impl Add for ActValue {
    type Output = Self;

    fn add(mut self, rhs: Self) -> Self::Output {
        let ActValue { mut pairs } = rhs;
        self.pairs.append(&mut pairs);
        self
    }
}

impl Sum for ActValue {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        iter.fold(Self::default(), |a, b| a + b)
    }
}
