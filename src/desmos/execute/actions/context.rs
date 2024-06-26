use super::{ActExpr, ActIdent, ActValue};
use crate::desmos::evaluate::UserIdent;
use crate::vec_map::VecMap;

#[derive(Default)]
pub struct ActFuncBuilder {
    values: VecMap<ActIdent, ActFunction>,
}

impl ActFuncBuilder {
    pub fn add_function(&mut self, ident: ActIdent, function: ActFunction) {
        self.values.insert(&ident, function);
    }

    pub fn build(self) -> ActFunctions {
        ActFunctions {
            values: self.values,
        }
    }
}

pub struct ActFunctions {
    values: VecMap<ActIdent, ActFunction>,
}

impl ActFunctions {
    pub fn get_function(&self, ident: ActIdent) -> &ActFunction {
        &self.values[&ident]
    }
}

#[derive(Debug)]
pub struct ActFunction {
    pub params: Vec<UserIdent>,
    pub expr: ActExpr,
}

#[derive(Debug, Default)]
pub struct ActContext {
    values: VecMap<ActIdent, ActValue>,
}

#[allow(dead_code)]
impl ActContext {
    pub fn get_value(&self, ident: ActIdent) -> ActValue {
        match self.values.get(&ident) {
            Some(x) => x.clone(),
            None => unreachable!("Uninitialized variable: {ident:?}"),
        }
    }

    pub fn set_value(&mut self, ident: ActIdent, value: ActValue) {
        self.values.insert(&ident, value);
    }

    pub fn is_initialized(&self, ident: ActIdent) -> bool {
        self.values.contains_key(&ident)
    }
}
