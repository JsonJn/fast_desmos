use rustc_hash::FxHashMap;

use crate::desmos::evaluate::value::VarValue;
use crate::desmos::evaluate::{Function, IdentifierStorer, UserIdent};

#[derive(Default)]
pub struct FunctionsBuilder {
    values: FxHashMap<UserIdent, Function>,
}

impl FunctionsBuilder {
    pub fn add_function(&mut self, ident: UserIdent, function: Function) {
        self.values.insert(ident, function);
    }

    pub fn build(self) -> Functions {
        Functions {
            values: self.values,
        }
    }
}

#[derive(Default, Debug)]
pub struct Functions {
    values: FxHashMap<UserIdent, Function>,
}

impl Functions {
    pub fn get_function(&self, ident: UserIdent) -> &Function {
        match self.values.get(&ident) {
            Some(x) => x,
            _ => unreachable!("Uninitialized function: {ident:?}"),
        }
    }
}

#[derive(Debug)]
pub struct ValueContext {
    values: FxHashMap<UserIdent, VarValue>,
}

impl Default for ValueContext {
    fn default() -> Self {
        let mut values = FxHashMap::default();
        values.insert(
            IdentifierStorer::IDENT_PI,
            VarValue::number(std::f64::consts::PI),
        );
        values.insert(
            IdentifierStorer::IDENT_INFINITY,
            VarValue::number(f64::INFINITY),
        );

        Self { values }
    }
}

impl ValueContext {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn unset(&mut self, ident: UserIdent) {
        self.values.remove(&ident);
    }

    pub fn set_value(&mut self, ident: UserIdent, value: VarValue) {
        self.values.insert(ident, value);
    }

    pub fn get_value(&self, ident: UserIdent) -> VarValue {
        if let Some(value) = self.values.get(&ident) {
            return value.clone();
        }
        unreachable!("Uninitialized variable: {ident:?}");
    }

    pub fn try_get_value(&self, ident: UserIdent) -> Option<VarValue> {
        self.values.get(&ident).cloned()
    }

    pub fn is_initialized(&self, ident: UserIdent) -> bool {
        self.values.contains_key(&ident)
    }
}
