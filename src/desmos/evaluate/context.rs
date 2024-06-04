use crate::desmos::evaluate::{Function, IdentifierStorer, UserIdent};
use crate::desmos::value::VarValue;
use crate::vec_map::VecMap;

#[derive(Default)]
pub struct FunctionsBuilder {
    values: VecMap<UserIdent, Function>,
}

impl FunctionsBuilder {
    pub fn add_function(&mut self, ident: UserIdent, function: Function) {
        self.values.insert(&ident, function);
    }

    pub fn build(self) -> Functions {
        Functions {
            values: self.values,
        }
    }
}

#[derive(Default, Debug)]
pub struct Functions {
    values: VecMap<UserIdent, Function>,
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
    values: VecMap<UserIdent, VarValue>,
}

impl Default for ValueContext {
    fn default() -> Self {
        let mut values = VecMap::default();
        values.insert(
            &IdentifierStorer::IDENT_PI,
            VarValue::number(std::f64::consts::PI),
        );
        values.insert(
            &IdentifierStorer::IDENT_INFINITY,
            VarValue::number(f64::INFINITY),
        );

        Self { values }
    }
}

impl ValueContext {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn len(&self) -> usize {
        self.values.len()
    }

    pub fn iter(&self) -> impl Iterator<Item = (UserIdent, &VarValue)> {
        self.values
            .iter()
            .filter(|x| !IdentifierStorer::RESERVED_IDENTS.contains(&UserIdent(x.0)))
            .map(|(a, b)| (UserIdent(a), b))
    }

    pub fn unset(&mut self, ident: UserIdent) {
        self.values.remove(&ident);
    }

    pub fn un_or_set(&mut self, ident: UserIdent, value: Option<VarValue>) {
        match value {
            Some(x) => {
                let _ = self.set_value(ident, x);
            }
            None => self.unset(ident),
        }
    }

    #[must_use]
    pub fn set_value(&mut self, ident: UserIdent, value: VarValue) -> Option<VarValue> {
        let old = self.values.remove(&ident);
        self.values.insert(&ident, value);
        old
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
