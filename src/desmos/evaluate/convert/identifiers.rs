use std::sync::{Arc, RwLock};

use crate::desmos::evaluate::tree::Builtins;
use crate::desmos::evaluate::{tree, UserIdent};
use crate::desmos::parsing;

pub struct IdentifierStorer {
    pub idents: RwLock<Vec<Arc<String>>>,
}

impl IdentifierStorer {
    pub const RESERVED_NAMES: [&'static str; 7] = ["x", "y", "t", "index", "dt", "pi", "infty"];
    pub const RESERVED_IDENTS: [UserIdent; 7] = [
        UserIdent(0),
        UserIdent(1),
        UserIdent(2),
        UserIdent(3),
        UserIdent(4),
        UserIdent(5),
        UserIdent(6),
    ];
    pub const IDENT_X: UserIdent = UserIdent(0);
    pub const IDENT_Y: UserIdent = UserIdent(1);
    pub const IDENT_T: UserIdent = UserIdent(2);
    pub const IDENT_INDEX: UserIdent = UserIdent(3);
    pub const IDENT_DT: UserIdent = UserIdent(4);
    pub const IDENT_PI: UserIdent = UserIdent(5);
    pub const IDENT_INFINITY: UserIdent = UserIdent(6);

    pub fn new() -> Self {
        Self {
            idents: RwLock::new(
                Self::RESERVED_NAMES
                    .map(ToString::to_string)
                    .map(Arc::new)
                    .to_vec(),
            ),
        }
    }

    pub fn name_to_int(&self, name: &str) -> UserIdent {
        let mut idents = self.idents.write().unwrap();
        let pos = idents.iter().position(|v| v.as_str() == name);

        UserIdent(pos.unwrap_or_else(|| {
            idents.push(Arc::new(name.to_string()));
            idents.len() - 1
        }))
    }

    pub fn int_to_name(&self, i: UserIdent) -> Option<Arc<String>> {
        let guard = self.idents.read().unwrap();
        guard.get(i.0).cloned()
    }

    pub fn convert_ident(&self, ident: parsing::Ident) -> tree::Ident {
        let parsing::Ident(s) = ident;
        if s.starts_with('_') {
            tree::Ident::Builtin(Builtins::from_ident(&s))
        } else {
            tree::Ident::User(self.name_to_int(&s))
        }
    }
}
