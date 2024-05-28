use std::sync::Mutex;

use super::super::tree::ActIdent;

pub struct ActIdentifierStorer {
    idents: Mutex<Vec<String>>,
}

impl ActIdentifierStorer {
    pub fn new() -> Self {
        Self {
            idents: Mutex::new(Vec::new()),
        }
    }

    pub fn name_to_int(&self, name: &str) -> ActIdent {
        let mut idents = self.idents.lock().unwrap();
        let pos = idents.iter().position(|v| v == name);

        ActIdent(pos.unwrap_or_else(|| {
            idents.push(name.to_string());
            idents.len() - 1
        }))
    }

    pub fn int_to_name(&self, ActIdent(i): ActIdent) -> Option<String> {
        let guard = self.idents.lock().unwrap();
        guard.get(i).cloned()
    }
}
