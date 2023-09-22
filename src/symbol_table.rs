use std::cmp::min;
use std::collections::HashMap;

#[derive(Clone)]
pub enum Symbol {
    Variable {
        variable_type: String,
        stack_offset: isize,
    },
}

impl Symbol {
    pub fn size(&self) -> usize {
        4
    }
}

pub struct Scope {
    symbols: HashMap<String, Symbol>,
    stack_top: isize,
}

impl Scope {
    pub fn new(start_offset: isize) -> Self {
        Self {
            symbols: HashMap::default(),
            stack_top: start_offset,
        }
    }
}

pub struct SymbolTable {
    scopes: Vec<Scope>,
    _largest_offset: isize,
}

impl SymbolTable {
    pub fn new() -> Self {
        // The whole stack frame will reserve at least 8 bytes for
        // the return address.
        Self {
            scopes: Vec::new(),
            _largest_offset: -8,
        }
    }

    fn get_scope(&self, i: usize) -> &HashMap<String, Symbol> {
        &self.scopes[i].symbols
    }

    pub fn push_scope(&mut self, scope: Scope) {
        self.scopes.push(scope);
    }

    pub fn pop_scope(&mut self) -> Option<Scope> {
        self._largest_offset = min(self._largest_offset, self.scopes.last().unwrap().stack_top);
        self.scopes.pop()
    }

    pub fn reset_largest_offset(&mut self) {
        self._largest_offset = -8;
    }

    pub fn current_largest_offset(&self) -> isize {
        self._largest_offset
    }
    fn get_at_scope(&self, i: usize, symbol: &str) -> Option<&Symbol> {
        assert!(i < self.scopes.len());
        self.scopes[i].symbols.get(symbol)
    }

    pub fn get(&self, symbol: &str) -> Option<&Symbol> {
        let mut current = self.scopes.len() as isize - 1;
        while current >= 0 {
            if let Some(variable) = self.get_at_scope(current as usize, symbol) {
                return Some(variable);
            } else {
                current -= 1;
            }
        }
        None
    }

    pub fn get_at_current_scope(&self, symbol_name: &str) -> Option<&Symbol> {
        self.get_at_scope(self.scopes.len() - 1, symbol_name)
    }

    pub fn insert(&mut self, symbol_name: &String, definition: Symbol) {
        let last = self.scopes.len() - 1;
        self.scopes[last]
            .symbols
            .insert(symbol_name.clone(), definition.clone());
        match definition {
            Symbol::Variable { .. } => self.scopes[last].stack_top -= definition.size() as isize,
        }
    }

    pub fn current_scope_stack_top(&self) -> isize {
        self.scopes.last().unwrap().stack_top
    }
}
