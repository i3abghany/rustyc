use crate::symbol_table::Symbol::Variable;
use std::cmp::min;
use std::collections::HashMap;

#[derive(Clone)]
pub enum Symbol {
    // TODO change types from strings to enums.
    Variable {
        variable_type: String,
        stack_offset: isize,
    },
    Function {
        return_type: String,
        parameters: Vec<String>,
    },
}

impl Symbol {
    fn get_type_size_in_bytes(type_str: &str) -> usize {
        4
    }

    pub fn size(&self) -> usize {
        match self {
            Variable { variable_type, .. } => Symbol::get_type_size_in_bytes(variable_type),
            Symbol::Function { .. } => panic!(),
        }
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

    pub fn default(table: &SymbolTable) -> Self {
        Self::new(table.current_scope_stack_top())
    }

    pub fn insert(&mut self, symbol_name: &str, declaration: &Symbol) {
        self.symbols
            .insert(String::from(symbol_name), declaration.clone());
        match declaration {
            Symbol::Variable { .. } => self.stack_top -= declaration.size() as isize,
            Symbol::Function { .. } => {}
        }
    }

    pub fn insert_top(&mut self, symbol_name: &str, variable_type: &str) {
        self.insert(
            symbol_name,
            &Variable {
                variable_type: String::from(variable_type),
                stack_offset: self.stack_top
                    - (Symbol::get_type_size_in_bytes(variable_type) as isize),
            },
        )
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

    pub fn insert(&mut self, symbol_name: &str, declaration: &Symbol) {
        self.scopes
            .last_mut()
            .unwrap()
            .insert(symbol_name, declaration);
    }

    pub fn insert_top(&mut self, symbol_name: &str, variable_type: &str) {
        self.scopes
            .last_mut()
            .unwrap()
            .insert_top(symbol_name, variable_type);
    }

    pub fn current_scope_stack_top(&self) -> isize {
        self.scopes.last().unwrap().stack_top
    }
}
