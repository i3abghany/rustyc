use inkwell::values::PointerValue;
use std::collections::HashMap;

#[derive(Clone)]
pub struct Variable<'ctx> {
    pub pointer: PointerValue<'ctx>,
    pub variable_type: inkwell::types::BasicTypeEnum<'ctx>,
    pub is_initialized: bool,
}

pub struct SymbolTable<'ctx> {
    pub scopes: Vec<HashMap<String, Variable<'ctx>>>,
    pub globals: HashMap<String, Variable<'ctx>>,
}

impl<'ctx> SymbolTable<'ctx> {
    pub fn new() -> Self {
        Self {
            scopes: vec![],
            globals: HashMap::new(),
        }
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn insert_global(&mut self, name: &str, variable: &Variable<'ctx>) {
        self.globals.insert(String::from(name), variable.clone());
    }

    pub fn insert(&mut self, name: &str, variable: &Variable<'ctx>) {
        self.scopes
            .last_mut()
            .unwrap()
            .insert(String::from(name), variable.clone());
    }

    pub fn find(&self, name: &str) -> Option<&Variable<'ctx>> {
        for scope in self.scopes.iter().rev() {
            if let Some(value) = scope.get(name) {
                return Some(value);
            }
        }
        None
    }

    pub fn find_hierarchically(&self, name: &str) -> Option<&Variable<'ctx>> {
        let mut result = self.find(&name);
        if result.is_none() {
            result = self.find_in_global_scope(&name);
        }
        result
    }

    pub fn find_in_current_scope(&self, name: &str) -> Option<&Variable<'ctx>> {
        if let Some(scope) = self.scopes.last() {
            return scope.get(name);
        } else {
            None
        }
    }

    pub fn find_in_global_scope(&self, name: &str) -> Option<&Variable<'ctx>> {
        self.globals.get(name)
    }
}
