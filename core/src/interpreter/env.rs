use std::collections::HashMap;

use crate::{
    type_checker::types::Type,
    util::{failable::Failable, str::Str},
};

use super::{
    error::RuntimeError,
    value::{Function, Value},
};

/// The environment is a map of variable names to values.
/// The environment is used to store variables and functions.
#[derive(Debug, Clone)]
pub struct Environment<'a> {
    pub name: Str,
    variables: HashMap<String, Value>,
    functions: HashMap<String, Function>, // Sort function values in a way that allows for fast lookup
    types: HashMap<String, Type>,
    parent: Option<&'a Environment<'a>>,
}

impl<'a> Environment<'a> {
    pub fn new(name: Str) -> Self {
        Self {
            name,
            variables: HashMap::new(),
            functions: HashMap::new(),
            types: HashMap::new(),
            parent: None,
        }
    }

    pub fn new_child<'b: 'a>(&'a self, name: Str) -> Self {
        Self {
            name,
            variables: HashMap::new(),
            functions: HashMap::new(),
            types: HashMap::new(),
            parent: Some(self),
        }
    }

    /// Flatten all parent environments into a single static environment.
    pub fn deep_clone(&self) -> Environment<'static> {
        let mut env = if let Some(parent) = self.parent {
            parent.deep_clone()
        } else {
            Environment::new(self.name.clone())
        };
        for (name, value) in &self.variables {
            env.variables.insert(name.clone(), value.clone());
        }
        for (name, function) in &self.functions {
            env.functions.insert(name.clone(), function.clone());
        }
        for (name, type_) in &self.types {
            env.types.insert(name.clone(), type_.clone());
        }
        env
    }

    pub fn lookup_variable(&self, name: &str) -> Option<&Value> {
        log::trace!(
            "Looking up variable '{}' in environment '{}' of {:?}",
            name,
            self.name,
            self.variables.keys().collect::<Vec<&String>>()
        );
        self.variables
            .get(name)
            .or_else(|| self.parent.and_then(|p| p.lookup_variable(name)))
    }

    pub fn lookup_function(&self, name: &str) -> Option<&Function> {
        self.functions
            .get(name)
            .or_else(|| self.parent.and_then(|p| p.lookup_function(name)))
    }

    pub fn add_function_variation(
        &mut self,
        name: &str,
        variation: Function,
    ) -> Result<(), RuntimeError> {
        if let Some(_existing) = self.functions.get_mut(name) {
            // TODO: Allow for multiple variations of the same function
            // TODO: with the same name but different parameter types
            return Err(RuntimeError {
                message: format!(
                    "Function {} already exists in the current environment",
                    name
                ),
            });
        } else {
            self.functions.insert(name.to_string(), variation);
        }
        Ok(())
    }

    /// Get a value from the environment.
    /// If the value is not found in the current environment, the parent environment is searched recursively.
    pub fn lookup_identifier(&self, name: &str) -> (Option<&Value>, Option<&Function>) {
        log::trace!(
            "Looking up identifier '{}' in environment '{}'",
            name,
            self.name
        );
        (self.lookup_variable(name), self.lookup_function(name))
    }

    pub fn get_type(&self, name: &str) -> Option<Type> {
        if let Some(t) = self.types.get(name) {
            Some(t.clone())
        } else if let Some(p) = &self.parent {
            p.get_type(name)
        } else {
            None
        }
    }

    /// Add a type to the environment.
    /// If the type already exists in a parent environment, it is shadowed.
    pub fn add_type(&mut self, name: Str, type_: Type) -> Failable<RuntimeError> {
        if self.types.contains_key(&name.to_string()) {
            panic!("Type {} already exists in the current environment", name);
        }
        self.types.insert(name.to_string(), type_);
        Ok(())
    }

    /// Add a variable (any value or function) to the environment.
    /// If the variable already exists in a parent environment, it is shadowed.
    pub fn add_value(&mut self, name: Str, value: Value) -> Failable<RuntimeError> {
        let name = name.to_string();
        // Check if the variable already exists in the standard library
        if self.variables.contains_key(&name) {
            Err(RuntimeError {
                message: format!(
                    "Variable named '{}' already exists in the current environment",
                    name
                ),
            })
        } else {
            match value {
                Value::Function(func) => {
                    if self.functions.contains_key(&name) {
                        return Err(RuntimeError {
                            message: format!(
                                "Function {} already exists in the current environment",
                                name
                            ),
                        });
                    }
                    self.functions.insert(name, *func);
                }
                _ => {
                    if self.variables.contains_key(&name) {
                        return Err(RuntimeError {
                            message: format!(
                                "Variable {} already exists in the current environment",
                                name
                            ),
                        });
                    }
                    self.variables.insert(name, value);
                }
            };
            Ok(())
        }
    }
}

pub fn global_env() -> Environment<'static> {
    Environment::new(Str::from("global"))
}
