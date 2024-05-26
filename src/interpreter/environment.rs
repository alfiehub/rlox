use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::lox_type::LoxType;

#[derive(Debug)]
pub struct Environment {
    pub parent: Option<Rc<RefCell<Environment>>>,
    values: HashMap<String, Option<LoxType>>,
}

pub struct EnvironmentError(pub String);
impl std::fmt::Display for EnvironmentError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

macro_rules! environment_bail {
    ($msg:expr) => {
        return Err(EnvironmentError($msg.to_string()))
    };
}

impl Environment {
    pub fn new() -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self {
            parent: None,
            values: HashMap::new(),
        }))
    }

    pub fn get(&self, key: &str) -> Result<Option<LoxType>, EnvironmentError> {
        match self.values.get(key) {
            Some(value) => match value {
                Some(value) => Ok(Some(value.clone())),
                None => environment_bail!(format!("Uninitialized variable '{}'.", key)),
            },
            None => match &self.parent {
                Some(parent) => parent.borrow().get(key),
                None => environment_bail!(format!("Undefined variable '{}'.", key)),
            },
        }
    }

    fn ancestor(&self, distance: usize) -> Result<Rc<RefCell<Self>>, EnvironmentError> {
        if distance > 1 {
            if let Some(parent) = &self.parent {
                parent.borrow().ancestor(distance - 1)
            } else {
                environment_bail!("No parent.")
            }
        } else {
            self.parent
                .clone()
                .ok_or_else(|| EnvironmentError("No parent.".to_string()))
        }
    }

    pub fn get_at_distance(
        &self,
        key: &str,
        distance: Option<usize>,
    ) -> Result<Option<LoxType>, EnvironmentError> {
        if let Some(distance) = distance {
            if distance == 0 {
                self.get(key)
            } else {
                self.ancestor(distance - 1)?.borrow().get(key)
            }
        } else {
            // We need to get global
            if let Some(global) = self.global() {
                global.borrow().get(key)
            } else {
                self.get(key)
            }
        }
    }

    pub fn create(&mut self, key: String, value: Option<LoxType>) {
        self.values.insert(key, value);
    }

    fn assign(&mut self, key: String, value: Option<LoxType>) -> Result<(), EnvironmentError> {
        if let std::collections::hash_map::Entry::Occupied(mut e) = self.values.entry(key.clone()) {
            e.insert(value);
            Ok(())
        } else {
            match &self.parent {
                Some(parent) => parent.borrow_mut().assign(key, value),

                None => environment_bail!("Undefined variable '{key}'."),
            }
        }
    }

    pub fn assign_at_distance(
        &mut self,
        key: String,
        value: Option<LoxType>,
        distance: Option<usize>,
    ) -> Result<(), EnvironmentError> {
        if let Some(distance) = distance {
            if distance == 0 {
                self.assign(key, value)
            } else {
                self.ancestor(distance - 1)?.borrow_mut().assign(key, value)
            }
        } else {
            // We need to go global
            if let Some(global) = self.global() {
                global.borrow_mut().assign(key, value)
            } else {
                self.assign(key, value)
            }
        }
    }

    fn global(&self) -> Option<Rc<RefCell<Self>>> {
        if let Some(parent) = &self.parent {
            // We have a parent, but do we have a grandparent?
            if parent.borrow().parent.is_some() {
                // Yes, we need to check if the grandparent is the topmost
                parent.borrow().global()
            } else {
                Some(parent.clone())
            }
        } else {
            None
        }
    }
}

pub trait Nesting {
    fn nest(&self) -> Self;
    fn unnest(&self) -> Self;
}

impl Nesting for Rc<RefCell<Environment>> {
    fn nest(&self) -> Self {
        let child_environment = Environment::new();
        child_environment.borrow_mut().parent = Some(self.clone());
        child_environment
    }

    fn unnest(&self) -> Self {
        if let Some(parent_environment) = &self.borrow().parent {
            parent_environment.clone()
        } else {
            panic!("No parent environment to unnest to.")
        }
    }
}

impl From<HashMap<String, Option<LoxType>>> for Environment {
    fn from(values: HashMap<String, Option<LoxType>>) -> Self {
        Self {
            parent: None,
            values,
        }
    }
}

impl From<Environment> for Rc<RefCell<Environment>> {
    fn from(value: Environment) -> Self {
        Rc::new(RefCell::new(value))
    }
}
