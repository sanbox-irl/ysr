use std::collections::HashMap;

use crate::Value;

/// This is a storage mechanism for yarn variables. It is a wrapper
/// around a HashMap for now, but in the future, that might change.
#[derive(Debug, Default)]
pub struct Storage(HashMap<String, Value>);

impl Storage {
    /// Creates a new YarnStorage
    pub fn new() -> Self {
        Self::default()
    }

    /// Convenience method to insert an f32 into the map.
    pub fn insert_f32(&mut self, key: String, value: f32) -> Option<Value> {
        self.insert(key, Value::F32(value))
    }

    /// Convenience method to insert a String into the map.
    pub fn insert_string(&mut self, key: String, value: String) -> Option<Value> {
        self.insert(key, Value::Str(value))
    }

    /// Convenience method to insert a bool into the map.
    pub fn insert_bool(&mut self, key: String, value: bool) -> Option<Value> {
        self.insert(key, Value::Bool(value))
    }
}

impl std::ops::Deref for Storage {
    type Target = HashMap<String, Value>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::DerefMut for Storage {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
