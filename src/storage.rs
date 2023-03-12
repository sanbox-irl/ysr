use std::collections::HashMap;

use crate::YarnValue;

/// This is a storage mechanism for yarn variables. It is a wrapper
/// around a HashMap for now, but in the future, that might change.
pub struct YarnStorage(HashMap<String, YarnValue>);

impl YarnStorage {
    /// Convenience method to insert an f32 into the map.
    pub fn insert_f32(&mut self, key: String, value: f32) -> Option<YarnValue> {
        self.insert(key, YarnValue::F32(value))
    }

    /// Convenience method to insert a String into the map.
    pub fn insert_string(&mut self, key: String, value: String) -> Option<YarnValue> {
        self.insert(key, YarnValue::Str(value))
    }

    /// Convenience method to insert a bool into the map.
    pub fn insert_bool(&mut self, key: String, value: bool) -> Option<YarnValue> {
        self.insert(key, YarnValue::Bool(value))
    }
}

impl std::ops::Deref for YarnStorage {
    type Target = HashMap<String, YarnValue>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::DerefMut for YarnStorage {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
