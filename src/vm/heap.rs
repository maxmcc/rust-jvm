use vm::{Value, Class, sig};

use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug)]
pub enum Object {
    Scalar { class: Rc<Class>, fields: HashMap<sig::Field, Value> },
    Array { class: Rc<Class>, array: Vec<Value> },
}

impl Object {
    pub fn get_class(&self) -> Rc<Class> {
        match *self {
            Object::Scalar { ref class, .. } => class.clone(),
            Object::Array { ref class, .. } => class.clone(),
        }
    }
}
