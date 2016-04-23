use vm::{Value, Class, symref};

use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug)]
pub enum Object {
    Scalar { class: Rc<Class>, fields: HashMap<symref::Field, Value> },
    Array { class: Rc<Class>, array: Vec<Value> },
}

