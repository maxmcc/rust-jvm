use vm::{Value, Class, handle};

use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug)]
pub enum Object {
    Scalar { class: Rc<Class>, fields: HashMap<handle::Field, Value> },
    Array { class: Rc<Class>, array: Vec<Value> },
}

