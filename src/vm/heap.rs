use vm::{Value, Class, symref};

use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug)]
pub struct Object {
    class: Rc<Class>,
    fields: HashMap<symref::Field, Value>,
}

