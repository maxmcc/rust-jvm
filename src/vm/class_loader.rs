use vm::symref;
use std::collections::HashMap;

#[derive(Debug)]
pub struct ClassLoader {
    classes: HashMap<symref::Class, vm::Class>,
    methods: HashMap<symref::Method, vm::Method>,
    fields: HashMap<symref::Field, vm::Value>,
}

