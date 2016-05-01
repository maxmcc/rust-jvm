use std::fmt;

use vm::symref;
use vm::value::Value;

pub struct NativeMethod(&'static Fn(Vec<Value>) -> Option<Value>);

impl fmt::Debug for NativeMethod {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<native method>")
    }
}

impl NativeMethod {
    pub fn invoke(&self, args: Vec<Value>) -> Option<Value> {
        self.0(args)
    }
}

pub fn bind(symref: &symref::Method) -> Option<NativeMethod> {
    None
}
