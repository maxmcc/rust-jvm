use std::fmt;

use vm::{sig, symref};
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

const NOP: &'static Fn(Vec<Value>) -> Option<Value> = &(|_| None);

pub fn bind(symref: &symref::Method) -> Option<NativeMethod> {
    let object_symref = symref::Class {
        sig: sig::Class::Scalar(String::from("java/lang/System")),
    };
    let register_natives_sig = sig::Method {
        name: String::from("registerNatives"),
        params: vec![],
        return_ty: None,
    };
    let register_natives_symref = symref::Method {
        class: object_symref,
        sig: register_natives_sig,
    };

    if *symref == register_natives_symref {
        Some(NativeMethod(NOP))
    } else {
        None
    }
}
