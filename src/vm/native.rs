use std::fmt;
use std::num::Wrapping;

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

const ARRAYCOPY: &'static Fn(Vec<Value>) -> Option<Value> = &(|args| {
    if let Value::ArrayReference(ref src_rc) = args[0] {
        if let Value::Int(Wrapping(src_offset)) = args[1] {
            if let Value::ArrayReference(ref dest_rc) = args[2] {
                if let Value::Int(Wrapping(dest_offset)) = args[3] {
                    if let Value::Int(Wrapping(len)) = args[4] {
                        let src = src_rc.borrow();
                        let mut dest = dest_rc.borrow_mut();
                        for i in 0..len {
                            let value = src.get(src_offset + i);
                            dest.put(dest_offset + i, value);
                        }
                    } else {
                        panic!("length must be an int");
                    }
                } else {
                    panic!("dest_offset must be an int");
                }
            } else {
                panic!("ArrayStoreException");
            }
        } else {
            panic!("src_offset must be an int");
        }
    } else {
        panic!("ArrayStoreException");
    }
    None
});

pub fn bind(symref: &symref::Method) -> Option<NativeMethod> {
    let system_symref = symref::Class {
        sig: sig::Class::Scalar(String::from("java/lang/System")),
    };

    let register_natives_sig = sig::Method {
        name: String::from("registerNatives"),
        params: vec![],
        return_ty: None,
    };
    let register_natives_symref = symref::Method {
        class: system_symref.clone(),
        sig: register_natives_sig,
    };

    let object_ty = sig::Type::Reference(sig::Class::Scalar(String::from("java/lang/Object")));
    let arraycopy_sig = sig::Method {
        name: String::from("arraycopy"),
        params: vec![object_ty.clone(), sig::Type::Int, object_ty.clone(), sig::Type::Int,
                     sig::Type::Int],
        return_ty: None,
    };
    let arraycopy_symref = symref::Method {
        class: system_symref.clone(),
        sig: arraycopy_sig,
    };

    if *symref == register_natives_symref {
        Some(NativeMethod(NOP))
    } else if *symref == arraycopy_symref {
        Some(NativeMethod(ARRAYCOPY))
    } else {
        None
    }
}
