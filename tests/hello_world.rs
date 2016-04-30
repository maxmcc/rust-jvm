#![cfg(test)]

extern crate rust_jvm;
extern crate nom;

use self::rust_jvm::vm::{symref, ClassLoader};
use self::rust_jvm::vm::sig;
use self::rust_jvm::vm::frame::Frame;

#[test]
fn test_hello_world() {
    let mut loader = ClassLoader::new();
    let class_sig = sig::Class::new("HelloWorld");
    let class = loader.load_class(&class_sig).unwrap();
    let method = class.resolve_method(&symref::Method {
        class: symref::Class { sig: class_sig },
        sig: sig::Method::new("main", "([Ljava/lang/String;)V"),
    });
    let method_code = method.method_code.as_ref().unwrap();
    let frame = Frame::new(&class, method_code, Vec::new());
    let opt_value = frame.run(&mut loader);
    println!("{:#?}", opt_value);
    panic!();
}
