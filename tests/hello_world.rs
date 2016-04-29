#![cfg(test)]

extern crate rust_jvm;
extern crate nom;

use self::rust_jvm::parser::class_file::parse_class_file;
use self::rust_jvm::vm::{symref, ClassLoader};
use self::rust_jvm::vm::sig;
use self::rust_jvm::vm::stack::Frame;


#[test]
fn test_hello_world() {
    let data = include_bytes!("../data/HelloWorld.class");
    println!("{:#?}", parse_class_file(data));

    let mut loader = ClassLoader::new();
    let class_sig = sig::Class::new("HelloWorld");
    let class = loader.load_class(&class_sig).unwrap();
    let method = class.resolve_method(&symref::Method {
        class: symref::Class { sig: class_sig },
        sig: sig::Method::new("main", "([Ljava/lang/String;)V"),
    });
    let frame = Frame::new(&method, class.get_constant_pool(), Vec::new());
    let opt_value = frame.run(&mut loader);
    println!("{:#?}", opt_value);
    panic!();
}
