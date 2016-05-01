#![cfg(test)]

extern crate rust_jvm;
extern crate nom;

use self::rust_jvm::vm::{sig, symref, VirtualMachine};

#[test]
fn test_hello_world() {
    let class_sig = sig::Class::new("HelloWorld");
    let class_symref = symref::Class { sig: class_sig };
    let vm = VirtualMachine::new();
    vm.start(class_symref);
}
