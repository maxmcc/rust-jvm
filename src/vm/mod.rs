pub mod bytecode;
pub mod constant_pool;
pub mod stack;
pub mod heap;
pub mod handle;
mod class_loader;

pub use vm::constant_pool::{symref, RuntimeConstantPool};
pub use vm::heap::Object;

use std::cell::RefCell;
use std::rc::Rc;
use std::collections::{HashMap, HashSet};

/// A value in the Java virtual machine.
#[derive(Debug, Clone)]
pub enum Value {
    /// A 32-bit signed integral type, representing the Java types `byte`, `char`, `short`, `int`,
    /// and `boolean`.
    Int(i32),
    /// A 32-bit floating-point type, representing the Java type `float`.
    Float(f32),
    /// A 64-bit signed integral type, representing the Java type `long`.
    Long(i64),
    /// A 64-bit floating-point type, representing the Java type `double`.
    Double(f64),
    /// A reference to a Java object in the heap.
    Reference(Rc<RefCell<Object>>),
    /// A reference to a Java object which is `null`.
    NullReference,
}

#[derive(Debug)]
pub struct Class {
    symref: symref::Class,
    superclass: Option<Rc<Class>>,
    constant_pool: RuntimeConstantPool,
    methods: HashMap<handle::Method, Method>,
    class_fields: HashMap<handle::Field, Value>,
    instance_fields: HashSet<handle::Field>,
}

#[derive(Debug)]
pub struct Method {
    symref: symref::Method,
    code: Vec<bytecode::Instruction>,
}

