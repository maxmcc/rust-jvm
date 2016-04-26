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

impl Class {
    pub fn new(symref: symref::Class, superclass: Option<Rc<Class>>,
               constant_pool: RuntimeConstantPool, methods: HashMap<handle::Method, Method>,
               class_fields: HashMap<handle::Field, Value>, instance_fields: HashSet<handle::Field>)
            -> Class {
        Class {
            symref: symref,
            superclass: superclass,
            constant_pool: constant_pool,
            methods: methods,
            class_fields: class_fields,
            instance_fields: instance_fields,
        }
    }

    pub fn new_array(object_class: Rc<Class>, component_type: handle::Type) -> Class {
        let length_field = handle::Field {
            name: String::from("length"),
            ty: handle::Type::Int,
        };
        let instance_fields = HashSet::new();
        instance_fields.insert(length_field);
        Class {
            symref: symref::Class { handle: handle::Class::Array(Box::new(component_type)) },
            superclass: Some(object_class.clone()),
            constant_pool: Vec::new(),
            methods: HashMap::new(),
            class_fields: HashMap::new(),
            instance_fields: instance_fields,
        }
    }
}

#[derive(Debug)]
pub struct Method {
    symref: symref::Method,
    code: Vec<bytecode::Instruction>,
}

