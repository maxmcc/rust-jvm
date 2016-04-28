pub mod bytecode;
pub mod constant_pool;
pub mod stack;
pub mod heap;
mod class_loader;

use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use util::one_indexed_vec::OneIndexedVec;

pub use vm::constant_pool::{handle, symref, RuntimeConstantPool};
pub use vm::heap::Object;
use model::class_file::{ClassFile, MethodInfo};
use model::class_file::access_flags;
use model::class_file::attribute::{AttributeInfo, ExceptionTableEntry};
use vm::constant_pool::handle::Type;

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
    class_fields: HashMap<handle::Field, Value>,
    instance_fields: HashSet<handle::Field>,
    methods: HashMap<handle::Method, Method>,
}

impl Class {
    pub fn new(symref: symref::Class, superclass: Option<Rc<Class>>,
               constant_pool: RuntimeConstantPool, class_file: ClassFile) -> Self {
        let mut class_fields = HashMap::new();
        let mut instance_fields = HashSet::new();
        for field_info in class_file.fields {
            let name = constant_pool.force_string(field_info.name_index);
            let ty = Type::new(constant_pool.force_string(field_info.descriptor_index));
            let handle = handle::Field { name: name, ty: ty };
            if field_info.access_flags & access_flags::field_access_flags::ACC_STATIC != 0 {
                let default_value =
                    match ty {
                        Type::Byte | Type::Char | Type::Int | Type::Short | Type::Boolean =>
                            Value::Int(0),
                        Type::Double => Value::Double(0.0),
                        Type::Float => Value::Float(0.0),
                        Type::Long => Value::Long(0),
                        Type::Reference(_) => Value::NullReference,
                    };
                class_fields.insert(handle, default_value);
            } else {
                instance_fields.insert(handle);
            }
        }

        let mut methods = HashMap::new();
        for method_info in class_file.methods {
            let name = constant_pool.force_string(method_info.name_index);
            let descriptor = constant_pool.force_string(method_info.descriptor_index);
            let handle = handle::Method::new(&name, &descriptor);
            methods.insert(handle, Method::new(method_info));
        }

        Class {
            symref: symref,
            superclass: superclass,
            constant_pool: constant_pool,
            class_fields: class_fields,
            instance_fields: instance_fields,
            methods: methods,
        }
    }

    pub fn new_array(object_class: Rc<Class>, component_type: handle::Type) -> Self {
        let length_field = handle::Field {
            name: String::from("length"),
            ty: handle::Type::Int,
        };
        let empty_constant_pool = OneIndexedVec::from(vec![]);
        let mut instance_fields = HashSet::new();
        instance_fields.insert(length_field);
        Class {
            symref: symref::Class { handle: handle::Class::Array(Box::new(component_type)) },
            superclass: Some(object_class.clone()),
            constant_pool: RuntimeConstantPool::new(&empty_constant_pool),
            class_fields: HashMap::new(),
            instance_fields: instance_fields,
            methods: HashMap::new(),
        }
    }
}

#[derive(Debug)]
pub struct Method {
    symref: symref::Method,
    max_stack: u16,
    max_locals: u16,
    code: Vec<bytecode::Instruction>,
    exception_table: Vec<ExceptionTableEntry>,
}

impl Method {
    pub fn new(symref: symref::Method, method_info: MethodInfo) -> Self {
        for attribute_info in method_info.attributes {
            match attribute_info {
                AttributeInfo::Code { max_stack, max_locals, code, exception_table, .. } => {
                    // TODO translate the code and exception_table
                    panic!("not yet implemented");
                },

                _ => (),
            }
        }
        panic!("no Code attribute in MethodInfo")
    }
}
