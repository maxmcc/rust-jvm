//! Structures for linked classes.

pub mod bytecode;
pub mod constant_pool;
pub mod stack;
pub mod heap;
mod class_loader;

use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use util::one_indexed_vec::OneIndexedVec;

pub use vm::constant_pool::{handle, RuntimeConstantPool};
pub use vm::heap::Object;
pub use vm::class_loader::ClassLoader;
use model::class_file::{ClassFile, MethodInfo};
use model::class_file::access_flags;
use model::class_file::attribute::{AttributeInfo, ExceptionTableEntry};
use vm::constant_pool::handle::Type;

/// References to unlinked structures from the runtime constant pool.
pub mod symref {
    use vm::handle;

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct Class {
        pub handle: handle::Class,
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct Field {
        pub class: Class,
        pub handle: handle::Field,
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct Method {
        pub class: Class,
        pub handle: handle::Method,
    }
}

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

/// A JVM representation of a class that has been loaded.
#[derive(Debug)]
pub struct Class {
    /// A symbolic reference to the class, comprised of its name (if a scalar type) or element type
    /// (if an array class).
    symref: symref::Class,
    /// The superclass extended by the class. If the class is `java/lang/Object`, this is `None`.
    superclass: Option<Rc<Class>>,
    /// The runtime constant pool of the current class, created from the constant pool defined in
    /// the `.class` file that has been loaded.
    constant_pool: RuntimeConstantPool,
    /// The `static` fields of the class, mapped to their values.
    class_fields: HashMap<handle::Field, Value>,
    /// The names of the non-`static` fields of an instance of this class.
    instance_fields: HashSet<handle::Field>,
    /// The methods of the class, mapped to their method handles.
    methods: HashMap<handle::Method, Method>,
}

impl Class {
    pub fn new(symref: symref::Class, superclass: Option<Rc<Class>>,
               constant_pool: RuntimeConstantPool, class_file: ClassFile) -> Self {
        let mut class_fields = HashMap::new();
        let mut instance_fields = HashSet::new();
        for field_info in class_file.fields {
            let name = constant_pool.lookup_raw_string(field_info.name_index);
            let ty = Type::new(&constant_pool.lookup_raw_string(field_info.descriptor_index));
            let handle = handle::Field { name: name, ty: ty };
            if field_info.access_flags & access_flags::field_access_flags::ACC_STATIC != 0 {
                let default_value = handle.ty.default_value();
                class_fields.insert(handle, default_value);
            } else {
                instance_fields.insert(handle);
            }
        }

        let mut methods = HashMap::new();
        for method_info in class_file.methods {
            let name = constant_pool.lookup_raw_string(method_info.name_index);
            let descriptor = constant_pool.lookup_raw_string(method_info.descriptor_index);
            let handle = handle::Method::new(&name, &descriptor);
            let method_symref = symref::Method { class: symref.clone(), handle: handle.clone() };
            methods.insert(handle, Method::new(method_symref, method_info));
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

    /// Create a new array class for a given element type.
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

    pub fn get_constant_pool(&self) -> &RuntimeConstantPool {
        &self.constant_pool
    }

    // TODO access control
    pub fn resolve_method(&self, method_symref: &symref::Method) -> &Method {
        // TODO check if this is an interface
        self.find_method(&method_symref.handle).expect("NoSuchMethodError")
    }

    fn find_method(&self, method_handle: &handle::Method) -> Option<&Method> {
        self.methods.get(method_handle).or_else(|| {
            self.superclass.as_ref().and_then(|superclass| superclass.find_method(method_handle))
        })
    }

    pub fn dispatch_method(&self, resolved_method: &Method) -> Option<(&Class, &Method)> {
        self.methods.get(&resolved_method.symref.handle).and_then(|our_method| {
            if our_method.access_flags & access_flags::method_access_flags::ACC_PRIVATE != 0
                    || our_method.access_flags & access_flags::method_access_flags::ACC_STATIC != 0 {
                None
            } else if resolved_method.access_flags & access_flags::method_access_flags::ACC_PUBLIC == 0
                    && resolved_method.access_flags & access_flags::method_access_flags::ACC_PROTECTED == 0
                    && resolved_method.access_flags & access_flags::method_access_flags::ACC_PRIVATE == 0 {
                // the resolved method is declared as package-private
                if self.symref.handle.get_package() == resolved_method.symref.class.handle.get_package() {
                    Some((self, our_method))
                } else {
                    None
                }
            } else {
                Some((self, our_method))
            }
        }).or_else({||
            self.superclass.as_ref().and_then(|superclass| superclass.dispatch_method(resolved_method))
        })
    }
}

#[derive(Debug)]
pub struct Method {
    /// The method's signature, comprised of its name and argument and return types.
    pub symref: symref::Method,
    /// The method's access flags.
    pub access_flags: u16,
    /// The method's bytecode instructions.
    pub code: Vec<u8>,
    /// The method's exception table, used for catching `Throwable`s. Order is significant.
    pub exception_table: Vec<ExceptionTableEntry>,
}

impl Method {
    pub fn new(symref: symref::Method, method_info: MethodInfo) -> Self {
        for attribute_info in method_info.attributes {
            match attribute_info {
                AttributeInfo::Code { code, exception_table, .. } => {
                    return Method {
                        symref: symref,
                        access_flags: method_info.access_flags,
                        code: code,
                        exception_table: exception_table,
                    }
                },
                _ => (),
            }
        }
        panic!("no Code attribute in MethodInfo")
    }
}
