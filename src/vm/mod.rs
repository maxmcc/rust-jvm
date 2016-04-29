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

pub use vm::constant_pool::RuntimeConstantPool;
pub use vm::heap::Object;
pub use vm::class_loader::ClassLoader;
use model::class_file::{ClassFile, MethodInfo};
use model::class_file::access_flags;
use model::class_file::attribute::{AttributeInfo, ExceptionTableEntry};

/// References to unlinked structures from the runtime constant pool.
pub mod symref {
    use vm::sig;

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct Class {
        pub sig: sig::Class,
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct Field {
        pub class: Class,
        pub sig: sig::Field,
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct Method {
        pub class: Class,
        pub sig: sig::Method,
    }
}

/// Descriptors for things in the runtime constant pool.
pub mod sig {
    use vm::Value;

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub enum Type {
        Byte,
        Char,
        Double,
        Float,
        Int,
        Long,
        Short,
        Boolean,
        Reference(Class),
    }

    impl Type {
        pub fn new(type_str: &str) -> Self {
            let (ty, rest) = Self::new_partial(type_str).unwrap();
            if rest.len() > 0 {
                panic!("extra content at end of type descriptor")
            } else {
                ty
            }
        }

        pub fn new_multi(multi_type_str: &str) -> (Vec<Self>, &str) {
            let mut types = vec![];
            let mut remainder = multi_type_str;
            {
                let mut result = Ok(&mut types);
                while let Ok(types) = result {
                    result = Self::new_partial(remainder).map(|(ty, new_remainder)| {
                        types.push(ty);
                        remainder = new_remainder;
                        types
                    });
                }
            }
            (types, remainder)
        }

        fn new_partial(type_str: &str) -> Result<(Self, &str), ()> {
            let (specifier, rest) = type_str.split_at(1);
            match specifier {
                "B" => Ok((Type::Byte, rest)),
                "C" => Ok((Type::Char, rest)),
                "D" => Ok((Type::Double, rest)),
                "F" => Ok((Type::Float, rest)),
                "I" => Ok((Type::Int, rest)),
                "J" => Ok((Type::Long, rest)),
                "S" => Ok((Type::Short, rest)),
                "Z" => Ok((Type::Boolean, rest)),
                "L" => {
                    let end_index = rest.find(';').unwrap();
                    let (name_slice, rest) = rest.split_at(end_index);
                    let name = String::from(name_slice);
                    let scalar_type = Type::Reference(Class::Scalar(name));
                    Ok((scalar_type, rest.split_at(1).1))
                },
                "[" => {
                    let (component_type, rest) = try!(Self::new_partial(rest));
                    let array_type = Type::Reference(Class::Array(Box::new(component_type)));
                    Ok((array_type, rest))
                },
                _ => Err(())
            }
        }

        pub fn default_value(&self) -> Value {
            match *self {
                Type::Byte | Type::Char | Type::Int | Type::Short | Type::Boolean => Value::Int(0),
                Type::Double => Value::Double(0.0),
                Type::Float => Value::Float(0.0),
                Type::Long => Value::Long(0),
                Type::Reference(_) => Value::NullReference,
            }
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub enum Class {
        Scalar(String),
        Array(Box<Type>),
    }

    impl Class {
        pub fn new(name: &str) -> Self {
            if name.starts_with('[') {
                let (_, component_type_str) = name.split_at(1);
                let component_type = Type::new(component_type_str);
                Class::Array(Box::new(component_type))
            } else {
                Class::Scalar(String::from(name))
            }
        }

        pub fn get_package(&self) -> Option<String> {
            match *self {
                Class::Scalar(ref name) => {
                    match name.rfind('/') {
                        None => Some(String::from("")),
                        Some(index) => Some(String::from(name.split_at(index).0)),
                    }
                },
                Class::Array(_) => None,
            }
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct Field {
        pub name: String,
        pub ty: Type,
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct Method {
        pub name: String,
        pub params: Vec<Type>,
        pub return_ty: Option<Type>,
    }

    impl Method {
        pub fn new(name: &str, descriptor: &str) -> Self {
            if !descriptor.starts_with('(') {
                panic!("invalid method descriptor");
            }
            let params_str = descriptor.split_at(1).1;
            let (params, remainder) = Type::new_multi(params_str);
            let (close_paren, return_ty_str) = remainder.split_at(1);
            if close_paren != ")" {
                panic!("invalid method descriptor");
            }
            let return_ty =
                if return_ty_str == "V" {
                    None
                } else {
                    Some(Type::new(return_ty_str))
                };
            Method {
                name: String::from(name),
                params: params,
                return_ty: return_ty
            }
        }
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
    /// The access flags for the class.
    access_flags: u16,
    /// The superclass extended by the class. If the class is `java/lang/Object`, this is `None`.
    superclass: Option<Rc<Class>>,
    /// The runtime constant pool of the current class, created from the constant pool defined in
    /// the `.class` file that has been loaded.
    constant_pool: RuntimeConstantPool,
    /// The `static` fields of the class, mapped to their values.
    class_fields: HashMap<sig::Field, Value>,
    /// The names of the non-`static` fields of an instance of this class.
    instance_fields: HashSet<sig::Field>,
    /// The methods of the class, mapped to their method sigs.
    methods: HashMap<sig::Method, Method>,
}

impl Class {
    pub fn new(symref: symref::Class, superclass: Option<Rc<Class>>,
               constant_pool: RuntimeConstantPool, class_file: ClassFile) -> Self {
        let mut class_fields = HashMap::new();
        let mut instance_fields = HashSet::new();
        for field_info in class_file.fields {
            let name = constant_pool.lookup_raw_string(field_info.name_index);
            let ty = sig::Type::new(&constant_pool.lookup_raw_string(field_info.descriptor_index));
            let sig = sig::Field { name: name, ty: ty };
            if field_info.access_flags & access_flags::field_access_flags::ACC_STATIC != 0 {
                let default_value = sig.ty.default_value();
                class_fields.insert(sig, default_value);
            } else {
                instance_fields.insert(sig);
            }
        }

        let mut methods = HashMap::new();
        for method_info in class_file.methods {
            let name = constant_pool.lookup_raw_string(method_info.name_index);
            let descriptor = constant_pool.lookup_raw_string(method_info.descriptor_index);
            let sig = sig::Method::new(&name, &descriptor);
            let method_symref = symref::Method { class: symref.clone(), sig: sig.clone() };
            methods.insert(sig, Method::new(method_symref, method_info));
        }

        Class {
            symref: symref,
            access_flags: class_file.access_flags,
            superclass: superclass,
            constant_pool: constant_pool,
            class_fields: class_fields,
            instance_fields: instance_fields,
            methods: methods,
        }
    }

    /// Create a new array class for a given element type.
    pub fn new_array(object_class: Rc<Class>, component_access_flags: u16,
                     component_type: sig::Type) -> Self {
        let access_flags = (component_access_flags & 0x0001) | 0x1030;
        let length_field = sig::Field {
            name: String::from("length"),
            ty: sig::Type::Int,
        };
        let empty_constant_pool = OneIndexedVec::from(vec![]);
        let mut instance_fields = HashSet::new();
        instance_fields.insert(length_field);
        Class {
            symref: symref::Class { sig: sig::Class::Array(Box::new(component_type)) },
            access_flags: access_flags,
            superclass: Some(object_class.clone()),
            constant_pool: RuntimeConstantPool::new(&empty_constant_pool),
            class_fields: HashMap::new(),
            instance_fields: instance_fields,
            methods: HashMap::new(),
        }
    }

    pub fn get_access_flags(&self) -> u16 {
        self.access_flags
    }

    pub fn get_constant_pool(&self) -> &RuntimeConstantPool {
        &self.constant_pool
    }

    // TODO access control
    pub fn resolve_method(&self, method_symref: &symref::Method) -> &Method {
        // TODO check if this is an interface
        self.find_method(&method_symref.sig).expect("NoSuchMethodError")
    }

    pub fn find_method(&self, method_sig: &sig::Method) -> Option<&Method> {
        self.methods.get(method_sig).or_else(|| {
            self.superclass.as_ref().and_then(|superclass| superclass.find_method(method_sig))
        })
    }

    pub fn dispatch_method(&self, resolved_method: &Method) -> Option<(&Class, &Method)> {
        self.methods.get(&resolved_method.symref.sig).and_then(|our_method| {
            if our_method.access_flags & access_flags::method_access_flags::ACC_PRIVATE != 0
                    || our_method.access_flags & access_flags::method_access_flags::ACC_STATIC != 0 {
                None
            } else if resolved_method.access_flags & access_flags::method_access_flags::ACC_PUBLIC == 0
                    && resolved_method.access_flags & access_flags::method_access_flags::ACC_PROTECTED == 0
                    && resolved_method.access_flags & access_flags::method_access_flags::ACC_PRIVATE == 0 {
                // the resolved method is declared as package-private
                if self.symref.sig.get_package() == resolved_method.symref.class.sig.get_package() {
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

    pub fn is_descendant(&self, other: &Class) -> bool {
        if self.symref.sig == other.symref.sig {
            true
        } else {
            self.superclass.as_ref().map_or(false, |superclass| {
                superclass.is_descendant(other)
            })
        }
    }
}

#[derive(Debug)]
pub struct Method {
    /// The method's signature, comprised of its name and argument and return types.
    pub symref: symref::Method,
    /// The method's access flags.
    pub access_flags: u16,
    /// An optional MethodCode structure. Not present for abstract and native methods.
    pub method_code: Option<MethodCode>,
}

impl Method {
    pub fn new(symref: symref::Method, method_info: MethodInfo) -> Self {
        for attribute_info in method_info.attributes {
            match attribute_info {
                AttributeInfo::Code { code, exception_table, .. } => {
                    return Method {
                        symref: symref,
                        access_flags: method_info.access_flags,
                        method_code: Some(MethodCode {
                            code: code,
                            exception_table: exception_table,
                        }),
                    }
                },
                _ => (),
            }
        }

        Method {
            symref: symref,
            access_flags: method_info.access_flags,
            method_code: None,
        }
    }
}

#[derive(Debug)]
pub struct MethodCode {
    /// The method's bytecode instructions.
    pub code: Vec<u8>,
    /// The method's exception table, used for catching `Throwable`s. Order is significant.
    pub exception_table: Vec<ExceptionTableEntry>,
}
