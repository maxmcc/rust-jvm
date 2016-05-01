//! The public interface for the Java virtual machine.

mod bytecode;
mod class;
mod class_loader;
mod constant_pool;
mod frame;
mod native;
mod value;

use self::class_loader::ClassLoader;

/// A symbolic reference to an entity in the runtime constant pool (§5.1). Symbolic references
/// must be resolved (§5.4.3) before their usage by the interpreter.
pub mod symref {
    use vm::sig;

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    /// A symbolic reference to a class constant pool entry.
    pub struct Class {
        /// The signature of the class to which the symbolic reference refers.
        pub sig: sig::Class,
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    /// A symbolic reference to a field constant pool entry.
    pub struct Field {
        /// A symbolic reference to the class containing this field.
        pub class: Class,
        /// The signature of the field to which the symbolic reference refers.
        pub sig: sig::Field,
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    /// A symbolic reference to a method constant pool entry.
    pub struct Method {
        /// A symbolic reference to the class containing this method.
        pub class: Class,
        /// The signature of the method to which the symbolic reference refers.
        pub sig: sig::Method,
    }
}

/// Signatures of runtime constant pool entities that serve to uniquely identify those entities.
/// These are derived from structures in the binary representation of the constant pool (§5.1).
pub mod sig {
    use std::num::Wrapping;
    use vm::value::Value;

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    /// Java language type information.
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

        /// Compute a sequence of types from a JVM-internal string representation.
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

        /// Compute a type from a JVM-internal string representation.
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

        /// Get the default value for the type. Java's default values for integers is `0`, floats
        /// `0.0`, characters `'\0'`, booleans `false`, and references `null`.
        pub fn default_value(&self) -> Value {
            match *self {
                Type::Byte | Type::Char | Type::Int | Type::Short | Type::Boolean =>
                    Value::Int(Wrapping(0)),
                Type::Double => Value::Double(0.0),
                Type::Float => Value::Float(0.0),
                Type::Long => Value::Long(Wrapping(0)),
                Type::Reference(_) => Value::NullReference,
            }
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    /// A class signature.
    pub enum Class {
        /// The signature of a non-array class, parametrized by its name.
        Scalar(String),
        /// The signature of an array class, parametrized by the type of elements in the array.
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
    /// A field signature.
    pub struct Field {
        /// The name of the field.
        pub name: String,
        /// The type of the field.
        pub ty: Type,
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    /// A method signature.
    pub struct Method {
        /// The name of the method.
        pub name: String,
        /// The types of the parameters of the method.
        pub params: Vec<Type>,
        /// The return type of the method, or `None` if the method is `void`.
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

#[derive(Debug)]
/// The top-level virtual machine. The virtual machine contains a reference to its _bootstrap class
/// loader_, which is used to load the main class and all of that class's dependencies.
///
/// The virtual machine's `start` method causes initialization of the boostrap class loader, which
/// loads the main class and invokes the `main(String[])` method to begin execution of the program.
/// The virtual machine terminates either when the program completes successfully, or when there is
/// an uncaught error in the program or the internal implementation of the virtual machine itself.
pub struct VirtualMachine {
    /// The bootstrap class loader used to initialize the virtual machine.
    bootstrap_class_loader: ClassLoader,
}

impl VirtualMachine {
    pub fn new() -> Self {
        VirtualMachine {
            bootstrap_class_loader: ClassLoader::new(),
        }
    }

    /// Begin execution of the virtual machine instance's `main(String[])` method.
    pub fn start(mut self, main_class: symref::Class) {
        let class = self.bootstrap_class_loader.load_class(&main_class.sig).unwrap();
        class.initialize(&mut self.bootstrap_class_loader);
        let string_ty = sig::Type::Reference(sig::Class::Scalar(String::from("java/lang/String")));
        let string_array_ty = sig::Type::Reference(sig::Class::Array(Box::new(string_ty)));
        let main_sig = sig::Method {
            name: String::from("main"),
            params: vec![string_array_ty],
            return_ty: None,
        };
        let main_symref = symref::Method {
            class: main_class,
            sig: main_sig,
        };
        let method = class.resolve_method(&main_symref);
        method.invoke(&class, &mut self.bootstrap_class_loader, vec![]);
    }
}

