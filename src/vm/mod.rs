//! Structures for linked classes.

mod bytecode;
mod class;
mod class_loader;
mod constant_pool;
mod frame;
mod native;
mod value;

use self::class_loader::ClassLoader;

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
    use std::num::Wrapping;
    use vm::value::Value;

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

#[derive(Debug)]
pub struct VirtualMachine {
    bootstrap_class_loader: ClassLoader,
}

impl VirtualMachine {
    pub fn new() -> Self {
        VirtualMachine {
            bootstrap_class_loader: ClassLoader::new(),
        }
    }

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

