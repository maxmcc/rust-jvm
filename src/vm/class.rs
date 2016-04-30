//! Internal JVM representations of classes and methods.

use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use model::class_file::{access_flags, ClassFile, constant_pool_index, MethodInfo};
use model::class_file::attribute::{AttributeInfo, ExceptionTableEntry};
use util::one_indexed_vec::OneIndexedVec;
use vm::{sig, symref};
use vm::class_loader::ClassLoader;
use vm::constant_pool::RuntimeConstantPool;
use vm::frame::Frame;
use vm::value::Value;

/// A JVM representation of a class that has been loaded.
#[derive(Debug)]
pub struct Class {
    /// A symbolic reference to the class, comprised of its name (if a scalar type) or element type
    /// (if an array class).
    pub symref: symref::Class,
    /// The access flags for the class.
    pub access_flags: u16,
    /// The superclass extended by the class. If the class is `java/lang/Object`, this is `None`.
    pub superclass: Option<Rc<Class>>,
    /// The runtime constant pool of the current class, created from the constant pool defined in
    /// the `.class` file that has been loaded.
    constant_pool: RuntimeConstantPool,
    /// The fields of this class mapped to their access flags. This map includes both `static` and
    /// non-`static` fields. We don't separate them because it makes it easier to throw the correct
    /// runtime `Error` when certain invalid conditions are detected.
    fields: HashMap<sig::Field, u16>,
    /// The constants which populate the `static final` fields of this class. We don't immediately
    /// put these values into `class_fields` because they can include `String` literals, and we may
    /// not have loaded the `String` class yet. (This is also consistent with the spec, which
    /// states that these constants are set at class initialization time.)
    field_constants: HashMap<sig::Field, constant_pool_index>,
    /// The methods of the class, mapped to their method structures.
    methods: HashMap<sig::Method, Method>,
    /// The values of the static fields of this class. These are only set at class initialization.
    /// §5.5 of the JVM spec requires that initialization occur only at certain specific points,
    /// in particular:
    /// * When an instance of the class is created with the `new` instruction
    /// * When one of the `getstatic`, `putstatic`, or `invokestatic` instructions refers to one
    ///   of the static members of the class
    /// * When a subclass of the class is initialized
    /// * When the VM is about to begin executing the `main` method in the class containing the
    ///   overall program entry point
    /// Prior to initialization, this structure field contains `None`. After initialization, this
    /// field contains a `Some` with a `HashMap` value, which must contain the current values for
    /// each `static` field of this class.
    field_values: RefCell<Option<HashMap<sig::Field, Value>>>,
}

impl Class {
    pub fn new(symref: symref::Class, superclass: Option<Rc<Class>>,
               constant_pool: RuntimeConstantPool, class_file: ClassFile) -> Self {
        let mut fields = HashMap::new();
        let mut field_constants = HashMap::new();
        for field_info in class_file.fields {
            let name = constant_pool.lookup_raw_string(field_info.name_index);
            let ty = sig::Type::new(&constant_pool.lookup_raw_string(field_info.descriptor_index));
            let sig = sig::Field { name: name, ty: ty };
            if field_info.access_flags & access_flags::field_access_flags::ACC_STATIC != 0 {
                for attribute in field_info.attributes {
                    if let AttributeInfo::ConstantValue { constant_value_index } = attribute {
                        field_constants.insert(sig.clone(), constant_value_index);
                    }
                }
            }
            fields.insert(sig, field_info.access_flags);
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
            fields: fields,
            field_constants: field_constants,
            methods: methods,
            field_values: RefCell::new(None),
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
        let mut fields = HashMap::new();
        fields.insert(length_field, 0x1011);
        Class {
            symref: symref::Class { sig: sig::Class::Array(Box::new(component_type)) },
            access_flags: access_flags,
            superclass: Some(object_class.clone()),
            constant_pool: RuntimeConstantPool::new(&empty_constant_pool),
            fields: fields,
            field_constants: HashMap::new(),
            methods: HashMap::new(),
            field_values: RefCell::new(None),
        }
    }

    pub fn get_symref(&self) -> symref::Class {
        self.symref.clone()
    }

    pub fn get_access_flags(&self) -> u16 {
        self.access_flags
    }

    pub fn get_constant_pool(&self) -> &RuntimeConstantPool {
        &self.constant_pool
    }

    /// Find the method in the current class referred to by a given symbolic reference. If the
    /// method is not found, panics with a `NoSuchMethodError`.
    pub fn resolve_method(&self, method_symref: &symref::Method) -> &Method {
        // TODO access control
        // TODO check if this is an interface
        self.find_method(&method_symref.sig).expect("NoSuchMethodError")
    }

    /// Implements dynamic lookup of a method's signature in the current class. If no method with
    /// the given signature is found, then recursively searches the current class's superclasses.
    pub fn find_method(&self, method_sig: &sig::Method) -> Option<&Method> {
        self.methods.get(method_sig).or_else(|| {
            self.superclass.as_ref().and_then(|superclass| superclass.find_method(method_sig))
        })
    }

    /// Implements dynamic dispatch of a resolved method according to the lookup procedure
    /// specified for the `invokevirtual` instruction. Method resolution depends on whether the
    /// method in question overrides a superclass method. (See spec for more information.)
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

    /// Returns true if this class is a descendant (direct or indirect subclass) of another class.
    pub fn is_descendant(&self, other: &Class) -> bool {
        if self.symref.sig == other.symref.sig {
            true
        } else {
            self.superclass.as_ref().map_or(false, |superclass| {
                superclass.is_descendant(other)
            })
        }
    }

    /// Initialize the class by executing its class or interface initialization method.  Prior to
    /// initialization, a class or interface must be linked, that is, verified, prepared, and
    /// optionally resolved.
    fn initialize(&self, class_loader: &mut ClassLoader) {
        // we don't want to have the RefCell borrowed during the initializer
        // therefore, we borrow it in an inner scope and run the initializer later
        let run_initializer = {
            let mut field_values = self.field_values.borrow_mut();
            match *field_values {
                None => {
                    let mut map = HashMap::new();

                    // initialize all static fields to their default values
                    for (sig, access_flags) in &self.fields {
                        if access_flags & access_flags::field_access_flags::ACC_STATIC != 0 {
                            let default_value = sig.ty.default_value();
                            map.insert(sig.clone(), default_value);
                        }
                    }

                    // initialize fields with a ConstantValue attribute to those constant values
                    for (sig, index) in &self.field_constants {
                        let value = self.constant_pool.resolve_literal(*index, class_loader).unwrap();
                        map.insert(sig.clone(), value);
                    }

                    *field_values = Some(map);
                    true
                },

                Some(_) => false,
            }
        };

        if run_initializer {
            let clinit_sig = sig::Method {
                name: String::from("<clinit>"),
                params: vec![],
                return_ty: None,
            };
            match self.methods.get(&clinit_sig) {
                None => (),
                Some(ref method) => {
                    let method_code = method.method_code.as_ref().unwrap();
                    let frame = Frame::new(&self, method_code, vec![]);
                    match frame.run(class_loader) {
                        None => (),
                        Some(_) => panic!("<clinit> returned a value!"),
                    }
                },
            };
        }
    }

    /// Resolves a symbolic reference to a field and reads a value from that field.
    pub fn resolve_and_get_field(&self, symref: &symref::Field, class_loader: &mut ClassLoader)
            -> Value {
        self.initialize(class_loader);
        // TODO we're ignoring the superinterfaces
        // TODO: also not checking for static
        let field_values_opt = self.field_values.borrow();
        let field_values = field_values_opt.as_ref().unwrap();
        let value_opt = field_values.get(&symref.sig).map(|v| v.clone());
        value_opt.unwrap_or_else(move || {
            let superclass = self.superclass.as_ref().expect("NoSuchFieldError");
            superclass.resolve_and_get_field(symref, class_loader)
        })
    }

    /// Resolves a symbolic reference to a field and writes a new value to that field.
    pub fn resolve_and_put_field(&self, symref: &symref::Field, new_value: Value,
                                 class_loader: &mut ClassLoader) {
        self.initialize(class_loader);
        // TODO we're ignoring superinterfaces and not checking for static
        let mut field_values_opt = self.field_values.borrow_mut();
        let mut field_values = field_values_opt.as_mut().unwrap();
        if field_values.contains_key(&symref.sig) {
            field_values.insert(symref.sig.clone(), new_value);
        } else {
            let superclass = self.superclass.as_ref().expect("NoSuchFieldError");
            superclass.resolve_and_put_field(symref, new_value, class_loader);
        }
    }

    /// Returns a set of the signatures of the fields of an instance of this class.
    pub fn collect_instance_fields(&self) -> HashSet<sig::Field> {
        // TODO: fix semantics wrt private fields (which aren't inherited, but still exist in a
        // class object)
        let mut instance_fields = self.superclass.as_ref().map(|superclass| {
            superclass.collect_instance_fields()
        }).unwrap_or(HashSet::new());
        for (sig, access_flags) in &self.fields {
            if access_flags & access_flags::field_access_flags::ACC_STATIC == 0 {
                instance_fields.insert(sig.clone());
            }
        }
        instance_fields
    }
}

#[derive(Debug)]
/// A JVM representation of a method in a loaded class.
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
/// Code associated with a JVM method struct.
pub struct MethodCode {
    /// The method's bytecode instructions.
    pub code: Vec<u8>,
    /// The method's exception table, used for catching `Throwable`s. Order is significant.
    pub exception_table: Vec<ExceptionTableEntry>,
}

