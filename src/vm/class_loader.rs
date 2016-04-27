use std::error;
use std::fmt;
use std::fs::File;
use std::io;
use std::io::Read;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use nom;

use model::class_file::ClassFile;
use parser::class_file;
use vm;
use vm::constant_pool::{RuntimeConstantPool, RuntimeConstantPoolEntry};
use vm::handle;
use vm::symref;

#[derive(Debug)]
pub enum Error {
    /// If no "purported representation" of the class is found. §5.3.1.
    ClassNotFound(io::Error),
    /// The "purported representation" does not follow the class file format. §5.3.5.
    ClassFormat,
    /// The "purported representation" is not of a supported version. §5.3.5.
    UnsupportedVersion { major: u16, minor: u16 },
    /// The "purported representation" does not actually represent the requested class. §5.3.5.
    NoClassDefFound,
    /// Declared superclasses are actually interfaces or vice versa. §5.3.5.
    IncompatibleClassChange(String),
    /// The class is its own superclass or superinterface. §5.3.5.
    ClassCircularity,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Error::ClassNotFound(ref err) => write!(f, "ClassNotFound: {}", err),
            Error::ClassFormat => write!(f, "ClassFormat"),
            Error::UnsupportedVersion { major, minor } =>
                write!(f, "UnsupportedVersion {}.{}", major, minor),
            Error::NoClassDefFound => write!(f, "NoClassDefFound"),
            Error::IncompatibleClassChange(ref class) =>
                write!(f, "IncompatibleClassChange with {}", class),
            Error::ClassCircularity => write!(f, "ClassCircularity"),
        }
    }
}

impl error::Error for Error {
    fn description(&self) -> &str {
        match *self {
            Error::ClassNotFound(_) => "class representation not found due to I/O error",
            Error::ClassFormat => "invalid class format",
            Error::UnsupportedVersion { .. } => "unsupported version",
            Error::NoClassDefFound => "class representation is not of the requested class",
            Error::IncompatibleClassChange(_) => "declared superclass (superinterface) is actually \
                                                  an interface (class)",
            Error::ClassCircularity => "the class is its own superclass or superinterface",
        }
    }

    fn cause(&self) -> Option<&error::Error> {
        match *self {
            Error::ClassNotFound(ref err) => Some(err),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub struct ClassLoader {
    classes: HashMap<handle::Class, Rc<vm::Class>>,
    pending: HashSet<handle::Class>,
}

impl ClassLoader {
    fn new() -> ClassLoader {
        ClassLoader {
            classes: HashMap::new(),
            pending: HashSet::new(),
        }
    }

    /// Attempts to create, load, and prepare the specified using the bootstrap class loader
    /// implementation. The bootstrap class loader searches the current directory for a class file
    /// with the correct fully-qualified name. If none is found, the bootstrap class loader then
    /// attempts to load the class from the standard library JAR.
    ///
    /// This implementation lazily resolves symbolic references, so no resolution of references
    /// within the loaded class is performed by this function.
    ///
    /// This implementation does not attempt to perform bytecode verification; we assume that any
    /// class files we attempt to load are valid.
    pub fn load_class(&mut self, handle: &handle::Class) -> Result<Rc<vm::Class>, Error> {
        if self.pending.contains(&handle) {
            // we're already resolving this name
            Err(Error::ClassCircularity)
        } else if let Some(class) = self.classes.get(&handle) {
            // the class is already resolved
            Ok(class.clone())
        } else {
            self.pending.insert(handle.clone());
            let res = match *handle {
                handle::Class::Scalar(ref name) => {
                    let class_bytes = try!(self.find_class_bytes(name)
                                               .map_err(|err| Error::ClassNotFound(err)));
                    let (class_file, rcp) = try!(self.derive_class(&handle, &class_bytes));
                    panic!("not yet implemented")
                },

                handle::Class::Array(ref component_type) => {
                    // load the component type class, even though we don't use it, to ensure that
                    // any errors resulting from the load happen at the right time
                    match **component_type {
                        handle::Type::Byte | handle::Type::Char | handle::Type::Double
                            | handle::Type::Float | handle::Type::Int | handle::Type::Long
                            | handle::Type::Short | handle::Type::Boolean => Ok(None),
                        handle::Type::Reference(ref component_handle) =>
                            self.load_class(component_handle).map(|class| Some(class))
                    }.and_then(|_| {
                        let object_name = String::from("java/lang/Object");
                        let object_handle = handle::Class::Scalar(object_name);
                        self.load_class(&object_handle).map(|object_class| {
                            let class = vm::Class::new_array(object_class, **component_type);
                            let rc = Rc::new(class);
                            self.classes.insert(handle.clone(), rc.clone());
                            rc
                        })
                    })
                },
            };
            self.pending.remove(&handle);
            res
        }
    }

    fn find_class_bytes(&mut self, name: &str) -> Result<Vec<u8>, io::Error> {
        // isn't this so convenient!
        let file_name = String::from(name) + ".class";
        File::open(file_name).and_then(|mut file| {
            let mut res = vec![];
            file.read_to_end(&mut res).map(|_| res)
        })
    }

    fn resolve_class_symref(&mut self, rcp: &RuntimeConstantPool, index: u16)
            -> Result<Rc<vm::Class>, Error> {
        if let Some(RuntimeConstantPoolEntry::ClassRef(ref class_symref)) = rcp[index] {
            self.load_class(&class_symref.handle)
        } else {
            Err(Error::ClassFormat)
        }
    }

    fn derive_class(&mut self, handle: &handle::Class, class_bytes: &[u8])
            -> Result<(ClassFile, RuntimeConstantPool), Error> {
        // TODO we discard the parse errors, but it's so hard to fix that...
        let parsed_class = try!(
            match class_file::parse_class_file(&class_bytes) {
                Ok(nom::IResult::Done(_, parsed_class)) => Ok(parsed_class),
                Ok(nom::IResult::Incomplete(_)) => Err(Error::ClassFormat),
                Ok(nom::IResult::Error(_)) => Err(Error::ClassFormat),
                Err(_) => Err(Error::ClassFormat),
            }
        );
        if parsed_class.major_version == 51 && parsed_class.minor_version == 0 {
            let rcp = RuntimeConstantPool::new(&parsed_class.constant_pool);
            let this_entry = &rcp[parsed_class.this_class];
            if let Some(RuntimeConstantPoolEntry::ClassRef(this_symref)) = *this_entry {
                if *handle == this_symref.handle {
                    let superclass =
                        try!(self.resolve_class_symref(&rcp, parsed_class.super_class));
                    for interface in &parsed_class.interfaces {
                        try!(self.resolve_class_symref(&rcp, *interface));
                    }
                    Ok((parsed_class, rcp))
                } else {
                    Err(Error::NoClassDefFound)
                }
            } else {
                Err(Error::ClassFormat)
            }
        } else {
            Err(Error::UnsupportedVersion {
                major: parsed_class.major_version,
                minor: parsed_class.minor_version,
            })
        }
    }
}
