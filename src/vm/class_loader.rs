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
use vm::{self, sig, symref};
use vm::constant_pool::{RuntimeConstantPool, RuntimeConstantPoolEntry};

#[derive(Debug)]
pub enum Error {
    /// If no "purported representation" of the class is found. §5.3.1.
    ClassNotFound { name: String, error: io::Error },
    /// The "purported representation" does not follow the class file format. §5.3.5.
    ClassFormat,
    /// The "purported representation" is not of a supported version. §5.3.5.
    UnsupportedVersion { major: u16, minor: u16 },
    /// The "purported representation" does not actually represent the requested class. §5.3.5.
    NoClassDefFound { name: String },
    /// (A subtlety here is that recursive class loading to load superclasses is performed as part
    /// of resolution (§5.3.5, step 3). Therefore, a ClassNotFoundException that results from a
    /// class loader failing to load a superclass must be wrapped in a NoClassDefFoundError.) §5.3
    NoClassDefFoundCause { name: String, not_found: String },
    IncompatibleClassChange(String),
    /// The class is its own superclass or superinterface. §5.3.5.
    ClassCircularity,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Error::ClassNotFound { ref name, ref error } => write!(f, "ClassNotFound: {}. {:?}",
                                                                   name, error),
            Error::ClassFormat => write!(f, "ClassFormat"),
            Error::UnsupportedVersion { major, minor } =>
                write!(f, "UnsupportedVersion {}.{}", major, minor),
            Error::NoClassDefFound { ref name } => write!(f, "NoClassDefFound: {}", name),
            Error::NoClassDefFoundCause { ref name, ref not_found } =>
                write!(f, "NoClassDefFound: {}. Caused by ClassNotFound: {}", name, not_found),
            Error::IncompatibleClassChange(ref class) =>
                write!(f, "IncompatibleClassChange with {}", class),
            Error::ClassCircularity => write!(f, "ClassCircularity"),
        }
    }
}

impl error::Error for Error {
    fn description(&self) -> &str {
        match *self {
            Error::ClassNotFound { .. } => "class representation not found due to I/O error",
            Error::ClassFormat => "invalid class format",
            Error::UnsupportedVersion { .. } => "unsupported version",
            Error::NoClassDefFound { .. } => "class representation is not of the requested class",
            Error::NoClassDefFoundCause { .. } => "no class definition because a superclass is not found",
            Error::IncompatibleClassChange(_) => "declared superclass (superinterface) is actually \
                                                  an interface (class)",
            Error::ClassCircularity => "the class is its own superclass or superinterface",
        }
    }

    fn cause(&self) -> Option<&error::Error> {
        match *self {
            Error::ClassNotFound { ref error, .. } => Some(error),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub struct ClassLoader {
    classes: HashMap<sig::Class, Rc<vm::Class>>,
    pending: HashSet<sig::Class>,
}

impl ClassLoader {
    pub fn new() -> ClassLoader {
        ClassLoader {
            classes: HashMap::new(),
            pending: HashSet::new(),
        }
    }

    fn find_class_bytes(&mut self, name: &str) -> Result<Vec<u8>, io::Error> {
        // isn't this so convenient!
        // FIXME: Set up classpath for find_class_bytes
        let file_name = String::from(name) + ".class";
        File::open(file_name).and_then(|mut file| {
            let mut res = vec![];
            file.read_to_end(&mut res).map(|_| res)
        })
    }

    fn get_class_ref(rcp: &RuntimeConstantPool, index: u16)-> Result<&symref::Class, Error> {
        if let Some(RuntimeConstantPoolEntry::ClassRef(ref class_symref)) = rcp[index] {
            Ok(class_symref)
        } else {
            Err(Error::ClassFormat)
        }
    }

    pub fn resolve_class(&mut self, symref: &symref::Class) -> Result<Rc<vm::Class>, Error> {
        // TODO check access modifiers
        self.load_class(&symref.sig)
    }

    /// Derives the super class (if it exists) of the specified class.
    fn derive_super_class(&mut self, name: &str, rcp: &RuntimeConstantPool,
                          class_file: &ClassFile) -> Result<Option<Rc<vm::Class>>, Error> {
        if class_file.super_class == 0 {
            Ok(None)
        } else {
            let super_symref = try!(Self::get_class_ref(rcp, class_file.super_class));
            self.resolve_class(&super_symref).map(Some)
        }
    }

    /// Derives the specified class and its interfaces, but not its superclass.
    fn derive_class(&mut self, original_name: &str, sig: &sig::Class, class_bytes: &[u8])
                    -> Result<Rc<vm::Class>, Error> {
        // TODO we discard the parse errors, but it's so hard to fix that...
        let parsed_class = try!(
            match class_file::parse_class_file(&class_bytes) {
                nom::IResult::Done(_, parsed_class) => Ok(parsed_class),
                nom::IResult::Incomplete(_) => Err(Error::ClassFormat),
                nom::IResult::Error(_) => Err(Error::ClassFormat),
            }
        );
        try!(
            if parsed_class.major_version != 52 || parsed_class.minor_version != 0 {
                Err(Error::UnsupportedVersion {
                    major: parsed_class.major_version,
                    minor: parsed_class.minor_version,
                })
            } else {
                Ok(())
            }
        );
        let rcp = RuntimeConstantPool::new(&parsed_class.constant_pool);
        let sig_matches = {
            let this_symref = try!(Self::get_class_ref(&rcp, parsed_class.this_class));
            *sig == this_symref.sig
        };
        if sig_matches {
            let super_class = try!(self.derive_super_class(original_name, &rcp, &parsed_class));
            // TODO: Check that the entry is actually an interface
            for interface in &parsed_class.interfaces {
                let iface_symref = try!(Self::get_class_ref(&rcp, *interface));
                try!(self.resolve_class(&iface_symref));
            }
            Ok(Rc::new(vm::Class::new(symref::Class { sig: sig.clone() }, super_class, rcp,
                                      parsed_class)))
        } else {
            Err(Error::NoClassDefFound { name: String::from(original_name) })
        }
    }

    /// Attempts to create, load, and prepare the specified class from the specified bytes. The
    /// bootstrap class loader searches the current directory for a class file with the correct
    /// fully-qualified name. If none is found, the bootstrap class loader then attempts to load
    /// the class from the standard library JAR.
    ///
    /// This implementation lazily resolves symbolic references, so no resolution of references
    /// within the loaded class is performed by this function.
    ///
    /// This implementation does not attempt to perform bytecode verification; we assume that any
    /// class files we attempt to load are valid.
    fn load_class_bytes(&mut self, name: &str, sig: &sig::Class, class_bytes: &[u8])
                            -> Result<Rc<vm::Class>, Error> {
        self.derive_class(name, sig, class_bytes)
    }

    /// Attempts to create, load, and prepare the specified class using the bootstrap class loader
    /// implementation. The bootstrap class loader searches the current directory for a class file
    /// with the correct fully-qualified name. If none is found, the bootstrap class loader then
    /// attempts to load the class from the standard library JAR.
    ///
    /// This implementation lazily resolves symbolic references, so no resolution of references
    /// within the loaded class is performed by this function.
    ///
    /// This implementation does not attempt to perform bytecode verification; we assume that any
    /// class files we attempt to load are valid.
    pub fn load_class(&mut self, sig: &sig::Class) -> Result<Rc<vm::Class>, Error> {
        if self.pending.contains(&sig) {
            // we're already resolving this name
            return Err(Error::ClassCircularity)
        } else if let Some(class) = self.classes.get(&sig) {
            // the class is already resolved
            return Ok(class.clone())
        }

        // this can't just be an else block thanks to the borrow checker
        self.pending.insert(sig.clone());
        let res = match *sig {
            sig::Class::Scalar(ref name) => {
                let class_bytes = try!(self.find_class_bytes(name)
                                       .map_err(|e| Error::ClassNotFound {
                                           name: name.clone(),
                                           error: e,
                                       }));
                // TODO: Catch errors in recursive loading and wrap them in a NoClassDefFoundError
                // as specified in §5.3
                self.load_class_bytes(name, sig, class_bytes.as_slice())
            },

            sig::Class::Array(ref component_type) => {
                // load the component type class, even though we don't use it, to ensure that any
                // errors resulting from the load happen at the right time
                try!(
                    match **component_type {
                        sig::Type::Byte | sig::Type::Char | sig::Type::Double
                            | sig::Type::Float | sig::Type::Int | sig::Type::Long
                            | sig::Type::Short | sig::Type::Boolean => Ok(None),
                        sig::Type::Reference(ref component_sig) =>
                            self.load_class(component_sig).map(|class| Some(class))
                    }
                );
                let object_name = String::from("java/lang/Object");
                let object_sig = sig::Class::Scalar(object_name);
                let object_class = try!(self.load_class(&object_sig));
                let class = vm::Class::new_array(object_class, *component_type.clone());
                let rc = Rc::new(class);
                self.classes.insert(sig.clone(), rc.clone());
                Ok(rc)
            },
        };
        self.pending.remove(&sig);
        res
    }
}
