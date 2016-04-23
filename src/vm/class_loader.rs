use std::error;
use std::fmt;
use std::collections::HashMap;
use std::rc::Rc;

use parser::class_file;
use vm;
use vm::symref;

#[derive(Debug)]
pub enum Error {
    /// If no "purported representation" of the class is found. §5.3.1.
    ClassNotFound,
    /// The "purported representation" does not follow the class file format. §5.3.5.
    ClassFormat(class_file::Error),
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
            Error::ClassNotFound => write!(f, "ClassNotFound"),
            Error::ClassFormat(ref err) => write!(f, "ClassFormat: {}", err),
            Error::UnsupportedVersion { major, minor } =>
                write!(f, "UnsupportedVersion {}.{}", major, minor),
            Error::NoClassDefFound => write!(f, "NoClassDefFound"),
            Error::IncompatibleClassChange(class) =>
                write!(f, "IncompatibleClassChange with {}", class),
            Error::ClassCircularity => write!(f, "ClassCircularity"),
        }
    }
}

impl error::Error for Error {
    fn description(&self) -> &str {
        match *self {
            Error::ClassNotFound => "class representation not found",
            Error::ClassFormat(ref err) => format!("invalid class file: {}", err.description()),
            Error::UnsupportedVersion { major, minor } =>
                &format!("unsupported version: {}.{}", major, minor),
            Error::NoClassDefFound => "class representation is not of the requested class",
            Error::IncompatibleClassChange(class) =>
                &format!("declared superclass (superinterface) {} is actually an interface (class)",
                        class),
            Error::ClassCircularity => "the class is its own superclass or superinterface",
        }
    }

    fn cause(&self) -> Option<&error::Error> {
        match *self {
            Error::ClassNotFound => None,
            Error::ClassFormat(ref err) => Some(err),
            Error::UnsupportedVersion { .. } => None,
            Error::NoClassDefFound => None,
            Error::IncompatibleClassChange(_) => None,
            Error::ClassCircularity => None,
        }
    }
}

#[derive(Debug)]
pub struct ClassLoader {
    classes: HashMap<String, Rc<vm::Class>>,
}

impl ClassLoader {
    /// Attempts to create, load, and prepare the class with the specified binary name using the
    /// bootstrap class loader implementation. The bootstrap class loader searches the current
    /// directory for a class file with the correct fully-qualified name. If none is found, the
    /// bootstrap class loader then attempts to load the class from the standard library JAR.
    ///
    /// This implementation lazily resolves symbolic references, so no resolution of references
    /// within the loaded class is performed by this function.
    ///
    /// This implementation does not attempt to perform bytecode verification; we assume that any
    /// class files we attempt to load are valid.
    pub fn load_class(&mut self, binaryName: String) -> Result<Rc<vm::Class>, Error> {
        self.load_class_impl(binaryName, &mut vec![])
    }

    /// Implements `load_class`. There is an additional parameter containing names that are
    /// currently being resolved recursively, to ensure that we can detect the ClassCirularity
    /// error condition without overflowing the Rust stack.
    fn load_class_impl(&mut self, binaryName: String, pendingNames: &mut Vec<String>)
        -> Result<Rc<vm::Class>, Error> {
        let firstIndex = pendingNames.into_iter().position(|name| *name == binaryName);
        if let Some(_) = firstIndex {
            // we're already resolving this name
            Err(Error::ClassCircularity)
        } else if let Some(class) = self.classes.get(&binaryName) {
            // the class is already resolved
            Ok(class.clone())
        } else {
            pendingNames.push(binaryName);
            let res =
                // first, determine if the class is an array class
                if binaryName.chars().next().map(|c| c == '[').unwrap_or(false) {
                    let (_, componentDescriptor) = binaryName.split_at(1);
                    match componentDescriptor {
                        "B" | "C" | "D" | "F" | "I" | "J" | "S" | "Z" => {
                            // the components of this array are primitive; no component to resolve
                            Ok(None)
                        },
                        componentName => {
                            // recursively resolve the component
                            self.load_class_impl(String::from(componentName), pendingNames)
                                .map(|class| Some(class))
                        },
                    }.and_then(|_| {
                        self.load_class_impl(String::from("java/lang/Object"), pendingNames)
                            .map(|object_class| {
                                let class = Rc::new(vm::Class {
                                    name: symref::Class { name: binaryName.clone() },
                                    superclass: Some(object_class),
                                    methods: HashMap::new(),
                                    constant_pool: Vec::new(),
                                });
                                self.classes.insert(binaryName, class);
                                class
                            })
                    })
                };
            pendingNames.pop().unwrap();
        }
    }
}
