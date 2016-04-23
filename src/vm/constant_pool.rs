use vm::Value;

/// A reference to an unresolved structure in the runtime constant pool.
pub mod symref {
    use vm::handle;

    #[derive(Debug, PartialEq, Eq, Hash)]
    pub struct Class {
        pub handle: handle::Class,
    }

    #[derive(Debug, PartialEq, Eq, Hash)]
    pub struct Method {
        pub class: Class,
        pub handle: handle::Method,
    }

    #[derive(Debug, PartialEq, Eq, Hash)]
    pub struct Field {
        pub class: Class,
        pub handle: handle::Field,
    }
}

#[derive(Debug)]
pub enum RuntimeConstantPoolEntry {
    Literal(Value),
    ClassRef(symref::Class),
    MethodRef(symref::Method),
    FieldRef(symref::Field),
}

pub type RuntimeConstantPool = Vec<Option<RuntimeConstantPoolEntry>>;
