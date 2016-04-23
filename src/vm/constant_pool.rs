use vm::Value;

/// A reference to an unresolved structure in the runtime constant pool.
pub mod symref {

    #[derive(Debug, PartialEq, Eq, Hash)]
    pub struct Class {
        name: String,
    }

    #[derive(Debug, PartialEq, Eq, Hash)]
    pub struct Method {
        name: String,
        class: Class,
    }

    #[derive(Debug, PartialEq, Eq, Hash)]
    pub struct Field {
        name: String,
        class: Class,
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
