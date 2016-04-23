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
    Array(Box<Type>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Class {
    name: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Method {
    name: String,
    parameters: Vec<Type>,
    return_type: Option<Type>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Field {
    name: String,
    ty: Type,
}
