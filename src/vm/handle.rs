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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Class {
    Scalar(Vec<String>),
    Array(Box<Type>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Method {
    name: String,
    parameters: Vec<Type>,
    return_type: Option<Type>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Field {
    pub name: String,
    pub ty: Type,
}
