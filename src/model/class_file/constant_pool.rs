use super::u1;
use super::u2;
use super::u4;

#[allow(non_camel_case_types)]
pub type constant_pool_index = u2;

pub mod tags {
    use super::super::u1;
    pub const CLASS: u1 = 7;
    pub const FIELD_REF: u1 = 9;
    pub const METHOD_REF: u1 = 10;
    pub const INTERFACE_METHOD_REF: u1 = 11;
    pub const STRING: u1 = 8;
    pub const INTEGER: u1 = 3;
    pub const FLOAT: u1 = 4;
    pub const LONG: u1 = 5;
    pub const DOUBLE: u1 = 6;
    pub const NAME_AND_TYPE: u1 = 12;
    pub const UTF_8: u1 = 1;
    pub const METHOD_HANDLE: u1 = 15;
    pub const METHOD_TYPE: u1 = 16;
    pub const INVOKE_DYNAMIC: u1 = 18;
}

pub enum Tag {
    Class,
    FieldRef,
    MethodRef,
    InterfaceMethodRef,
    String,
    Integer,
    Float,
    Long,
    Double,
    NameAndType,
    Utf8,
    MethodHandle,
    MethodType,
    InvokeDynamic,
    Unknown(u1),
}

impl From<u1> for Tag {
    fn from(tag: u1) -> Self {
        match tag {
            tags::CLASS => Tag::Class,
            tags::FIELD_REF => Tag::FieldRef,
            tags::METHOD_REF => Tag::MethodRef,
            tags::INTERFACE_METHOD_REF => Tag::InterfaceMethodRef,
            tags::STRING => Tag::String,
            tags::INTEGER => Tag::Integer,
            tags::FLOAT => Tag::Float,
            tags::LONG => Tag::Long,
            tags::DOUBLE => Tag::Double,
            tags::NAME_AND_TYPE => Tag::NameAndType,
            tags::UTF_8 => Tag::Utf8,
            tags::METHOD_HANDLE => Tag::MethodHandle,
            tags::METHOD_TYPE => Tag::MethodType,
            tags::INVOKE_DYNAMIC => Tag::InvokeDynamic,
            _ => Tag::Unknown(tag),
        }
    }
}

#[derive(Debug)]
pub enum ReferenceKind {
    GetField { reference_index: constant_pool_index },
    GetStatic { reference_index: constant_pool_index },
    PutField { reference_index: constant_pool_index },
    PutStatic { reference_index: constant_pool_index },
    InvokeVirtual { reference_index: constant_pool_index },
    InvokeStatic { reference_index: constant_pool_index },
    InvokeSpecial { reference_index: constant_pool_index },
    NewInvokeSpecial { reference_index: constant_pool_index },
    InvokeInterface { reference_index: constant_pool_index },
}

#[derive(Debug)]
pub enum ConstantPoolInfo {
    Class { name_index: constant_pool_index },
    FieldRef { class_index: constant_pool_index, name_and_type_index: constant_pool_index },
    MethodRef { class_index: constant_pool_index, name_and_type_index: constant_pool_index },
    InterfaceMethodRef {
        class_index: constant_pool_index,
        name_and_type_index: constant_pool_index
    },
    String { string_index: u2 },
    Integer { bytes: u4 },
    Float { bytes: u4 },
    Long { high_bytes: u4, low_bytes: u4 },
    Double { high_bytes: u4, low_bytes: u4 },
    NameAndType {
        name_index: constant_pool_index,
        descriptor_index: constant_pool_index,
    },
    Utf8 { bytes: Vec<u1> },
    MethodHandle { reference_kind: ReferenceKind, reference_index: constant_pool_index },
    MethodType { descriptor_index: constant_pool_index },
    InvokeDynamic {
        /// A valid index into the `bootstrap_methods` array of the bootstrap
        /// method table.
        bootstrap_method_attr_index: constant_pool_index,
        /// A valid index into the `constant_pool` table. The `constant_pool`
        /// entry at that index must be a valid `ConstantPoolInfo::Utf8` structure
        /// representing the name of the attribute.
        name_and_type_index: constant_pool_index,
    },
}
