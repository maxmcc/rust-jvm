//! Contains structures to describe the constant pool
//! [§4.4](https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.4).

use util::one_indexed_vec::OneIndexedVec;

use super::u1;
use super::u2;
use super::u4;

#[allow(non_camel_case_types)]
pub type constant_pool_index = u2;

/// Values of constant pool tags [Table
/// 4.4-A](https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.4-140).
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

/// Type-safe representation of constant pool tags [Table
/// 4.4-A](https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.4-140).
#[derive(Debug, PartialEq)]
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

pub mod reference_kind {
    use super::super::u1;

    pub mod tags {
        use super::super::super::u1;
        pub const GET_FIELD: u1 = 1;
        pub const GET_STATIC: u1 = 2;
        pub const PUT_FIELD: u1 = 3;
        pub const PUT_STATIC: u1 = 4;
        pub const INVOKE_VIRTUAL: u1 = 5;
        pub const INVOKE_STATIC: u1 = 6;
        pub const INVOKE_SPECIAL: u1 = 7;
        pub const NEW_INVOKE_SPECIAL: u1 = 8;
        pub const INVOKE_INTERFACE: u1 = 9;
    }

    #[derive(Debug, PartialEq)]
    pub enum Tag {
        GetField,
        GetStatic,
        PutField,
        PutStatic,
        InvokeVirtual,
        InvokeStatic,
        InvokeSpecial,
        NewInvokeSpecial,
        InvokeInterface,
        Unknown(u1),
    }

    impl From<u1> for Tag {
        fn from(tag: u1) -> Self {
            match tag {
                tags::GET_FIELD => Tag::GetField,
                tags::GET_STATIC => Tag::GetStatic,
                tags::PUT_FIELD => Tag::PutField,
                tags::PUT_STATIC => Tag::PutStatic,
                tags::INVOKE_VIRTUAL => Tag::InvokeVirtual,
                tags::INVOKE_STATIC => Tag::InvokeStatic,
                tags::INVOKE_SPECIAL => Tag::InvokeSpecial,
                tags::NEW_INVOKE_SPECIAL => Tag::NewInvokeSpecial,
                tags::INVOKE_INTERFACE => Tag::InvokeInterface,
                _ => Tag::Unknown(tag),
            }
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum MethodReference {
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

#[derive(Debug, PartialEq)]
pub enum ConstantPoolInfo {
    /// The `CONSTANT_Class_info` structure
    /// [§4.4.1](https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.4.1).
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
    MethodHandle { reference: MethodReference },
    MethodType { descriptor_index: constant_pool_index },
    InvokeDynamic {
        /// A valid index into the `bootstrap_methods` array of the bootstrap method table.
        bootstrap_method_attr_index: constant_pool_index,
        /// A valid index into the `constant_pool` table. The `constant_pool` entry at that index
        /// must be a valid `ConstantPoolInfo::Utf8` structure representing the name of the
        /// attribute.
        name_and_type_index: constant_pool_index,
    },
    /// Indicates an unusable constant pool entry.
    ///
    /// All 8-byte constants take up two entries in the constant_pool table of the class file. If a
    /// `CONSTANT_Long_info` or `CONSTANT_Double_info` structure is the item in the `constant_pool`
    /// table at index _n_, then the next usable item in the pool is located at index _n_ + 2. The
    /// constant_pool index _n_ + 1 must be valid but is considered unusable.
    Unusable,
}

impl ConstantPoolInfo {
    pub fn tag(&self) -> Tag {
        match *self {
            ConstantPoolInfo::Class { .. } => Tag::Class,
            ConstantPoolInfo::FieldRef { .. } => Tag::FieldRef,
            ConstantPoolInfo::MethodRef { .. } => Tag::MethodRef,
            ConstantPoolInfo::InterfaceMethodRef { .. } => Tag::InterfaceMethodRef,
            ConstantPoolInfo::String { .. } => Tag::String,
            ConstantPoolInfo::Integer { .. } => Tag::Integer,
            ConstantPoolInfo::Float { .. } => Tag::Float,
            ConstantPoolInfo::Long { .. } => Tag::Long,
            ConstantPoolInfo::Double { .. } => Tag::Double,
            ConstantPoolInfo::NameAndType { .. } => Tag::NameAndType,
            ConstantPoolInfo::Utf8 { .. } => Tag::Utf8,
            ConstantPoolInfo::MethodHandle { .. } => Tag::MethodHandle,
            ConstantPoolInfo::MethodType { .. } => Tag::MethodType,
            ConstantPoolInfo::InvokeDynamic { .. } => Tag::InvokeDynamic,
            ConstantPoolInfo::Unusable =>
                panic!("unusable constant pool entry does not have a valid tag"),
        }
    }
}

/// The constant pool
/// [§4.4](https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.4).
pub type ConstantPool = OneIndexedVec<ConstantPoolInfo>;

impl ConstantPool {
    pub fn from_zero_indexed_vec(vec: Vec<ConstantPoolInfo>) -> Self {
        OneIndexedVec::from(vec)
    }
}
