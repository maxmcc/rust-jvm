use model::class_file::{constant_pool_index, u1, u2};

pub use self::element_value::ElementValue;
pub use self::target_type::TargetInfo;

pub mod element_value {
    use model::class_file::{constant_pool_index, u1};

    #[derive(Debug)]
    pub enum ElementValue {
        Byte { const_value_index: constant_pool_index },
        Char { const_value_index: constant_pool_index },
        Double { const_value_index: constant_pool_index },
        Float { const_value_index: constant_pool_index },
        Int { const_value_index: constant_pool_index },
        Long { const_value_index: constant_pool_index },
        Short { const_value_index: constant_pool_index },
        Boolean { const_value_index: constant_pool_index },
        String { const_value_index: constant_pool_index },
        Enum { type_name_index: constant_pool_index, const_name_index: constant_pool_index },
        Class { class_info_index: constant_pool_index },
        Annotation { annotation_value: super::Annotation },
        Array { values: Vec<super::ElementValue> },
    }

    #[derive(Debug, PartialEq)]
    pub enum Tag {
        Byte,
        Char,
        Double,
        Float,
        Int,
        Long,
        Short,
        Boolean,
        String,
        Enum,
        Class,
        Annotation,
        Array,
        Unknown(u1),
    }
    impl From<u1> for Tag {
        fn from(tag: u1) -> Self {
            match tag {
                b'B' => Tag::Byte,
                b'C' => Tag::Char,
                b'D' => Tag::Double,
                b'F' => Tag::Float,
                b'I' => Tag::Int,
                b'J' => Tag::Long,
                b'S' => Tag::Short,
                b'Z' => Tag::Boolean,
                b's' => Tag::String,
                b'e' => Tag::Enum,
                b'c' => Tag::Class,
                b'@' => Tag::Annotation,
                b'[' => Tag::Array,
                tag => Tag::Unknown(tag),
            }
        }
    }
}

#[derive(Debug)]
pub struct ElementValuePair {
    pub element_name_index: constant_pool_index,
    pub value: ElementValue,
}

#[derive(Debug)]
pub struct LocalVariableTargetInfo {
    pub start_pc: u2,
    pub length: u2,
    pub index: u2,
}

pub mod target_type {
    use model::class_file::{u1, u2};

    #[derive(Debug)]
    pub enum TargetInfo {
        TypeParameter { type_parameter_index: u1 },
        Supertype { supertype_index: u2 },
        TypeParameterBound { type_parameter_index: u1, bound_index: u1 },
        Empty,
        FormalParameter { formal_parameter_index: u1 },
        Throws { throws_type_index: u2 },
        LocalVariable { table: Vec<super::LocalVariableTargetInfo> },
        Catch { exception_table_index: u2 },
        Offset { offset: u2 },
        TypeArgument { offset: u2, type_argument_index: u1 },
    }

    pub enum Tag {
        TypeParameter,
        Supertype,
        TypeParameterBound,
        Empty,
        FormalParameter,
        Throws,
        LocalVariable,
        Catch,
        Offset,
        TypeArgument,
        Unknown(u1),
    }

    impl From<u1> for Tag {
        fn from(tag: u1) -> Self {
            match tag {
                0x00 | 0x01 => Tag::TypeParameter,
                0x10 => Tag::Supertype,
                0x11 | 0x12 => Tag::TypeParameterBound,
                0x13 | 0x14 | 0x15 => Tag::Empty,
                0x16 => Tag::FormalParameter,
                0x17 => Tag::Throws,

                0x40 | 0x41 => Tag::LocalVariable,
                0x42 => Tag::Catch,
                0x43 | 0x44 | 0x45 | 0x46 => Tag::Offset,
                0x47 | 0x48 | 0x49 | 0x4A | 0x4B => Tag::TypeArgument,

                _ => Tag::Unknown(tag),
            }
        }
    }
}

#[derive(Debug)]
pub struct Annotation {
    /// An index into the `constant_pool` table for a `ConstantPoolInfo::Utf8` structure.
    pub type_index: constant_pool_index,
    pub element_value_pairs: Vec<ElementValuePair>,
}

#[derive(Debug)]
pub struct TypePathPart {
    pub type_path_kind: u1,
    pub type_argument_index: u1,
}

#[derive(Debug)]
pub struct TypePath {
    pub path: Vec<TypePathPart>,
}

#[derive(Debug)]
pub struct TypeAnnotation {
    pub target_info: TargetInfo,
    pub target_path: TypePath,
    pub type_index: u2,
    pub element_value_pairs: Vec<ElementValuePair>,
}
