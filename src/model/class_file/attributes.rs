use super::u1;
use super::u2;
use super::constant_pool_index;
use super::access_flags::inner_class_access_flags;
use super::access_flags::parameter_access_flags;

pub use self::stack_map_frame::VerificationTypeInfo;

#[derive(Debug)]
pub struct ExceptionTableEntry {
    /// Indicates the (inclusive) start of the range in the `code` array at
    /// which the exception handler is active. The value of `start_pc` must be a
    /// valid index into the `code` array of the opcode of an instruction. The
    /// exception handler is active in the range `[start_pc, end_pc)`.
    pub start_pc: u2,
    /// Indicates the (exclusive) end of the range in the `code` array at which
    /// the exception handler is active. The value of `end_pc` must be a valid
    /// index into the `code` array of the opcode of an instruction or must be
    /// equal to the length of the `code` array. The exception handler is active
    /// in the range `[start_pc, end_pc)`.
    pub end_pc: u2,
    /// The value of the `handler_pc` item indicates the start of the exception
    /// handler. The value of the item must be a valid index into the code array
    /// and must be the index of the opcode of an instruction.
    pub handler_pc: u2,
    /// If the value of the `catch_type` item is nonzero, it must be a valid
    /// index into the `constant_pool` table. The `constant_pool` entry at that
    /// index must be a `ConstantPoolInfo::Class` structure representing a class
    /// of exceptions that this exception handler is designated to catch. The
    /// exception handler will be called only if the thrown exception is an
    /// instance of the given class or one of its subclasses.
    pub catch_type: constant_pool_index,
}

/// A `StackMapFrame` variant stores a relative bytecode offset, the
/// verification types (§4.10.1.2) for the local variables, and the verification
/// types for the operand stack. Each variant stores a bytecode offset _relative
/// to the previous_ `StackMapFrame`. The actual bytecode offset can be
/// calculated as described in (§4.7.4).
#[derive(Debug)]
pub enum StackMapFrame {
    SameFrame { offset_delta: u1 },
    SameLocals1StackItemFrame { offset_delta: u1, stack_item: VerificationTypeInfo },
    SameLocals1StackItemFrameExtended { offset_delta: u2, stack_item: VerificationTypeInfo },
    ChopFrame { offset_delta: u2, num_chopped: u1 },
    SameFrameExtended { offset_delta: u2 },
    AppendFrame { offset_delta: u2, locals: Vec<VerificationTypeInfo> },
    FullFrame { offset_delta: u2, locals: Vec<VerificationTypeInfo>, stack: Vec<VerificationTypeInfo> },
}

pub mod stack_map_frame {
    use super::super::u1;
    use super::super::u2;
    use super::super::constant_pool_index;

    pub mod verification_type_info {
        use super::super::super::u1;

        #[derive(Debug, PartialEq)]
        pub enum Tag {
            Top,
            Integer,
            Float,
            Long,
            Double,
            Null,
            UninitializedThis,
            Object,
            Uninitialized,
            Unknown(u1),
        }

        impl From<u1> for Tag {
            fn from(t: u1) -> Self {
                match t {
                    0 => Tag::Top,
                    1 => Tag::Integer,
                    2 => Tag::Float,
                    4 => Tag::Long,
                    3 => Tag::Double,
                    5 => Tag::Null,
                    6 => Tag::UninitializedThis,
                    7 => Tag::Object,
                    8 => Tag::Uninitialized,
                    _ => Tag::Unknown(t),
                }
            }
        }
    }

    #[derive(Debug)]
    pub enum VerificationTypeInfo {
        Top,
        Integer,
        Float,
        Long,
        Double,
        Null,
        UninitializedThis,
        Object { class_index: constant_pool_index },
        Uninitialized {
            /// The offset in the `code` array of the `Code` attribute that contains
            /// this `StackMapTable` attribute, of the _new_ instruction that
            /// created the object stored in the location.
            offset: u2,
        },
    }

    #[derive(Debug, PartialEq)]
    pub enum Tag {
        SameFrame(u1),
        SameLocals1StackItemFrame(u1),
        SameLocals1StackItemFrameExtended(u1),
        ChopFrame(u1),
        SameFrameExtended(u1),
        AppendFrame(u1),
        FullFrame(u1),
        Reserved(u1),
        Unknown(u1),
    }

    impl From<u1> for Tag {
        fn from(t: u1) -> Self {
            match t {
                0...63 => Tag::SameFrame(t),
                64...127 => Tag::SameLocals1StackItemFrame(t),
                247 => Tag::SameLocals1StackItemFrameExtended(t),
                248...250 => Tag::ChopFrame(t),
                251 => Tag::SameFrameExtended(t),
                252...254 => Tag::AppendFrame(t),
                255 => Tag::FullFrame(t),
                128...246 => Tag::Reserved(t),
                _ => Tag::Unknown(t),
            }
        }
    }
}

#[derive(Debug)]
pub struct BootstrapMethod {
    /// An index into the `constant_pool` to a `ConstantPoolInfo::MethodHandle` structure.
    bootstrap_method_ref: constant_pool_index,
    /// The indices into the `constant_pool` to `ConstantPoolInfo::String`,
    /// `ConstantPoolInfo::Class`, `ConstantPoolInfo::Integer`,
    /// `ConstantPoolInfo::Long`, `ConstantPoolInfo::Float`,
    /// `ConstantPoolInfo::Double`, `ConstantPoolInfo::MethodHandle`, or
    /// `ConstantPoolInfo::MethodType`.
    bootstrap_arguments: Vec<constant_pool_index>,
}

#[derive(Debug)]
pub struct InnerClass {
    inner_class_info_index: constant_pool_index,
    outer_class_info_index: constant_pool_index,
    inner_name_index: constant_pool_index,
    inner_class_access_flags: inner_class_access_flags::t,
}

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
    Annotation { annotation_value: Annotation },
    Array { values: Vec<ElementValue> },
}

#[derive(Debug)]
pub struct ElementValuePair {
    element_name_index: constant_pool_index,
    element_value: ElementValue,
}

#[derive(Debug)]
pub struct Annotation {
    /// An index into the `constant_pool` table for a `ConstantPoolInfo::Utf8` structure.
    type_index: constant_pool_index,
    element_value_pairs: Vec<ElementValuePair>,
}

#[derive(Debug)]
pub struct Parameter {
    name_index: constant_pool_index,
    access_flags: parameter_access_flags::t,
}

#[derive(Debug)]
pub struct LineNumberInfo {
    start_pc: u2,
    line_number: u2,
}

#[derive(Debug)]
pub struct LocalVariableInfo {
    start_pc: u2,
    length: u2,
    name_index: u2,
    descriptor_index: u2,
    index: u2,
}

#[derive(Debug)]
pub struct LocalVariableTypeInfo {
    start_pc: u2,
    length: u2,
    name_index: u2,
    signature_index: u2,
    index: u2,
}

#[derive(Debug)]
pub enum AttributeInfo {
    ConstantValue { constant_value_index: constant_pool_index },
    Code {
        max_stack: u2,
        max_locals: u2,
        code: Vec<u1>,
        exception_table: Vec<ExceptionTableEntry>,
        attributes: Vec<AttributeInfo>,
    },
    StackMapTable {
        entries: Vec<StackMapFrame>,
    },
    Exceptions {
        /// Contains indices into the `constant_pool` table for the class type
        /// that the method is declared to throw.
        exception_index_table: Vec<constant_pool_index>,
    },
    BootstrapMethods {
        bootstrap_methods: Vec<BootstrapMethod>
    },

    InnerClasses {
        classes: Vec<InnerClass>
    },
    EnclosingMethod {
        class_index: constant_pool_index,
        method_index: constant_pool_index,
    },
    Synthetic,
    Signature {
        /// A valid index into the `constant_pool` table for a `ConstantPoolInfo::Utf8` structure.
        signature_index: constant_pool_index,
    },
    RuntimeVisibleAnnotations {
        attribute_name_index: constant_pool_index,
        annotations: Vec<Annotation>,
    },
    RuntimeInvisibleAnnotations {
        attribute_name_index: constant_pool_index,
        annotations: Vec<Annotation>,
    },
    RuntimeVisibleParameterAnnotations {
        attribute_name_index: constant_pool_index,
        parameter_annotations: Vec<Vec<Annotation>>,
    },
    RuntimeInvisibleParameterAnnotations {
        attribute_name_index: constant_pool_index,
        parameter_annotations: Vec<Vec<Annotation>>,
    },
    RuntimeVisibleTypeAnnotations {
        attribute_name_index: constant_pool_index,
        annotations: Vec<Annotation>,
    },
    RuntimeInvisibleTypeAnnotations {
        attribute_name_index: constant_pool_index,
        annotations: Vec<Annotation>,
    },
    AnnotationDefault {
        attribute_name_index: constant_pool_index,
        default_value: ElementValue,
    },
    MethodParameters {
        attribute_name_index: constant_pool_index,
        parameters: Vec<Parameter>,
    },

    SourceFile {
        attribute_name_index: constant_pool_index,
        sourcefile_index: constant_pool_index,
    },
    SourceDebugExtension {
        attribute_name_index: constant_pool_index,
        debug_extension: Vec<u1>,
    },
    LineNumberTable {
        attribute_name_index: constant_pool_index,
        line_number_table: Vec<LineNumberInfo>,
    },
    LocalVariableTable {
        attribute_name_index: constant_pool_index,
        local_variable_table: Vec<LocalVariableInfo>,
    },
    LocalVariableTypeTable {
        attribute_name_index: constant_pool_index,
        local_variable_type_table: Vec<LocalVariableTypeInfo>,
    },
    Deprecated {
        attribute_name_index: constant_pool_index,
    },

    Unknown {
        /// A valid index into the `constant_pool` table. The `constant_pool`
        /// entry at that index must be a valid `ConstantPoolInfo::Utf8`
        /// structure representing the name of the attribute.
        attribute_name_index: constant_pool_index,
        /// The data for this attribute.
        info: Vec<u1>,
    },
}
