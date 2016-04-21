pub mod annotation;

use super::u1;
use super::u2;
use super::constant_pool_index;
use super::access_flags::inner_class_access_flags;
use super::access_flags::parameter_access_flags;

pub use self::stack_map_frame::StackMapFrame;

/// Each `ExceptionTableEntry` describes one exception handler in the `code`
/// array. The order of the handlers in an `exception_table` array is
/// significant (§2.10).
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

pub mod stack_map_frame {
    use super::super::u1;
    use super::super::u2;

    pub use self::verification_type_info::VerificationTypeInfo;

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
        FullFrame {
            offset_delta: u2,
            locals: Vec<VerificationTypeInfo>,
            stack: Vec<VerificationTypeInfo>
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

    pub mod verification_type_info {
        use super::super::super::u1;
        use super::super::super::u2;
        use super::super::super::constant_pool_index;

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
    pub inner_class_info_index: constant_pool_index,
    pub outer_class_info_index: constant_pool_index,
    pub inner_name_index: constant_pool_index,
    pub inner_class_access_flags: inner_class_access_flags::t,
}

#[derive(Debug)]
pub struct MethodParameter {
    pub name_index: constant_pool_index,
    pub access_flags: parameter_access_flags::t,
}

#[derive(Debug)]
pub struct LineNumberInfo {
    pub start_pc: u2,
    pub line_number: u2,
}

#[derive(Debug)]
pub struct LocalVariableInfo {
    pub start_pc: u2,
    pub length: u2,
    pub name_index: u2,
    pub descriptor_index: u2,
    pub index: u2,
}

#[derive(Debug)]
pub struct LocalVariableTypeInfo {
    pub start_pc: u2,
    pub length: u2,
    pub name_index: u2,
    pub signature_index: u2,
    pub index: u2,
}

/// Attributes are used in the `ClassFile`, `FieldInfo`, `MethodInfo`, and
/// `AttributeInfo::Code` structures of the class file format (§4.1, §4.5, §4.6,
/// §4.7.3).
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
        annotations: Vec<annotation::Annotation>,
    },
    RuntimeInvisibleAnnotations {
        annotations: Vec<annotation::Annotation>,
    },
    RuntimeVisibleParameterAnnotations {
        parameter_annotations: Vec<Vec<annotation::Annotation>>,
    },
    RuntimeInvisibleParameterAnnotations {
        parameter_annotations: Vec<Vec<annotation::Annotation>>,
    },
    RuntimeVisibleTypeAnnotations {
        annotations: Vec<annotation::TypeAnnotation>,
    },
    RuntimeInvisibleTypeAnnotations {
        annotations: Vec<annotation::TypeAnnotation>,
    },
    AnnotationDefault {
        default_value: annotation::ElementValue,
    },
    MethodParameters {
        parameters: Vec<MethodParameter>,
    },

    SourceFile {
        sourcefile_index: constant_pool_index,
    },
    SourceDebugExtension {
        debug_extension: Vec<u1>,
    },
    LineNumberTable {
        line_number_table: Vec<LineNumberInfo>,
    },
    LocalVariableTable {
        local_variable_table: Vec<LocalVariableInfo>,
    },
    LocalVariableTypeTable {
        local_variable_type_table: Vec<LocalVariableTypeInfo>,
    },
    Deprecated,
    Unknown {
        /// A valid index into the `constant_pool` table. The `constant_pool`
        /// entry at that index must be a valid `ConstantPoolInfo::Utf8`
        /// structure representing the name of the attribute.
        attribute_name_index: constant_pool_index,
        /// The data for this attribute.
        info: Vec<u1>,
    },
}
