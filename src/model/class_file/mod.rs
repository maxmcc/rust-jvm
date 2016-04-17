//! Structures for the [Java SE 8 JVM class file
//! format](https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html).

pub mod access_flags;
pub mod attributes;
pub mod constant_pool;

pub use self::access_flags::class_access_flags;
pub use self::access_flags::field_access_flags;
pub use self::access_flags::method_access_flags;
pub use self::attributes::AttributeInfo;
pub use self::constant_pool::ConstantPoolInfo;
pub use self::constant_pool::ConstantPool;

/// Represents an unsigned one-byte quantity.
#[allow(non_camel_case_types)]
pub type u1 = u8;

/// Represents an unsigned two-byte quantity.
#[allow(non_camel_case_types)]
pub type u2 = u16;

/// Represents an unsigned four-byte quantity.
#[allow(non_camel_case_types)]
pub type u4 = u32;

/// Represents an index into the constant pool.
#[allow(non_camel_case_types)]
pub type constant_pool_index = constant_pool::constant_pool_index;

#[derive(Debug)]
pub struct FieldInfo {
    /// Mask of flags used to denote access permissions to and properties of
    /// this field.
    pub access_flags: field_access_flags::t,
    /// A valid index into the `constant_pool` table. The `constant_pool` entry
    /// at that index must be a `ConstantPoolInfo::Utf8` structure representing
    /// a valid unqualified name denoting a field.
    pub name_index: constant_pool_index,
    /// A valid index into the `constant_pool` table. The `constant_pool` entry
    /// at that index must be a `ConstantPoolInfo::Utf8` structure representing
    /// a valid unqualified name denoting a field.
    pub descriptor_index: constant_pool_index,
    /// The attributes associated with this field.
    pub attributes: Vec<AttributeInfo>,
}

#[derive(Debug)]
pub struct MethodInfo {
    /// Mask of flags used to denote access permissions to and properties of
    /// this class or interface. See the documentation for `ClassAccessFlags`
    /// for the interpretation of each flag.
    pub access_flags: method_access_flags::t,
    /// A valid index into the `constant_pool` table. The `constant_pool` entry
    /// at that index must be a `ConstantPoolInfo::Utf8` structure representing
    /// a valid unqualified name denoting a method.
    pub name_index: u2,
    /// A valid index into the `constant_pool` table. The `constant_pool` entry
    /// at that index must be a `ConstantPoolInfo::Utf8` structure representing
    /// a valid method descriptor.
    pub descriptor_index: u2,
    /// The attributes associated with this method.
    pub attributes: Vec<AttributeInfo>,
}

#[derive(Debug)]
pub struct ClassFile {
    /// Minor version number
    pub minor_version: u2,
    /// Major version number
    pub major_version: u2,
    /// Table of structures representing various string constants, class and
    /// interface names, field names, and other constants. The `constant_pool`
    /// table is indexed from 1 to `constant_pool_count - 1`.
    pub constant_pool: ConstantPool,
    /// Mask of flags used to denote access permissions to and properties of
    /// this class or interface. See the documentation for `ClassAccessFlags`
    /// for the interpretation of each flag.
    pub access_flags: class_access_flags::t,
    /// A valid index into the `constant_pool` table. The `constant_pool` entry
    /// at that index must be a `ConstantPoolInfo::Class` structure representing
    /// a valid unqualified name denoting a field.
    pub this_class: constant_pool_index,
    /// For a class, must be either zero or a valid index into the
    /// `constant_pool` table. If the value of `super_class` is non-zero, then
    /// the `constant_pool` entry at that index must be a `ConstantPoolInfo::Class`
    /// structure denoting the direct superclass of the class defined by this
    /// class file. Neither the direct superclass nor any of its superclasses
    /// may have the `ACC_FINAL` flag set in the `access_flags` item of its
    /// `ClassFile` structure.
    pub super_class: constant_pool_index,
    /// Each value in `interfaces` mut be a valid index into the `constant_pool`
    /// table. The `constant_pool` entry at each value of `interfaces[i]`, where
    /// `0 ≤ i < interfaces_count`, must be a `ConstantPoolInfo::Class` structure
    /// representing an interface that is a direct superinterface of this class
    /// or interface type, in the left-to-right order given in the source for
    /// the type.
    pub interfaces: Vec<constant_pool_index>,
    /// Contains only those fields declared by this class or interface. Does not
    /// include items representing fields that are inherited from superclasses
    /// or superinterfaces.
    pub fields: Vec<FieldInfo>,
    /// Contains only those methods declared by this class or interface. Does
    /// not include items representing methods that are inherited from
    /// superclasses or superinterfaces.
    pub methods: Vec<MethodInfo>,
    /// Contains the attributes of this class.
    pub attributes: Vec<AttributeInfo>,
}
