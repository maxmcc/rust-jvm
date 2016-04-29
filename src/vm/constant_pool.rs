use std::ops::Index;

pub use model::class_file::constant_pool::constant_pool_index;
use model::class_file::constant_pool::{ConstantPool, ConstantPoolInfo};
use vm::{self, sig, symref};
use util::one_indexed_vec::OneIndexedVec;

#[derive(Debug)]
pub enum RuntimeConstantPoolEntry {
    ClassRef(symref::Class),
    MethodRef(symref::Method),
    FieldRef(symref::Field),
    PrimitiveLiteral(vm::Value),
    StringLiteralRef(constant_pool_index),
    StringValue(ModifiedUtf8String),
}

#[derive(Debug)]
pub struct RuntimeConstantPool {
    entries: OneIndexedVec<Option<RuntimeConstantPoolEntry>>,
}

impl Index<constant_pool_index> for RuntimeConstantPool {
    type Output = Option<RuntimeConstantPoolEntry>;

    fn index(&self, index: constant_pool_index) -> &Self::Output {
        &self.entries[index as usize]
    }
}

impl RuntimeConstantPool {
    pub fn new(constant_pool: &ConstantPool) -> Self {
        let mut entries = vec![];
        for info in constant_pool {
            let entry = match *info {
                ConstantPoolInfo::Class { .. } => {
                    let class_symref = Self::force_class_ref(&constant_pool, &info);
                    Some(RuntimeConstantPoolEntry::ClassRef(class_symref))
                },

                ConstantPoolInfo::FieldRef { class_index, name_and_type_index } => {
                    let class_symref =
                        Self::force_class_ref(&constant_pool,
                                              &constant_pool[class_index as usize]);
                    let (name, descriptor) =
                        Self::force_name_and_type(&constant_pool,
                                                  &constant_pool[name_and_type_index as usize]);
                    let ty = sig::Type::new(&descriptor);
                    let sig = sig::Field { name: name, ty: ty };
                    let field_symref = symref::Field { class: class_symref, sig: sig };
                    Some(RuntimeConstantPoolEntry::FieldRef(field_symref))
                },

                ConstantPoolInfo::MethodRef { class_index, name_and_type_index } => {
                    let class_symref =
                        Self::force_class_ref(&constant_pool, &constant_pool[class_index as usize]);
                    let (name, descriptor) =
                        Self::force_name_and_type(&constant_pool,
                                                  &constant_pool[name_and_type_index as usize]);
                    let sig = sig::Method::new(&name, &descriptor);
                    let method_symref = symref::Method { class: class_symref, sig: sig };
                    Some(RuntimeConstantPoolEntry::MethodRef(method_symref))
                },

                ConstantPoolInfo::String { string_index } => {
                    Some(RuntimeConstantPoolEntry::StringLiteralRef(string_index))
                },

                ConstantPoolInfo::Integer { bytes } => {
                    let value = vm::Value::Int(bytes as i32);
                    Some(RuntimeConstantPoolEntry::PrimitiveLiteral(value))
                },

                ConstantPoolInfo::Float { bytes } => {
                    let value = vm::Value::Float(bytes as f32);
                    Some(RuntimeConstantPoolEntry::PrimitiveLiteral(value))
                },

                ConstantPoolInfo::Long { high_bytes, low_bytes } => {
                    let bits = ((high_bytes as i64) << 32) & (low_bytes as i64);
                    let value = vm::Value::Long(bits);
                    Some(RuntimeConstantPoolEntry::PrimitiveLiteral(value))
                },

                ConstantPoolInfo::Double { high_bytes, low_bytes } => {
                    let bits = ((high_bytes as u64) << 32) & (low_bytes as u64);
                    let value = vm::Value::Double(bits as f64);
                    Some(RuntimeConstantPoolEntry::PrimitiveLiteral(value))
                },

                ConstantPoolInfo::NameAndType { .. } => None,

                ConstantPoolInfo::Utf8 { ref bytes } => {
                    let modified_utf8 = ModifiedUtf8String::new(bytes.to_vec());
                    Some(RuntimeConstantPoolEntry::StringValue(modified_utf8))
                },

                ConstantPoolInfo::Unusable => None,

                _ => panic!("unsupported ConstantPoolInfo"),
            };
            entries.push(entry);
        }
        RuntimeConstantPool { entries: OneIndexedVec::from(entries) }
    }

    fn force_class_ref(constant_pool: &ConstantPool, info: &ConstantPoolInfo) -> symref::Class {
        match *info {
            ConstantPoolInfo::Class { name_index } => {
                let name = Self::force_string(&constant_pool[name_index as usize]).to_string();
                symref::Class { sig: sig::Class::new(&name) }
            },
            _ => panic!("expected ConstantPoolInfo::Class"),
        }
    }

    fn force_name_and_type(constant_pool: &ConstantPool, info: &ConstantPoolInfo)
            -> (String, String) {
        match *info {
            ConstantPoolInfo::NameAndType { name_index, descriptor_index } => {
                let ref name_info = constant_pool[name_index as usize];
                let ref descriptor_info = constant_pool[descriptor_index as usize];
                let name_string = Self::force_string(name_info).to_string();
                let descriptor_string = Self::force_string(descriptor_info).to_string();
                (name_string, descriptor_string)
            },
            _ => panic!("expected ConstantPoolInfo::NameAndType"),
        }
    }

    fn force_string(info: &ConstantPoolInfo) -> ModifiedUtf8String {
        match *info {
            ConstantPoolInfo::Utf8 { ref bytes } => {
                ModifiedUtf8String::new(bytes.to_vec())
            },
            _ => panic!("attempting to coerce non-UTF8 constant to String"),
        }
    }

    pub fn lookup_raw_string(&self, index: constant_pool_index) -> String {
        match self.entries[index as usize] {
            Some(RuntimeConstantPoolEntry::StringValue(ref modified_utf8)) =>
                modified_utf8.to_string(),
            _ => panic!("expected RuntimeConstantPoolInfo::StringValue"),
        }
    }
}

#[derive(Debug)]
pub struct ModifiedUtf8String {
    bytes: Vec<u8>,
}

impl ModifiedUtf8String {
    fn new(bytes: Vec<u8>) -> Self {
        ModifiedUtf8String { bytes: bytes }
    }

    fn to_string(&self) -> String {
        let mut utf8 = vec![];
        let mut i = 0;
        while i < self.bytes.len() {
            match self.bytes[i] {
                0x01 ... 0x7f => {
                    utf8.push(self.bytes[i]);
                    i += 1;
                },
                0xc0 ... 0xdf => {
                    if self.bytes.len() < i + 2 {
                        panic!("error decoding modified UTF-8: invalid sequence");
                    } else if self.bytes[i] == 0xc0 && self.bytes[i + 1] == 0x80 {
                        // this is the encoding of a null character
                        utf8.push(0x00);
                    } else {
                        utf8.push(self.bytes[i]);
                        utf8.push(self.bytes[i + 1]);
                    }
                    i += 2;
                },
                0xe0 ... 0xef => {
                    if self.bytes.len() < i + 3 {
                        panic!("error decoding modified UTF-8: invalid sequence");
                    } else if self.bytes[i] == 0xed && self.bytes[i + 1] >= 0xa0
                            && self.bytes[i + 1] <= 0xaf {
                        // this sequence encodes a high surrogate
                        // check that the following sequence encodes a low surrogate
                        if self.bytes.len() < i + 6 || self.bytes[i + 3] != 0xed
                                || self.bytes[i + 4] < 0xb0 || self.bytes[i + 4] > 0xbf {
                            panic!("error decoding modified UTF-8: invalid surrogate pair");
                        } else {
                            // decode the surrogate pair into a code point
                            let code_point = (((self.bytes[i + 1] & 0x0f) as u32) << 16)
                                & (((self.bytes[i + 2] & 0x3f) as u32) << 10)
                                & (((self.bytes[i + 4] & 0x0f) as u32) << 6)
                                & ((self.bytes[i + 5] & 0x3f) as u32)
                                + 0x10000;
                            // encode the code point in UTF-8
                            utf8.push(0xf0 & ((code_point & 0x001c0000 >> 18) as u8));
                            utf8.push(0x80 & ((code_point & 0x0003f000 >> 12) as u8));
                            utf8.push(0x80 & ((code_point & 0x00000fc0 >> 6) as u8));
                            utf8.push(0x80 & ((code_point & 0x0000003f) as u8));
                            // skip past the entire surrogate pair
                            i += 6;
                        }
                    } else {
                        utf8.push(self.bytes[i]);
                        utf8.push(self.bytes[i + 1]);
                        utf8.push(self.bytes[i + 2]);
                        i += 3;
                    }
                },
                0x80 ... 0xbf => panic!("error decoding modified UTF-8: invalid continuation byte"),
                _ => panic!("error decoding modified UTF-8: illegal byte"),
            }
        }
        String::from_utf8(utf8).expect("unexpected error decoding modified UTF-8")
    }

    fn to_utf16(&self) -> Vec<u16> {
        let mut utf16 = vec![];
        let mut i = 0;
        while i < self.bytes.len() {
            match self.bytes[i] {
                0x01 ... 0x7f => {
                    utf16.push(self.bytes[i] as u16);
                    i += 1;
                },
                0xc0 ... 0xdf => {
                    if self.bytes.len() < i + 2 {
                        panic!("error decoding modified UTF-8: invalid sequence");
                    } else if self.bytes[i] == 0xc0 && self.bytes[i + 1] == 0x80 {
                        // this is the encoding of a null character
                        utf16.push(0x0000);
                    } else {
                        let code_point =
                            (((self.bytes[i] & 0x1f) as u16) << 6)
                               & ((self.bytes[i + 1] & 0x3f) as u16);
                        utf16.push(code_point);
                    }
                    i += 2;
                },
                0xe0 ... 0xef => {
                    if self.bytes.len() < i + 3 {
                        panic!("error decoding modified UTF-8: invalid sequence");
                    } else {
                        let code_point =
                            (((self.bytes[i] & 0x0f) as u16) << 12)
                                & (((self.bytes[i + 1] & 0x3f) as u16) << 6)
                                & ((self.bytes[i + 2] & 0x3f) as u16);
                        utf16.push(code_point);
                        i += 3;
                    }
                },
                0x80 ... 0xbf => panic!("error decoding modified UTF-8: invalid continuation byte"),
                _ => panic!("error decoding modified UTF-8: illegal byte"),
            }
        }
        utf16
    }
}
