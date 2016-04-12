use nom::{be_u8, be_u16, be_u32, ErrorKind};
use nom;

use model::class_file;
use model::class_file::AttributeInfo;
use model::class_file::FieldInfo;
use model::class_file::MethodInfo;
use model::class_file::attributes;
use model::class_file::constant_pool;
use model::class_file::constant_pool::ConstantPool;
use model::class_file::constant_pool::ConstantPoolInfo;

pub type Input<'a> = &'a [u8];
pub type ParseResult<'a, O> = Result<nom::IResult<Input<'a>, O, Error>, nom::Err<Input<'a>, Error>>;
pub type ConstantPoolIndex = class_file::constant_pool_index;

#[derive(Debug)]
pub enum Error {
    ClassFile,
    Magic,
    ConstantPool { constant_pool_count: usize },
    ConstantPoolInfo,
    UnknownConstantPoolTag { tag: u8 },
    IllegalModifiedUtf8 { byte: u8 },
    ModifiedUtf8 { length: usize },
    UnknownConstantPoolMethodReferenceTag { tag: u8 },
    Interfaces { interfaces_count: usize },
    Fields { fields_count: usize },
    FieldInfo,
    FieldAttributes { attributes_count: usize},
    Methods { methods_count: usize },
    MethodInfo,
    MethodAttributes { attributes_count: usize },
    ClassAttributes { attributes_count: usize },
    AttributeInfo,
    AttributeInfoNameIndexOutOfBounds { attribute_name_index: usize },

    CodeAttributes { attributes_count: usize },
    ExceptionTableEntry,
    StackMapTable { number_of_entries: usize },
    StackMapFrame,
}

macro_rules! p {
    ($i: expr, $($args: tt)*) => (fix_error!($i, Error, $($args)*));
}

macro_rules! p_cut {
    ($i: expr, $err: expr, $($args: tt)*) => (cut!($i, ErrorKind::Custom($err), $($args)*));
    ($i: expr, $err: expr, $f: expr) => (cut!($i, $err, call!($f)));
}

macro_rules! p_fail {
    ($e: expr) => (return Err($crate::nom::Err::Code(ErrorKind::Custom($e))));
}

/// Wraps a nom macro (returning `nom::IResult`) to produce a parser that does not
/// backtrack on error.
macro_rules! p_wrap {
    ($i: expr, $submac: ident ! ( $($args: tt)* )) => ({
        match $submac!($i, $($args)*) {
            $crate::nom::IResult::Done(i, o)    => Ok($crate::nom::IResult::Done(i, o)),
            i @ $crate::nom::IResult::Incomplete(_) => Ok(i),
            $crate::nom::IResult::Error(e) => return $crate::std::result::Result::Err(e),
        }
    });
  ($i: expr, $f: expr) => (p_wrap!($i, call!($f)));
}

macro_rules! done {
    ($i: expr, $e: expr) => ($crate::nom::IResult::Done($i, $e));
}

macro_rules! p_add_error {
    ($i: expr, $e: expr, $($args: tt)*) => (add_error!($i, ErrorKind::Custom($e), $($args)*))
}

macro_rules! p_try {
    ($i: expr, $submac: ident ! ( $($args:tt)* )) => (
        match $submac!($i, $($args)*) {
            Ok($crate::nom::IResult::Done(i, o)) => (i, o),
            Ok($crate::nom::IResult::Error(e)) => return Ok($crate::nom::IResult::Error(e)),
            Ok($crate::nom::IResult::Incomplete(i)) => return Ok($crate::nom::IResult::Incomplete(i)),
            Err(e) => return Err(e),
        });
  ($i: expr, $f: expr) => (
      p_try!($i, call!($f))
  );
}

n!(magic<Input, &[u8], Error>, p_cut!(Error::Magic, p!(tag!(&[0xCA, 0xFE, 0xBA, 0xBE]))));

n!(cp_info_tag<Input, constant_pool::Tag, Error>, chain!(
    tag: p!(be_u8),
    || constant_pool::Tag::from(tag)));

n!(cp_index<Input, u16, Error>, p!(be_u16));

macro_rules! satisfy (
    ($i: expr, $f: expr, $e: expr) => ({
      let res: $crate::nom::IResult<_, _, _> = if $i.len() == 0 {
          $crate::nom::IResult::Incomplete($crate::nom::Needed::Size(1))
      } else {
          let b = $i[0];
          if $f(b) {
              $crate::nom::IResult::Done(&$i[1..], b)
          } else {
              p_fail!($e(b))
          }
      };
      res
    }
  );
);

n!(modified_utf8<Input, u8, Error>, satisfy!(
    |b| ![0x00, 0xf0, 0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7, 0xf8, 0xf9, 0xfa, 0xfb, 0xfc, 0xfd,
          0xfe, 0xff].contains(&b),
    |b| Error::IllegalModifiedUtf8 { byte: b }));

macro_rules! take_modified_utf8 {
    ($i: expr, $n: expr) => (p_cut!($i, Error::ModifiedUtf8 { length: $n }, count!(c!(modified_utf8), $n)))
}

n!(reference_kind<Input, constant_pool::reference_kind::Tag, Error>, map!(
    p!(be_u8),
    constant_pool::reference_kind::Tag::from));

fn reference(input: Input, tag: constant_pool::reference_kind::Tag)
             -> ParseResult<constant_pool::MethodReference> {
    let r = match tag {
        constant_pool::reference_kind::Tag::GetField => map!(
            input, c!(cp_index), |ri| constant_pool::MethodReference::GetField {
                reference_index: ri
            }),
        constant_pool::reference_kind::Tag::GetStatic => map!(
            input, c!(cp_index), |ri| constant_pool::MethodReference::GetStatic {
                reference_index: ri
            }),
        constant_pool::reference_kind::Tag::PutField => map!(
            input, c!(cp_index), |ri| constant_pool::MethodReference::PutField {
                reference_index: ri
            }),
        constant_pool::reference_kind::Tag::PutStatic => map!(
            input, c!(cp_index), |ri| constant_pool::MethodReference::PutStatic {
                reference_index: ri
            }),
        constant_pool::reference_kind::Tag::InvokeVirtual => map!(
            input, c!(cp_index), |ri| constant_pool::MethodReference::InvokeVirtual {
                reference_index: ri
            }),
        constant_pool::reference_kind::Tag::InvokeStatic => map!(
            input, c!(cp_index), |ri| constant_pool::MethodReference::InvokeStatic {
                reference_index: ri
            }),
        constant_pool::reference_kind::Tag::InvokeSpecial => map!(
            input, c!(cp_index), |ri| constant_pool::MethodReference::InvokeSpecial {
                reference_index: ri
            }),
        constant_pool::reference_kind::Tag::NewInvokeSpecial => map!(
            input, c!(cp_index), |ri| constant_pool::MethodReference::NewInvokeSpecial {
                reference_index: ri
            }),
        constant_pool::reference_kind::Tag::InvokeInterface => map!(
            input, c!(cp_index), |ri| constant_pool::MethodReference::InvokeInterface {
                reference_index: ri
            }),
        constant_pool::reference_kind::Tag::Unknown(t) =>
            p_fail!(Error::UnknownConstantPoolMethodReferenceTag { tag: t }),
    };
    Ok(r)
}

fn cp_info_info(input: Input, tag: constant_pool::Tag) -> ParseResult<ConstantPoolInfo> {
    let r = match tag {
        constant_pool::Tag::Class => map!(input, c!(cp_index),
                                          |ci| ConstantPoolInfo::Class { name_index: ci }),

        constant_pool::Tag::FieldRef => chain!(input,
                                               ci: c!(cp_index) ~
                                               nti: c!(cp_index),
                                               || ConstantPoolInfo::FieldRef {
                                                   class_index: ci,
                                                   name_and_type_index: nti,
                                               }),

        constant_pool::Tag::MethodRef => chain!(input,
                                               ci: c!(cp_index) ~
                                               nti: c!(cp_index),
                                               || ConstantPoolInfo::MethodRef {
                                                   class_index: ci,
                                                   name_and_type_index: nti,
                                               }),

        constant_pool::Tag::InterfaceMethodRef => chain!(input,
                                                         ci: c!(cp_index) ~
                                                         nti: c!(cp_index),
                                                         || ConstantPoolInfo::InterfaceMethodRef {
                                                             class_index: ci,
                                                             name_and_type_index: nti,
                                                         }),

        constant_pool::Tag::String => map!(input, c!(cp_index),
                                           |si| ConstantPoolInfo::String { string_index: si }),

        constant_pool::Tag::Integer => map!(input, p!(be_u32),
                                            |bs| ConstantPoolInfo::Integer { bytes: bs }),

        constant_pool::Tag::Float => map!(input, p!(be_u32),
                                          |bs| ConstantPoolInfo::Float { bytes: bs }),
        constant_pool::Tag::Long => chain!(input,
                                           hi: p!(be_u32) ~
                                           lo: p!(be_u32),
                                           || ConstantPoolInfo::Long {
                                               high_bytes: hi,
                                               low_bytes: lo,
                                           }),

        constant_pool::Tag::Double => chain!(input,
                                             hi: p!(be_u32) ~
                                             lo: p!(be_u32),
                                             || ConstantPoolInfo::Double {
                                                 high_bytes: hi,
                                                 low_bytes: lo,
                                             }),

        constant_pool::Tag::NameAndType => chain!(input,
                                                  ni: c!(cp_index) ~
                                                  di: c!(cp_index),
                                                  || ConstantPoolInfo::NameAndType {
                                                      name_index: ni,
                                                      descriptor_index: di,
                                                  }),

        constant_pool::Tag::Utf8 => chain!(input,
                                           len: p!(be_u16) ~
                                           bs: take_modified_utf8!(len as usize),
                                           || ConstantPoolInfo::Utf8 { bytes: bs }),

        constant_pool::Tag::MethodHandle => chain!(input,
                                                   rk: c!(reference_kind) ~
                                                   r: c!(reference, rk),
                                                   || ConstantPoolInfo::MethodHandle {
                                                       reference: r
                                                   }),

        constant_pool::Tag::MethodType => map!(input, c!(cp_index),
                                               |di| ConstantPoolInfo::MethodType {
                                                   descriptor_index: di
                                               }),

        constant_pool::Tag::InvokeDynamic => chain!(input,
                                                    bmai: c!(cp_index) ~
                                                    nti: c!(cp_index),
                                                    || ConstantPoolInfo::InvokeDynamic {
                                                        bootstrap_method_attr_index: bmai,
                                                        name_and_type_index: nti,
                                                    }),

        constant_pool::Tag::Unknown(t) => p_fail!(Error::UnknownConstantPoolTag { tag: t }),
    };
    Ok(r)
}

n!(cp_info<Input, ConstantPoolInfo, Error>, p_cut!(
    Error::ConstantPoolInfo,
    chain!(tag: c!(cp_info_tag) ~
           cp_info: c!(cp_info_info, tag),
           || cp_info)));

fn exception_table<'a, 'b>(input: Input<'a>, constant_pool: &'b Vec<ConstantPoolInfo>)
                           -> ParseResult<'a, attributes::ExceptionTableEntry> {
    p_wrap!(input, p_cut!(Error::ExceptionTableEntry,
                          chain!(start_pc: p!(be_u16) ~
                                 end_pc: p!(be_u16) ~
                                 handler_pc: p!(be_u16) ~
                                 catch_type: c!(cp_index),
                                 || attributes::ExceptionTableEntry {
                                     start_pc: start_pc,
                                     end_pc: end_pc,
                                     handler_pc: handler_pc,
                                     catch_type: catch_type,
                                 })))
}

fn verification_type_info(input: Input) -> ParseResult<attributes::VerificationTypeInfo> {
    unimplemented!()
}

fn stack_map_frame_info<'a, 'b>(input: Input<'a>, tag: attributes::stack_map_frame::Tag,
                                constant_pool: &'b Vec<ConstantPoolInfo>)
                                -> ParseResult<'a, attributes::StackMapFrame> {
    use model::class_file::attributes::stack_map_frame::Tag;
    use model::class_file::attributes::StackMapFrame;
    let r = match tag {
        Tag::SameFrame(t) => done!(input, StackMapFrame::SameFrame { offset_delta: t }),
        Tag::SameLocals1StackItemFrame(t) => chain!(input,
                                                    stack_item: c!(verification_type_info),
                                                    || StackMapFrame::SameLocals1StackItemFrame {
                                                        offset_delta: t - 64,
                                                        stack_item: stack_item,
                                                    }),
        Tag::SameLocals1StackItemFrameExtended(t) =>
            chain!(input,
                   offset_delta: p!(be_u16) ~
                   stack_item: c!(verification_type_info),
                   || StackMapFrame::SameLocals1StackItemFrameExtended {
                       offset_delta: offset_delta,
                       stack_item: stack_item,
                   }),
        Tag::ChopFrame(t) => unimplemented!(),
        Tag::SameFrameExtended(t) => unimplemented!(),
        Tag::AppendFrame(t) => unimplemented!(),
        Tag::FullFrame(t) => unimplemented!(),
        Tag::Unknown(t) => unimplemented!(),
    };
    Ok(r)
}

fn stack_map_frame<'a, 'b>(input: Input<'a>, constant_pool: &'b Vec<ConstantPoolInfo>)
                           -> ParseResult<'a, attributes::StackMapFrame> {
    p_wrap!(input, p_cut!(Error::StackMapFrame,
                          chain!(tag: map!(p!(be_u8), attributes::stack_map_frame::Tag::from) ~
                                 frame: c!(stack_map_frame_info, tag, constant_pool),
                                 || frame)))
}

fn attribute_info<'a, 'b>(input: Input<'a>, constant_pool: &'b Vec<ConstantPoolInfo>)
                          -> ParseResult<'a, AttributeInfo> {
    let (input, attribute_name_index) = p_try!(input, cp_index);
    let r = match constant_pool.get(attribute_name_index as usize) {
        None => p_fail!(Error::AttributeInfoNameIndexOutOfBounds {
            attribute_name_index: attribute_name_index as usize
        }),
        Some(cp_entry) => match *cp_entry {
            ConstantPoolInfo::Utf8 { bytes: ref bs } => match bs.as_slice() {
                b"ConstantValue" => chain!(input,
                                           len: p!(be_u32) ~
                                           ci: c!(cp_index),
                                           || AttributeInfo::ConstantValue {
                                               constant_value_index: ci
                                           }),
                b"Code" => chain!(input,
                                  len: p!(be_u32) ~
                                  max_stack: p!(be_u16) ~
                                  max_locals: p!(be_u16) ~
                                  code_length: p!(be_u32) ~
                                  code: p!(map!(take!(code_length as usize), |bs: &[u8]| bs.to_vec())) ~
                                  exception_table_length: p!(be_u16) ~
                                  exception_table: count!(c!(exception_table, constant_pool),
                                                          exception_table_length as usize) ~
                                  attributes_count: p!(be_u16) ~
                                  attributes: p_cut!(
                                      Error::CodeAttributes {
                                          attributes_count: attributes_count as usize
                                      },
                                      count!(c!(attribute, constant_pool),
                                             attributes_count as usize)),
                                  || AttributeInfo::Code {
                                      max_stack: max_stack,
                                      max_locals: max_locals,
                                      code: code,
                                      exception_table: exception_table,
                                      attributes: attributes,
                                  }),

                b"StackMapTable" => chain!(input,
                                           entries_count: p!(be_u16) ~
                                           entries: p_cut!(
                                               Error::StackMapTable {
                                                   number_of_entries: entries_count as usize,
                                               },
                                               count!(c!(stack_map_frame, &constant_pool),
                                                      entries_count as usize)),
                                           || AttributeInfo::StackMapTable { entries: entries }),

                _ => unimplemented!(),
            },
            _ => unimplemented!(),
        },
    };
    Ok(r)
}

fn attribute<'a, 'b>(input: Input<'a>, constant_pool: &'b Vec<ConstantPoolInfo>)
                     -> ParseResult<'a, AttributeInfo> {
    p_wrap!(input, p_cut!(
        Error::AttributeInfo,
        chain!(attribute_name_index: c!(cp_index) ~
               attribute_length: p!(be_u32) ~
               attribute: c!(attribute_info, constant_pool),
               || attribute)))
}

fn field<'a, 'b>(input: Input<'a>, constant_pool: &'b Vec<ConstantPoolInfo>)
                 -> ParseResult<'a, FieldInfo> {
    p_wrap!(input, p_cut!(
        Error::FieldInfo,
        chain!(access_flags: p!(be_u16) ~
               name_index: c!(cp_index) ~
               descriptor_index: c!(cp_index) ~
               attributes_count: p!(be_u16) ~
               attributes: p_cut!(
                   Error::FieldAttributes { attributes_count: attributes_count as usize },
                   count!(c!(attribute, constant_pool), attributes_count as usize)),
               || FieldInfo {
                   access_flags: access_flags,
                   name_index: name_index,
                   descriptor_index: descriptor_index,
                   attributes: attributes,
               })))
}

fn method<'a, 'b>(input: Input<'a>, constant_pool: &'b Vec<ConstantPoolInfo>)
                 -> ParseResult<'a, MethodInfo> {
    p_wrap!(input, p_cut!(
        Error::MethodInfo,
        chain!(access_flags: p!(be_u16) ~
               name_index: c!(cp_index) ~
               descriptor_index: c!(cp_index) ~
               attributes_count: p!(be_u16) ~
               attributes: p_cut!(
                   Error::MethodAttributes { attributes_count: attributes_count as usize },
                   count!(c!(attribute, constant_pool), attributes_count as usize)),
               || MethodInfo {
                   access_flags: access_flags,
                   name_index: name_index,
                   descriptor_index: descriptor_index,
                   attributes: attributes,
               })))
}


n!(pub parse_class_file<Input, i32, Error>, p_cut!(
    Error::ClassFile,
    chain!(c!(magic) ~
           minor_version: p!(be_u16) ~
           major_version: p!(be_u16) ~
           constant_pool_count: p!(be_u16) ~
           constant_pool: p_cut!(
               Error::ConstantPool {
                   constant_pool_count: constant_pool_count as usize
               },
               count!(c!(cp_info), constant_pool_count as usize - 1)) ~
           access_flags: p!(be_u16) ~
           this_class: c!(cp_index) ~
           super_class: c!(cp_index) ~
           interfaces_count: p!(be_u16) ~
           interfaces: p_cut!(Error::Interfaces { interfaces_count: interfaces_count as usize },
                              count!(c!(cp_index), interfaces_count as usize)) ~
           fields_count: p!(be_u16) ~
           fields: p_cut!(Error::Fields { fields_count: fields_count as usize },
                          count!(c!(field, &constant_pool), fields_count as usize)) ~
           methods_count: p!(be_u16) ~
           methods: p_cut!(Error::Methods { methods_count: methods_count as usize },
                           count!(c!(method, &constant_pool), methods_count as usize)) ~
           attributes_count: p!(be_u16) ~
           attributes: p_cut!(
               Error::ClassAttributes { attributes_count: attributes_count as usize },
               count!(c!(attribute, &constant_pool), attributes_count as usize)),
           || { 17 })));

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_hello_world() {
        let data = include_bytes!("../../data/HelloWorld.class");
        parse_class_file(data).unwrap();
    }

}
