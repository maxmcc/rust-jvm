use std::io::Read;

use nom::{be_u8, be_u16, be_u32, ErrorKind};
use nom;

use model::class_file::ClassFile;
use model::class_file::constant_pool;
use model::class_file::constant_pool::ConstantPoolInfo;

pub type ParseResult<I, O> = Result<nom::IResult<I, O, Error>, nom::Err<I, Error>>;

#[derive(Debug)]
pub enum Error {
    ClassFile,
    Magic,
    ConstantPool { constant_pool_count: u16 },
    ConstantPoolInfo,
    UnknownConstantPoolTag { tag: u8 },
    IllegalModifiedUtf8 { byte: u8 },
    ModifiedUtf8,
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

macro_rules! p_wrap {
  ($i: expr, $submac: ident ! ( $($args: tt)* )) => (Ok($submac!($i, $($args)*)));
  ($i: expr, $f: expr) => (Ok(call!($i, $f)));
}

macro_rules! p_add_error {
    ($i: expr, $e: expr, $($args: tt)*) => (add_error!($i, ErrorKind::Custom($e), $($args)*))
}

n!(magic<&[u8], &[u8], Error>, p_cut!(Error::Magic, p!(tag!(&[0xCA, 0xFE, 0xBA, 0xBE]))));

n!(cp_info_tag<&[u8], constant_pool::Tag, Error>, chain!(
    tag: p!(be_u8),
    || constant_pool::Tag::from(tag)));

n!(cp_index<&[u8], u16, Error>, p!(be_u16));

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

n!(modified_utf8<&[u8], u8, Error>, satisfy!(
    |b| [0x00, 0xf0, 0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7, 0xf8, 0xf9, 0xfa, 0xfb, 0xfc, 0xfd,
         0xfe, 0xff].contains(&b),
    |b| Error::IllegalModifiedUtf8 { byte: b }));

macro_rules! take_modified_utf8 {
    ($i: expr, $n: expr) => (p_cut!($i, Error::ModifiedUtf8, count!(c!(modified_utf8), $n)))
}

static mut cp_info_count: usize = 0;
fn cp_info_info(input: &[u8], tag: constant_pool::Tag) -> ParseResult<&[u8], ConstantPoolInfo> {
    unsafe {
        cp_info_count += 1;
        println!("processing cp info #{}", cp_info_count);
    }
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

        constant_pool::Tag::MethodHandle => unimplemented!(),

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

n!(cp_info<&[u8], constant_pool::ConstantPoolInfo, Error>, p_cut!(
    Error::ConstantPoolInfo,
    chain!(
        tag: c!(cp_info_tag) ~
        cp_info: c!(cp_info_info, tag),
        || cp_info)));


n!(pub parse_class_file<&[u8], i32, Error>, p_cut!(
    Error::ClassFile,
    chain!(c!(magic) ~
           minor_version: p!(be_u16) ~
           major_version: p!(be_u16) ~
           constant_pool_count: p!(be_u16) ~
           constant_pool: p_cut!(Error::ConstantPool { constant_pool_count: constant_pool_count },
                                 count!(c!(cp_info), constant_pool_count as usize - 1)),
           || { 17 })));

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test() {
        let data = include_bytes!("../../data/HelloWorld.class");
        let r = parse_class_file(data);
        panic!("{:?}", r);
    }

}
