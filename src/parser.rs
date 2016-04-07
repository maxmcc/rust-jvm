use std::io::Read;

use nom::{be_u8, be_u16, be_u32, ErrorKind};
use nom;

use model::class_file::ClassFile;
use model::class_file::constant_pool;
use model::class_file::constant_pool::ConstantPoolInfo;

pub type ParseResult<I, O> = nom::IResult<I, O, Error>;

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

macro_rules! p_fail {
    ($e: expr) => ({
        return $crate::nom::IResult::Error($crate::nom::Err::Code(ErrorKind::Custom($e)))
    })
}

macro_rules! my_error {
    ($i: expr, $e: expr, $($args: tt)*) => (error!($i, ErrorKind::Custom($e), $($args)*))
}

named!(magic<&[u8], &[u8], Error>, my_error!(Error::Magic, fix_error!(Error, tag!(&[0xCA, 0xFE, 0xBA, 0xBE]))));

named!(cp_info_tag<&[u8], constant_pool::Tag, Error>, chain!(
    tag: p!(be_u8),
    || constant_pool::Tag::from(tag)));

named!(cp_index<&[u8], u16, Error>, p!(be_u16));

macro_rules! satisfy (
    ($i: expr, $f: expr, $e: expr) => ({
        use nom::HexDisplay;
        println!("{}", $i.to_hex(8));
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

named!(modified_utf8<&[u8], u8, Error>, satisfy!(
    |b| [0x00, 0xf0, 0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7, 0xf8, 0xf9, 0xfa, 0xfb, 0xfc, 0xfd,
         0xfe, 0xff].contains(&b),
    |b| Error::IllegalModifiedUtf8 { byte: b }));

macro_rules! take_modified_utf8 {
    ($i: expr, $n: expr) => (my_error!($i, Error::ModifiedUtf8, count!(modified_utf8, $n)))
}

static mut cp_info_count: usize = 0;
fn cp_info_info(input: &[u8], tag: constant_pool::Tag) -> ParseResult<&[u8], ConstantPoolInfo> {
    unsafe {
        cp_info_count += 1;
        println!("processing cp info #{}", cp_info_count);
    }
    match tag {
        constant_pool::Tag::Class => map!(input, cp_index,
                                          |ci| ConstantPoolInfo::Class { name_index: ci }),

        constant_pool::Tag::FieldRef => chain!(input,
                                               ci: cp_index ~
                                               nti: cp_index,
                                               || ConstantPoolInfo::FieldRef {
                                                   class_index: ci,
                                                   name_and_type_index: nti,
                                               }),

        constant_pool::Tag::MethodRef => chain!(input,
                                               ci: cp_index ~
                                               nti: cp_index,
                                               || ConstantPoolInfo::MethodRef {
                                                   class_index: ci,
                                                   name_and_type_index: nti,
                                               }),

        constant_pool::Tag::InterfaceMethodRef => chain!(input,
                                                         ci: cp_index ~
                                                         nti: cp_index,
                                                         || ConstantPoolInfo::InterfaceMethodRef {
                                                             class_index: ci,
                                                             name_and_type_index: nti,
                                                         }),

        constant_pool::Tag::String => map!(input, cp_index,
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
                                                  ni: cp_index ~
                                                  di: cp_index,
                                                  || ConstantPoolInfo::NameAndType {
                                                      name_index: ni,
                                                      descriptor_index: di,
                                                  }),

        constant_pool::Tag::Utf8 => chain!(input,
                                           len: p!(be_u16) ~
                                           bs: take_modified_utf8!(len as usize),
                                           || ConstantPoolInfo::Utf8 { bytes: bs }),

        constant_pool::Tag::MethodHandle => unimplemented!(),

        constant_pool::Tag::MethodType => map!(input, cp_index,
                                               |di| ConstantPoolInfo::MethodType {
                                                   descriptor_index: di
                                               }),

        constant_pool::Tag::InvokeDynamic => chain!(input,
                                                    bmai: cp_index ~
                                                    nti: cp_index,
                                                    || ConstantPoolInfo::InvokeDynamic {
                                                        bootstrap_method_attr_index: bmai,
                                                        name_and_type_index: nti,
                                                    }),

        constant_pool::Tag::Unknown(t) => p_fail!(Error::UnknownConstantPoolTag { tag: t }),
    }
}

named!(cp_info<&[u8], constant_pool::ConstantPoolInfo, Error>, my_error!(Error::ConstantPoolInfo, chain!(
    tag: cp_info_tag ~
    cp_info: apply!(cp_info_info, tag),
    || {println!("{:?}", cp_info); cp_info})));


named!(pub parse_class_file<&[u8], i32, Error>,
       chain!(magic ~
              minor_version: p!(be_u16) ~
              major_version: p!(be_u16) ~
              constant_pool_count: p!(be_u16) ~
              constant_pool: my_error!(Error::ConstantPool { constant_pool_count: constant_pool_count },
                                       dbg_dmp!(count!(cp_info, constant_pool_count as usize - 1))),
              || { 17 }));

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
