//! Workaround to get sane nested errors from `nom`. Follows [#160](https://github.com/Geal/nom/issues/160)

/// Declares a parser that can be controlled for backtracking by using `c!` and `cut!`. The return
/// type is `std::result::Result<nom::IResult, nom::Err<I, E>>`. If the parser returns
/// `std::result::Result::Ok`, then backtracking occurs. If the parser returns
/// `std::result::Result::Err`, then backtracking does not occur.
macro_rules! n (
    ($name:ident( $i:ty ) -> $o:ty, $submac:ident!( $($args:tt)* )) => (
        fn $name( i: $i ) -> $crate::std::result::Result<$crate::nom::IResult<$i,$o,u32>, $crate::nom::Err<$i, u32>> {
            $crate::std::result::Result::Ok($submac!(i, $($args)*))
        }
    );
    ($name:ident<$i:ty,$o:ty,$e:ty>, $submac:ident!( $($args:tt)* )) => (
        fn $name( i: $i ) -> $crate::std::result::Result<$crate::nom::IResult<$i, $o, $e>, $crate::nom::Err<$i, $e>> {
            $crate::std::result::Result::Ok($submac!(i, $($args)*))
        }
    );
    ($name:ident<$i:ty,$o:ty>, $submac:ident!( $($args:tt)* )) => (
        fn $name( i: $i ) -> $crate::std::result::Result<$crate::nom::IResult<$i, $o, u32>, $crate::nom::Err<$i, u32>> {
            $crate::std::result::Result::Ok($submac!(i, $($args)*))
        }
    );
    ($name:ident<$o:ty>, $submac:ident!( $($args:tt)* )) => (
        fn $name<'a>( i: &'a[u8] ) -> $crate::std::result::Result<$crate::nom::IResult<&'a [u8], $o, u32>, $crate::nom::Err<&'a [u8], u32>> {
            $crate::std::result::Result::Ok($submac!(i, $($args)*))
        }
    );
    ($name:ident, $submac:ident!( $($args:tt)* )) => (
        fn $name( i: &[u8] ) -> $crate::std::result::Result<$crate::nom::IResult<&[u8], &[u8], u32>, $crate::nom::Err<&[u8], u32>> {
            $crate::std::result::Result::Ok($submac!(i, $($args)*))
        }
    );
    (pub $name:ident( $i:ty ) -> $o:ty, $submac:ident!( $($args:tt)* )) => (
        pub fn $name( i: $i ) -> $crate::std::result::Result<$crate::nom::IResult<$i,$o, u32>, $crate::nom::Err<$i, u32>> {
            $crate::std::result::Result::Ok($submac!(i, $($args)*))
        }
    );
    (pub $name:ident<$i:ty,$o:ty,$e:ty>, $submac:ident!( $($args:tt)* )) => (
        pub fn $name( i: $i ) -> $crate::std::result::Result<$crate::nom::IResult<$i, $o, $e>, $crate::nom::Err<$i, $e>> {
            $crate::std::result::Result::Ok($submac!(i, $($args)*))
        }
    );
    (pub $name:ident<$i:ty,$o:ty>, $submac:ident!( $($args:tt)* )) => (
        pub fn $name( i: $i ) -> $crate::std::result::Result<$crate::nom::IResult<$i, $o, u32>, $crate::nom::Err<$i, u32>> {
            $crate::std::result::Result::Ok($submac!(i, $($args)*))
        }
    );
    (pub $name:ident<$o:ty>, $submac:ident!( $($args:tt)* )) => (
        pub fn $name( i: &[u8] ) -> $crate::std::result::Result<$crate::nom::IResult<&[u8], $o, u32>, $crate::nom::Err<&[u8], u32>> {
            $crate::std::result::Result::Ok($submac!(i, $($args)*))
        }
    );
    (pub $name:ident, $submac:ident!( $($args:tt)* )) => (
        pub fn $name<'a>( i: &'a [u8] ) -> $crate::std::result::Result<$crate::nom::IResult<&[u8], &[u8], u32>, $crate::nom::Err<&[u8], u32>> {
            $crate::std::result::Result::Ok($submac!(i, $($args)*))
        }
    );
    ($name:ident<$i:ty,$o:ty,$e:ty>, $submac:ident!( $($args:tt)* )) => (
        fn $name( i: $i ) -> $crate::std::result::Result<$crate::nom::IResult<$i, $o, $e>, $crate::nom::Err<$i, $e>> {
            $crate::std::result::Result::Ok($submac!(i, $($args)*))
        }
    );
);

macro_rules! p_named (
    ($name:ident( $i:ty ) -> $o:ty, $submac:ident!( $($args:tt)* )) => (
        fn $name( i: $i ) -> $crate::std::result::Result<$crate::nom::IResult<$i,$o,u32>, $crate::nom::Err<$i, u32>> {
            $submac!(i, $($args)*)
        }
    );
    ($name:ident<$i:ty,$o:ty,$e:ty>, $submac:ident!( $($args:tt)* )) => (
        fn $name( i: $i ) -> $crate::std::result::Result<$crate::nom::IResult<$i, $o, $e>, $crate::nom::Err<$i, $e>> {
            $submac!(i, $($args)*)
        }
    );
    ($name:ident<$i:ty,$o:ty>, $submac:ident!( $($args:tt)* )) => (
        fn $name( i: $i ) -> $crate::std::result::Result<$crate::nom::IResult<$i, $o, u32>, $crate::nom::Err<$i, u32>> {
            $submac!(i, $($args)*)
        }
    );
    ($name:ident<$o:ty>, $submac:ident!( $($args:tt)* )) => (
        fn $name<'a>( i: &'a[u8] ) -> $crate::std::result::Result<$crate::nom::IResult<&'a [u8], $o, u32>, $crate::nom::Err<&'a [u8], u32>> {
            $submac!(i, $($args)*)
        }
    );
    ($name:ident, $submac:ident!( $($args:tt)* )) => (
        fn $name( i: &[u8] ) -> $crate::std::result::Result<$crate::nom::IResult<&[u8], &[u8], u32>, $crate::nom::Err<&[u8], u32>> {
            $submac!(i, $($args)*)
        }
    );
    (pub $name:ident( $i:ty ) -> $o:ty, $submac:ident!( $($args:tt)* )) => (
        pub fn $name( i: $i ) -> $crate::std::result::Result<$crate::nom::IResult<$i,$o, u32>, $crate::nom::Err<$i, u32>> {
            $submac!(i, $($args)*)
        }
    );
    (pub $name:ident<$i:ty,$o:ty,$e:ty>, $submac:ident!( $($args:tt)* )) => (
        pub fn $name( i: $i ) -> $crate::std::result::Result<$crate::nom::IResult<$i, $o, $e>, $crate::nom::Err<$i, $e>> {
            $submac!(i, $($args)*)
        }
    );
    (pub $name:ident<$i:ty,$o:ty>, $submac:ident!( $($args:tt)* )) => (
        pub fn $name( i: $i ) -> $crate::std::result::Result<$crate::nom::IResult<$i, $o, u32>, $crate::nom::Err<$i, u32>> {
            $submac!(i, $($args)*)
        }
    );
    (pub $name:ident<$o:ty>, $submac:ident!( $($args:tt)* )) => (
        pub fn $name( i: &[u8] ) -> $crate::std::result::Result<$crate::nom::IResult<&[u8], $o, u32>, $crate::nom::Err<&[u8], u32>> {
            $submac!(i, $($args)*)
        }
    );
    (pub $name:ident, $submac:ident!( $($args:tt)* )) => (
        pub fn $name<'a>( i: &'a [u8] ) -> $crate::std::result::Result<$crate::nom::IResult<&[u8], &[u8], u32>, $crate::nom::Err<&[u8], u32>> {
            $submac!(i, $($args)*)
        }
    );

);

macro_rules! as_fn_params {
    ($arg: ident : $t: ty) => ($arg : $t);
}

/// Prevents backtracking out of the specified parser.
macro_rules! cut {
  ($i:expr, $code:expr, $submac:ident!( $($args:tt)* )) => ({
      let cl = || {
        Ok($submac!($i, $($args)*))
      };

      match cl() {
        $crate::std::result::Result::Ok($crate::nom::IResult::Incomplete(x)) => $crate::nom::IResult::Incomplete(x),
        $crate::std::result::Result::Ok($crate::nom::IResult::Done(i, o))    => $crate::nom::IResult::Done(i, o),
        $crate::std::result::Result::Ok($crate::nom::IResult::Error(e)) | $crate::std::result::Result::Err(e)  => {
          return $crate::std::result::Result::Err($crate::nom::Err::NodePosition($code, $i, Box::new(e)))
        }
      }
  });
  ($i:expr, $code:expr, $f:expr) => (cut!($i, $code, call!($f)));
}

/// Wraps a `nom::IResult` in variant `std::result::Result::Ok`.
macro_rules! c {
    ($i:expr, $f:expr) => ({
        match $f($i) {
            $crate::std::result::Result::Ok($crate::nom::IResult::Incomplete(x)) => $crate::nom::IResult::Incomplete(x),
            $crate::std::result::Result::Ok($crate::nom::IResult::Done(i, o)) => $crate::nom::IResult::Done(i, o),
            $crate::std::result::Result::Ok($crate::nom::IResult::Error(e)) => $crate::nom::IResult::Error(e),
            $crate::std::result::Result::Err(e) => return $crate::std::result::Result::Err(e),
        }
    });
    ($i:expr, $f:expr, $($arg: expr),* ) => ({
        match $f($i, $($arg),*) {
            $crate::std::result::Result::Ok($crate::nom::IResult::Incomplete(x)) => $crate::nom::IResult::Incomplete(x),
            $crate::std::result::Result::Ok($crate::nom::IResult::Done(i, o)) => $crate::nom::IResult::Done(i, o),
            $crate::std::result::Result::Ok($crate::nom::IResult::Error(e)) => $crate::nom::IResult::Error(e),
            $crate::std::result::Result::Err(e) => return $crate::std::result::Result::Err(e),
        }
    });
}

#[cfg(test)]
mod test {
    use nom::IResult;
    use nom::Err;
    use nom::ErrorKind;

    n!(pub foo< bool >,
       chain!(
           tag!("a") ~
               cut!($crate::nom::ErrorKind::Custom(42),tag!("b")) ,
           || { true }
       )
    );

    n!(pub foos< Vec<bool> >,
       delimited!(
           tag!("("),
           many0!(c!(foo)),
           tag!(")")
       )
    );

   #[test]
    fn test_ok() {
        let r = foos(b"(abab)");
        println!("result: {:?}", r);
        match r {
            Ok(IResult::Done(_,result)) => assert_eq!(result, vec![true,true]),
            res => panic!("Oops {:?}.",res)
        }
    }

    #[test]
    fn test_err() {
        let r = foos(b"(ac)");
        println!("result: {:?}", r);
        match r {
            Err(Err::NodePosition(kind, _, _)) => assert_eq!(kind, ErrorKind::Custom(42)),
            res => panic!("Oops, {:?}",res)
        }
    }
}
