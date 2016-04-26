//! Workaround to get sane nested errors from `nom`. The nom library does not provide parsers that
//! can have controllable backtracking behavior, so we need to hack it in. Follows
//! [#160](https://github.com/Geal/nom/issues/160).
//!
//! This module contains support for converting to and from nom parsers and
//! backtracking-controllable parsers. A _parser that can be controlled for backtracking_ has type
//! `std::result::Result<nom::IResult, nom::Err<I, E>>`, while a _nom parser_ has type
//! `nom::IResult`.

/// `done!(O) => nom::IResult::Done<I, O>` wraps the specified expression in `nom::IResult::Done`.
#[macro_export]
macro_rules! done {
    ($i: expr, $e: expr) => ($crate::nom::IResult::Done($i, $e));
}

/// Convenience for creating a custom nom error `nom::Err::Code(nom::ErrorKind::Custom($err))`.
#[macro_export]
macro_rules! custom_error {
    ($e: expr) => ($crate::nom::Err::Code($crate::nom::ErrorKind::Custom($e)));
}

/// Adds a custom error if the child nom parser fails.
#[macro_export]
macro_rules! p_add_error {
    ($i: expr, $e: expr, $($args: tt)*) => (add_error!($i, ErrorKind::Custom($e), $($args)*))
}

/// Declares a parser (with a body of type `nom::IResult`) that can be controlled for backtracking
/// by using `c!` and `cut!`. The return type is `std::result::Result<nom::IResult, nom::Err<I,
/// E>>`. If the parser returns `std::result::Result::Ok`, then backtracking occurs. If the parser
/// returns `std::result::Result::Err`, then backtracking does not occur.
#[macro_export]
macro_rules! n {
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
}

/// Declares a parser (with a body of type `std::result::Result<nom::IResult, nom::Err<I, E>>`)
/// that can be controlled for backtracking by using `c!` and `cut!`. The return type is
/// `std::result::Result<nom::IResult, nom::Err<I, E>>`. If the parser returns
/// `std::result::Result::Ok`, then backtracking occurs. If the parser returns
/// `std::result::Result::Err`, then backtracking does not occur.
#[macro_export]
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

/// `cut!(nom::ErrorKind<E>, I -> nom::IResult<I, O, E>) => Err<_, nom::Err<I, E>> OR nom::IResult::Done<I, O, E> OR IResult::Incomplete<_>`
/// Prevents backtracking out of the specified nom parser.
#[macro_export]
macro_rules! cut {
    ($i: expr, $code: expr, $submac:ident !( $($args:tt)* )) => ({
        let cl = || {
            Ok($submac!($i, $($args)*))
        };

        match cl() {
            $crate::std::result::Result::Ok($crate::nom::IResult::Incomplete(x)) =>
                $crate::nom::IResult::Incomplete(x),
            $crate::std::result::Result::Ok($crate::nom::IResult::Done(i, o)) =>
                $crate::nom::IResult::Done(i, o),
            $crate::std::result::Result::Ok(
                $crate::nom::IResult::Error(e)) | $crate::std::result::Result::Err(e) =>
                return $crate::std::result::Result::Err($crate::nom::Err::NodePosition($code, $i, Box::new(e))),
        }
    });
    ($i:expr, $code:expr, $f:expr) => (cut!($i, $code, call!($f)));
}

/// Converts a backtracking-controllable parser to a nom parser that will have the correct
/// backtracking behavior.
#[macro_export]
macro_rules! c {
    ($i:expr, $f:expr) => ({
        match $f($i) {
            $crate::std::result::Result::Ok($crate::nom::IResult::Incomplete(x)) =>
                $crate::nom::IResult::Incomplete(x),
            $crate::std::result::Result::Ok($crate::nom::IResult::Done(i, o)) =>
                $crate::nom::IResult::Done(i, o),
            $crate::std::result::Result::Ok($crate::nom::IResult::Error(e)) =>
                $crate::nom::IResult::Error(e),
            $crate::std::result::Result::Err(e) => return $crate::std::result::Result::Err(e),
        }
    });
    ($i:expr, $f:expr, $($arg: expr),* ) => ({
        match $f($i, $($arg),*) {
            $crate::std::result::Result::Ok($crate::nom::IResult::Incomplete(x)) =>
                $crate::nom::IResult::Incomplete(x),
            $crate::std::result::Result::Ok($crate::nom::IResult::Done(i, o)) =>
                $crate::nom::IResult::Done(i, o),
            $crate::std::result::Result::Ok($crate::nom::IResult::Error(e)) =>
                $crate::nom::IResult::Error(e),
            $crate::std::result::Result::Err(e) => return $crate::std::result::Result::Err(e),
        }
    });
}

/// `p_cut!(E, I -> nom::IResult<I, O, E>) => Err<_, nom::Err<I, E>> OR IResult::Done<I, O> OR IResult::Incomplete<_>`
/// Like `cut!`, but with a custom error type.
#[macro_export]
macro_rules! p_cut {
    ($i: expr, $err: expr, $($args: tt)*) => (cut!($i, ErrorKind::Custom($err), $($args)*));
    ($i: expr, $err: expr, $f: expr) => (cut!($i, $err, call!($f)));
}

/// Returns a custom error for a nom parser.
#[macro_export]
macro_rules! p_nom_error {
    ($e: expr) => ($crate::nom::IResult::Error(custom_error!($e)));
}

/// Returns a custom error for a backtracking-controllable parser.
#[macro_export]
macro_rules! p_fail {
    ($e: expr) => (return $crate::std::result::Result::Err(custom_error!($e)));
}

/// Binds monadically without backtracking the result of a backtracking-controllable parser.
#[macro_export]
macro_rules! p_unwrap {
   ($r: expr) => ({
        match $r {
            $crate::std::result::Result::Ok($crate::nom::IResult::Done(i, o)) => (i, o),
            $crate::std::result::Result::Ok($crate::nom::IResult::Incomplete(n)) =>
                return $crate::std::result::Result::Ok($crate::nom::IResult::Incomplete(n)),
            $crate::std::result::Result::Ok($crate::nom::IResult::Error(e)) =>
                return $crate::std::result::Result::Err(e),
            $crate::std::result::Result::Err(e) => return $crate::std::result::Result::Err(e),
        }
    })
}

/// Wraps the result of a nom parser (`nom::IResult`) to be non-backtracking.
#[macro_export]
macro_rules! wrap_nom {
    ($r: expr) => ({
        match $r {
            $crate::nom::IResult::Done(i, o) =>
                $crate::std::result::Result::Ok($crate::nom::IResult::Done(i, o)),
            i @ $crate::nom::IResult::Incomplete(_) => $crate::std::result::Result::Ok(i),
            $crate::nom::IResult::Error(e) => return $crate::std::result::Result::Err(e),
        }
    })
}

/// Wraps a nom parser (returning `nom::IResult`) to produce a parser that does not
/// backtrack on error.
#[macro_export]
macro_rules! p_wrap_nom {
    ($i: expr, $submac: ident ! ( $($args: tt)* )) => (wrap_nom!($submac!($i, $($args)*)));
    ($i: expr, $f: expr) => (p_wrap_nom!($i, call!($f)));
}

/// Binds monadically without backtracking a backtracking-controllable parser.
#[macro_export]
macro_rules! p_try {
    ($i: expr, $submac: ident ! ( $($args:tt)* )) => (match $submac!($i, $($args)*) {
        $crate::std::result::Result::Ok($crate::nom::IResult::Done(i, o)) => (i, o),
        $crate::std::result::Result::Ok($crate::nom::IResult::Error(e)) =>
            return $crate::std::result::Result::Ok($crate::nom::IResult::Error(e)),
        $crate::std::result::Result::Ok($crate::nom::IResult::Incomplete(i)) =>
            return $crate::std::result::Result::Ok($crate::nom::IResult::Incomplete(i)),
        $crate::std::result::Result::Err(e) => return $crate::std::result::Result::Err(e),
    });
    ($i: expr, $f: expr) => (
        p_try!($i, call!($f))
  );
}

#[cfg(test)]
mod test {
    use nom::IResult;
    use nom::Err;
    use nom::ErrorKind;

    n!(pub foo< bool >,
       chain!(
           tag!("a") ~
               cut!($crate::nom::ErrorKind::Custom(42), tag!("b")) ,
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
