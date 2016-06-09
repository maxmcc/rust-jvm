# Rust JVM Project Final Report
CIS 198 Final Project
Meyer Kizner, Max McCarthy, David Xu

## Summary

We implemented a considerable subset of a Java virtual machine, in accordance
with the Java SE 7 specification for the JVM. Our project consists of a parser,
class loader, and interpreter, which together can be used to read an arbitrary
class file (subject to certain restrictions on the Java bytecode it contains),
load it (and other necessary classes from the Java runtime libraries) into an
interpreter, and execute the main method. We achieved near-completion of each of
these features, but got stuck within initialization of the `PrintWriter` and
`FileDescriptor` classes for the `System.out.println` method, since they used a
number of JVM features (namely, loading the
`java.util.concurrent.atomic.AtomicInteger` class) which we had not intended to
support and did not end up having time to complete.

## Approximate time spent

We devoted approximately 120 person-hours to this project.

## Project structure

To aid in our development of the class file parser, we relied on a (somewhat
immature) parser combinator library called [nom](//github.com/Geal/nom).
Unfortunately, nom's error model produces pretty much unuseable error messages,
so much of our effort in developing the parser was spent on wrapping nom's
macros with other macros to provide backtracking capabilities.

The remainder of our project consists of the `vm` module, which consists of a
bootstrap class loader, type definitions for various runtime entities like
classes, objects, methods, and arrays, and the implementation of the JVM stack,
its frames, and a (pretty big!) pattern-match on various bytecode instructions.

We found that despite the JVM specification being a reasonably thorough 550-page
document, significant portions of key functionality were underspecified (or
clearly written under many of the same assumptions made by Oracle's existing
HotSpot JVM implementation). At many points, we found we had to make design
choices because Oracle derived their specification a C++ reference
implementation, which precluded us from using Rust's more modern language
features in several key areas. For instance, we had intended to translate the
individual bytecode instructions into more semantic cases of a Rust enum,
combining things like `iload_0`, `iload_1`, `iload_2`, etc. into a single
`iload` instruction. However, the addition of a `wide` operand made this
approach untenable, because it required a reinterpretation of the bytes in the
code section of a method.

Another particularly frustrating design decision made by Sun/Oracle was to have
32-bit int, float, and reference values occupy one index of the constant pool,
while 64-bit long and double values occupy two indices. Following Oracle's lead
would require us to break up these larger values to store them in an array of
Rust type `Vec<i32>`, and to recombine them whenever necessary to use Rust's i64
and f64 operations. We broke with Oracle's largely untyped approach to their JVM
implementation by introducing an enum called `Value` with a tag for each type,
and modeling the constant pool array as a `Vec<Option<Value>>`. We push either a
single 64-bit value or a 32-bit value followed by `None` so our indices match
the ones appearing in the bytecode instructions themselves. (Oracle did note in
a footnote that, "In retrospect, making 8-byte constants take two constant pool
entries was a poor choice," which was at least somewhat gratifying to read.)

## Testing

We expect our project to be unlike the majority of others submitted for this
class in that our implementation was almost completely driven by a single
integration test---the HelloWorld class. Because the architecture of the JVM
as-specified is so tightly-coupled, it would have been difficult to modularize
and unit-test parts of our implementation while remaining largely
specification-compliant. (We did some amount of unit-testing for our parser.) At
many times, we also created modified or bogus class files by hand and ran them
in the HotSpot JVM to observe its behavior where the specification was unclear
or ambiguous.

## Benchmarks

We didn't bother with any rigorous benchmarking of our JVM versus any existing
implementation of the JVM specification, for several pretty obvious reasons. For
instance, we don't support much more than the minimum instructions necessary to
implement a HelloWorld program. Also, our JVM also doesn't perform just-in-time
lowering of bytecode to native code. However, we do achieve a slight performance
advantage by not bothering to garbage-collect anything!

## Limitations

The primary limitation of our program is its lack of support for arbitrary
programs, especially arbitrary uses of the Java standard library implementation.
Oracle’s implementation of the Java class libraries makes extensive internal use
of the undocumented sun package, which itself calls many native methods
implemented in C++ with respect to Oracle’s JVM implementation. In order to use
arbitrary Java classes, we would need to write versions of the class library
that are implemented with our own Rust “native” methods.

There are many JVM features which we did not attempt to implement at all: among
others, we are missing multithreading and concurrency; reflection; the
invokedynamic instruction in Java 7 and the features introduced in Java 8;
garbage collection; inner classes (other than in the parser); enums; exceptions
and exception handling; custom class loaders; and bytecode verification.

## What went well

Our class file parser is more or less feature-complete, and capable of parsing
Java class files including all features up to and including Java 8. (Our VM does
not attempt to implement some Java 7 and 8 features, however.) The output of the
parser is a Rust structure which allows the class loader to fairly easily create
the values needed to populate the runtime representation of a class.

We put extensive effort into modeling the classes, methods, and other structures
needed to represent the state of the JVM. Many of the error conditions in the
specification are checked for as specified, although since we do not implement
exceptions or exception handling, we simply panic! when one of these internal
`java.lang.Error` conditions arise. With an implementation of the Java class
library which is compatible with our JVM, we believe it would be possible to run
many single-threaded Java programs (although not all instructions are
implemented).

## What we would do differently

Given the time invested in fixing the parser's error handling, it might have
been a better investment to write the class file parser by hand. That said, we
hope we to contribute our work back to the nom project as an [example parser](
https://github.com/Geal/nom/issues/14).

We expect that had each of us had more time to understand the specification and
Oracle's design decisions, we would have been able to complete the
implementation of a substantially larger amount of the JVM features. Perhaps the
choice of the JVM was too ambitious---it might have been a better idea to choose
a smaller and better-designed language bytecode (such as the OCaml bytecode).
That said, we learned a lot about the JVM internals as well as about Rust's
various language features in completing this project, and are satisfied with our
progress even if the demo isn't quite what we had hoped for in the beginning.

