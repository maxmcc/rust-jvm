# rust-jvm

This project was submitted by Meyer Kizner, Max McCarthy, and David Xu as our
final project for [CIS 198](//cis198-2016s.github.io) at the University of
Pennsylvania in spring 2016.

Please note that because this project was completed for a course, it is **not**
under active development. The code and documentation in this repository is
available as-is, and we will not be providing support beyond what already exists
here.

## About

This project implements a considerable subset of a Java virtual machine, in
accordance with the Java SE 7 specification. It consists of a parser, class
loader, and interpreter, which together can be used to read an arbitrary class
file (subject to certain restrictions on the Java bytecode it contains), load it
(and other necessary classes from the Java runtime libraries) into an
interpreter, and execute the `main` method.

We achieved near-completion of many of these features, but got stuck within
initialization of the `PrintWriter` and `FileDescriptor` classes for the
`System.out.println` method, since they used a number of JVM features (namely,
loading the `java.util.concurrent.atomic.AtomicInteger` class) which we had not
intended to support. For this reason, the minimum required parts of the Java
Class Library (JCL) are reimplemented in the `rt/` directory in terms of a
`RustStdout` class, which calls a `native` method for writing to a file
descriptor.

As it stands, the VM successfully executes a `HelloWorld.class` file. It has not
been tested beyond that.

To find out more information about the project, please see
[PROPOSAL.md](//github.com/maxmcc/rust-jvm/blob/master/PROPOSAL.md) and
[REPORT.md](//github.com/maxmcc/rust-jvm/blob/master/REPORT.md) for our initial
project propsoal and final report.

## Running Hello World

This project is organized around a single integration test: executing a compiled
"Hello, world!" program from a `.class` file.

Our VM does not support Java 7 and 8 features (including lambdas and the
`invokedynamic` instruction). You will need to compile the Java source code in
the `rt` directory targeting source level `1.6`, which can be accomplished with
the following command:

```sh
$ cd rt/
$ javac -source 1.6 -target 1.6 HelloWorld.java
```

Then, run `cargo test` from the project root and you should see the test pass.

## License

This software is freely available under the terms of the [Apache License
2.0](//apache.org/licenses/LICENSE-2.0).

