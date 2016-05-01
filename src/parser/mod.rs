//! Contains a parser for a Java class file.
//!
//! # Examples
//!
//! Basic usage:
//! ```
//! let data = include_bytes!("../../data/HelloWorld.class");
//! assert!(parse_class_file(data).is_done()); // returns a nom::IResult
//! ```

#[macro_use]
pub mod nom_support;

pub mod class_file;
