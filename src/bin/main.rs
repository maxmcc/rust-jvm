extern crate rust_jvm;

use std::io::Read;

use rust_jvm::parser::class_file;

fn main() {
    let file_name = std::env::args().nth(1).unwrap();
    let mut file = std::fs::File::open(file_name).unwrap();
    let mut bytes = vec![];
    file.read_to_end(&mut bytes).unwrap();
    let class = class_file::parse_class_file(&bytes).unwrap();
    println!("{:#?}", class);
}

