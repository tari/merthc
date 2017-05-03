extern crate gcc;
extern crate phf_codegen;

use std::{env, str};
use std::fs::File;
use std::io::{BufWriter, Write};
use std::path::Path;
use std::process::Command;

macro_rules! rt_file {
    ($phf:expr, $triple:expr, $file:expr) => (
        $phf.entry($triple, concat!("br####\"", concat!(
            include_str!(concat!("runtime/", concat!($file, ".ll"))),
            "\"####"
        )))
    )
}

fn main() {
    generate_runtime();
    build_cpp_helpers();
}

fn generate_runtime() {
    let path = Path::new(&env::var("OUT_DIR").unwrap()).join("rt_map.rs");
    let mut file = BufWriter::new(File::create(&path).unwrap());

    write!(&mut file, "static RT_TARGET_SOURCES: phf::Map<&'static str, &'static [u8]> =").unwrap();
    let mut phf = phf_codegen::Map::new();

    rt_file!(phf, "x86_64-unknown-linux-gnu", "linux-x86_64");

    phf.build(&mut file).unwrap();
    write!(&mut file, ";\n").unwrap();
}

fn build_cpp_helpers() {
    let cfg = Command::new("llvm-config")
                      .arg("--cxxflags")
                      .output()
                      .expect("Failed to execute llvm-config")
                      .stdout;
    let cfg = str::from_utf8(&cfg).expect("llvm-config emitted invalid UTF-8");

    let mut cc = gcc::Config::new();
    for word in cfg.split(' ').filter(|s| s.len() > 0) {
        cc.flag(word);
    }

    let infile = Path::new(&env::var("CARGO_MANIFEST_DIR").unwrap()).join("emit_ir.cpp");
    cc.file(infile);
    cc.compile("libemit_ir.a");
}
