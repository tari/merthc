extern crate phf_codegen;

use std::env;
use std::fs::File;
use std::io::{BufWriter, Write};
use std::path::Path;

macro_rules! rt_file {
    ($phf:expr, $triple:expr, $file:expr) => (
        $phf.entry($triple, concat!("br####\"", concat!(
            include_str!(concat!("runtime/", concat!($file, ".ll"))),
            "\"####"
        )))
    )
}

fn main() {
    let path = Path::new(&env::var("OUT_DIR").unwrap()).join("rt_map.rs");
    let mut file = BufWriter::new(File::create(&path).unwrap());

    write!(&mut file, "static RT_TARGET_SOURCES: phf::Map<&'static str, &'static [u8; 2811]> =").unwrap();
    let mut phf = phf_codegen::Map::new();

    rt_file!(phf, "x86_64-unknown-linux-gnu", "linux-x86_64");

    phf.build(&mut file).unwrap();
    write!(&mut file, ";\n").unwrap();
}
