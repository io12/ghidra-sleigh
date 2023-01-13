use std::fs::File;
use std::io::BufReader;
use std::path::Path;

pub fn build(spec_path: &str) {
    // Generate
    println!("cargo:rerun-if-changed={spec_path}");
    let file = File::open(spec_path).unwrap();
    let reader = BufReader::new(file);
    let spec_dir = Path::new(spec_path).parent().unwrap();
    let sleigh = sleigh_parser::preprocess(reader, spec_dir);
    std::fs::write("/tmp/dump/pp", &sleigh).unwrap();
    let (rest, sleigh) = sleigh_parser::parse_sleigh(&sleigh).unwrap();
    assert_eq!(rest, "");
    let ctx = sleigh_types::context::SleighContext::new(&sleigh);
    let generated = sleigh_generator::RustCodeGenerator::new(&ctx).out();

    // Format
    let generated = syn::parse_file(&generated.to_string()).unwrap();
    let generated = prettyplease::unparse(&generated);

    // Write
    let out_dir = std::env::var_os("OUT_DIR").unwrap();
    let dest_path = Path::new(&out_dir).join("generated.rs");
    std::fs::write(dest_path, generated).unwrap();
}
