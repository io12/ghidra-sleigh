mod ast;
mod context;
mod parse;
mod rust_gen;

fn main() {
    let pp = parse::preprocess(std::io::BufReader::new(
        std::fs::File::open("6502.slaspec").unwrap(),
    ));
    let (remaining, sleigh) = parse::parse_sleigh(&pp).unwrap();
    assert!(remaining.is_empty());
    let ctx = context::SleighContext::new(&sleigh);
    println!("{}", ctx.define_rust_types());
}
