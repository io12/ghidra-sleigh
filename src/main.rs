mod ast;
mod context;
mod parse;

fn main() {
    let pp = parse::preprocess(std::io::stdin().lock());
    let (remaining, sleigh) = parse::parse_sleigh(&pp).unwrap();
    assert!(remaining.is_empty());
    let ctx = context::SleighContext::new(&sleigh);
    println!("{}", ctx.define_rust_types());
}
