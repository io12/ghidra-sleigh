mod ast;
mod context;
mod parse;
mod preprocess;

fn main() {
    let pp = parse::preprocess(std::io::stdin().lock());
    let (remaining, sleigh) = parse::parse_sleigh(&pp).unwrap();
    assert!(remaining.is_empty());
    let ctx = context::SleighContext::new(&sleigh);
    let mut dis = String::new();
    ctx.disasm_insn(0, None, &[0x96, 0x11, 0x22], &mut dis);
    println!("{dis}");
}
