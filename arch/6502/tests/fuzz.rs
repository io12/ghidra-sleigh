#[test]
fn fuzz() {
    let u8_iter = u8::MIN..=u8::MAX;
    let start_addr = 0x1234;
    for a in u8_iter.clone() {
        for b in u8_iter.clone() {
            for c in u8_iter.clone() {
                let input = [a, b, c];
                let expected = match disasm6502::from_addr_array(&input, start_addr)
                    .unwrap()
                    .as_slice()
                {
                    [] => None,
                    [insn, ..] => Some(insn.as_str().trim().to_owned()),
                };
                let actual = sleigh_6502::Instruction::disasm(&input, start_addr)
                    .map(|insn| insn.to_string());
                assert_eq!(expected, actual);
            }
        }
    }
}
