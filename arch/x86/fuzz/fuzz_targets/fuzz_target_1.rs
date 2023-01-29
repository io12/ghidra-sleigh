#![no_main]

libfuzzer_sys::fuzz_target!(|data: &[u8]| {
    let addr = 0x1234_u32; // TODO: fuzz this
    let expected = {
        let (len, insns) = sleigh::Decompiler::builder()
            .x86(sleigh::X86Mode::Mode32)
            .build()
            .disassemble(data, addr.into());
        if len <= data.len() {
            insns.get(0).map(|insn| {
                format!("{} {}", insn.mnemonic, insn.body)
                    .trim()
                    .to_string()
            })
        } else {
            None
        }
    };
    let actual = sleigh_x86::Instruction::disasm(data, addr, 0b00000000000000000000000001100000)
        .map(|insn| insn.to_string());
    assert_eq!(expected, actual, "input was {data:02X?}");
});
