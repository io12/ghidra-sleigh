fn main() {
    let spec_path = &std::env::args().collect::<Vec<String>>()[1];
    sleigh_build_script_helper::build(spec_path);
}
