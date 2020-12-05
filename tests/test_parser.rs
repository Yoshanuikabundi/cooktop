use cooktop::gromacs::lexer::GmxLexer;
use cooktop::gromacs::parser::{GmxParser, Topology};
use std::io::Read;
use std::path::PathBuf;

#[test]
fn c22_top_parses() -> Result<(), std::io::Error> {
    let data_path = PathBuf::from("tests/data/chignolin_gmx/Cln_025.top");

    let mut gmx_top = String::new();
    std::fs::File::open(&data_path)?.read_to_string(&mut gmx_top)?;

    let lexer = GmxLexer::new(&gmx_top).add_include_path(data_path.parent().unwrap());

    let parser = GmxParser::from_lexer(lexer);

    let gmx_top = Topology::parse(parser);

    match gmx_top {
        Ok(_) => (),
        Err(e) => panic!(e),
    }

    println!("{:?}", gmx_top);

    Ok(())
}
