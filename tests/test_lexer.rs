use cooktop::gromacs::lexer::GmxLexer;
use std::io::Read;
use std::path::PathBuf;

#[test]
fn c22_top_lexes() -> Result<(), std::io::Error> {
    let data_path = PathBuf::from("tests/data/chignolin_gmx/Cln_025.top");

    let mut gmx_top = String::new();
    std::fs::File::open(&data_path)?.read_to_string(&mut gmx_top)?;

    let lexed = GmxLexer::new(&gmx_top)
        .add_macro("POSRES", "")
        .add_include_path(data_path.parent().unwrap())
        .lex_all();

    match lexed {
        Ok(_) => (),
        Err(e) => panic!(e),
    }

    Ok(())
}
