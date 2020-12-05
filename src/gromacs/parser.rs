use crate::gromacs::lexer::{GmxLexer, Item};
pub mod defaults;
use defaults::Defaults;
use std::path::Path;

pub trait Directive: std::fmt::Debug {
    type Error;
    fn parse(lexer: &mut GmxLexer<'_>) -> Result<Self, Self::Error>
    where
        Self: Sized;

    fn parse_box(
        lexer: &mut GmxLexer<'_>,
    ) -> Result<Box<dyn Directive<Error = Self::Error>>, Self::Error>
    where
        Self: Sized + 'static,
    {
        Self::parse(lexer).map(|dir| Box::new(dir) as Box<dyn Directive<Error = Self::Error>>)
    }
}

#[derive(Debug)]
pub struct GmxParser<'s> {
    lexer: GmxLexer<'s>,
}

impl<'s> GmxParser<'s> {
    pub fn new(input: &'s str) -> Self {
        Self {
            lexer: GmxLexer::new(input),
        }
    }

    pub fn from_lexer(lexer: GmxLexer<'s>) -> Self {
        Self { lexer }
    }
}

impl<'s> Iterator for GmxParser<'s> {
    type Item = Result<Box<dyn Directive<Error = &'static str>>, &'static str>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.lexer.next() {
            Some(Ok(Item::Directive(name))) => match &name.to_lowercase()[..] {
                "defaults" => Some(Defaults::parse_box(&mut self.lexer)),
                _ => Some(Err("Unknown directive name")),
            },
            Some(Ok(Item::DataLine(_))) => self.next(), // TODO: Some(Err("DataLines should all belong to directives")),
            Some(Ok(_)) => unreachable!(),
            Some(Err(e)) => Some(Err(e)),
            None => None,
        }
    }
}

#[derive(Debug)]
pub enum GmxParseError {
    Msg(&'static str),
    Io(std::io::Error),
}

impl From<&'static str> for GmxParseError {
    fn from(msg: &'static str) -> Self {
        Self::Msg(msg)
    }
}

impl From<std::io::Error> for GmxParseError {
    fn from(err: std::io::Error) -> Self {
        Self::Io(err)
    }
}

#[derive(Debug)]
pub struct Topology {
    directives: Vec<Result<Box<dyn Directive<Error = &'static str>>, GmxParseError>>,
}

impl Topology {
    pub fn open(path: impl AsRef<Path>) -> Result<Self, GmxParseError> {
        let mut gmx_top = String::new();
        use std::io::Read as _;
        std::fs::File::open(path)?.read_to_string(&mut gmx_top)?;

        let parser = GmxParser::new(&gmx_top);

        Self::parse(parser)
    }

    pub fn parse(parser: GmxParser) -> Result<Self, GmxParseError> {
        let vec: Vec<_> = parser
            .map(|result| result.map_err(GmxParseError::from))
            .collect();

        Ok(Self { directives: vec })
    }
}
