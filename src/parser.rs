pub use itertools::Itertools;
pub use nom::{
    branch::*,
    bytes::complete::*,
    character::complete::*,
    combinator::*,
    error::{context, ErrorKind, ParseError},
    multi::*,
    sequence::*,
};
pub use std::str::FromStr;

// use nom::error::Error;
pub use nom::error::VerboseError as Error;
pub type IResult<I, O, E = Error<I>> = nom::IResult<I, O, E>;

pub fn comment<I, E: ParseError<I>>(c: char) -> impl FnMut(I) -> IResult<I, (), E>
where
    I: nom::InputTakeAtPosition + nom::InputIter + nom::Slice<std::ops::RangeFrom<usize>>,
    <I as nom::InputIter>::Item: nom::AsChar,
    &'static str: nom::FindToken<<I as nom::InputTakeAtPosition>::Item>,
{
    value(
        (), // Output is thrown away.
        pair(char(c), is_not("\n\r")),
    )
}

/// Apply the parser to the next line of the input, ignoring anything matched by comment and the newline character (or eof)
pub fn line<I, O, OC, E, F, C>(mut parser: F, mut comment: C) -> impl FnMut(I) -> IResult<I, O, E>
where
    F: nom::Parser<I, O, E>,
    C: nom::Parser<I, OC, E>,
    E: ParseError<I>,
    I: nom::InputTake + nom::Compare<&'static str> + nom::InputLength + Copy,
{
    move |input: I| {
        let (input, output) = parser.parse(input)?;
        let input = match comment.parse(input) {
            Ok((i, _)) => i,
            Err(_) => input,
        };
        if input.input_len() != 0 {
            let (rest, _) = tag("\n")(input)?;
            Ok((rest, output))
        } else {
            Ok((input, output))
        }
    }
}

pub fn parse_to<'i, O, E>(input: &'i str) -> IResult<&'i str, O, E>
where
    O: FromStr,
    E: ParseError<&'i str>,
{
    let (rest, input) = take_till(|s: char| s.is_whitespace())(input)?;
    match input.parse() {
        Ok(o) => Ok((rest, o)),
        Err(_) => Err(nom::Err::Error(E::from_error_kind(
            input,
            ErrorKind::ParseTo,
        ))),
    }
}

pub fn equals_lowercase(first: &str, second: &str) -> bool {
    let first = first.chars().map(char::to_lowercase).flatten();
    let second = second.chars().map(char::to_lowercase).flatten();
    first.zip_longest(second).all(|or| match or {
        itertools::EitherOrBoth::Both(a, b) if a == b => true,
        _ => false,
    })
}

/// Recognizes zero or more unicode whitespace characters, but not newlines
pub fn whitespace0<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
where
    T: nom::InputTakeAtPosition,
    <T as nom::InputTakeAtPosition>::Item: nom::AsChar + Clone,
{
    use nom::AsChar as _;
    let out = input.split_at_position_complete(|item| {
        let c = item.as_char();
        !c.is_whitespace()
    });
    out
}

/// Recognizes zero or more unicode whitespace characters, but not newlines
pub fn inlinespace0<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
where
    T: nom::InputTakeAtPosition,
    <T as nom::InputTakeAtPosition>::Item: nom::AsChar + Clone,
{
    use nom::AsChar as _;
    let out = input.split_at_position_complete(|item| {
        let c = item.as_char();
        !(c.is_whitespace() && c != '\n')
    });
    out
}

pub fn is_not_ws_or<T, Input, Error: ParseError<Input>>(
    arr: T,
) -> impl Fn(Input) -> IResult<Input, Input, Error>
where
    Input: nom::InputTakeAtPosition,
    T: nom::FindToken<<Input as nom::InputTakeAtPosition>::Item>,
    <Input as nom::InputTakeAtPosition>::Item: nom::AsChar + Clone,
{
    use nom::AsChar as _;
    move |i: Input| {
        let e = nom::error::ErrorKind::IsNot;
        i.split_at_position1_complete(
            |c| c.clone().as_char().is_whitespace() || arr.find_token(c),
            e,
        )
    }
}

/// A combinator that takes a parser `inner` and produces a parser that also consumes both leading and
/// trailing unicode whitespace (except newline), returning the output of `inner`.
pub fn ws<'a, F: 'a, O, E: ParseError<&'a str>>(
    inner: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: Fn(&'a str) -> IResult<&'a str, O, E>,
    E: std::fmt::Debug,
{
    delimited(inlinespace0, inner, inlinespace0)
}
