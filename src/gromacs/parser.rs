use itertools::Itertools;
use nom::{
    branch::*,
    bytes::complete::*,
    character::complete::*,
    combinator::*,
    error::{context, ErrorKind, ParseError},
    multi::*,
    sequence::*,
};
use std::str::FromStr;

fn equals_lowercase(first: &str, second: &str) -> bool {
    let first = first.chars().map(char::to_lowercase).flatten();
    let second = second.chars().map(char::to_lowercase).flatten();
    first.zip_longest(second).all(|or| match or {
        itertools::EitherOrBoth::Both(a, b) if a == b => true,
        _ => false,
    })
}

struct NoField;
impl FromStr for NoField {
    type Err = &'static str;
    fn from_str(_: &str) -> Result<Self, Self::Err> {
        Ok(Self)
    }
}

enum YesOrNo {
    Yes,
    No,
}
impl FromStr for YesOrNo {
    type Err = &'static str;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if equals_lowercase(s, "yes") {
            Ok(Self::Yes)
        } else if equals_lowercase(s, "no") {
            Ok(Self::No)
        } else {
            Err("Not yes or no")
        }
    }
}
impl Into<bool> for YesOrNo {
    fn into(self) -> bool {
        match self {
            Self::Yes => true,
            Self::No => true,
        }
    }
}

// use nom::error::Error;
use nom::error::VerboseError as Error;
type IResult<I, O, E = Error<I>> = nom::IResult<I, O, E>;

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

fn parse_to<'i, O, E>(input: &'i str) -> IResult<&'i str, O, E>
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

pub fn data_line<'i, A, B, C, D, E, F, G, H, Er>(
    input: &'i str,
) -> IResult<&'i str, (A, B, C, D, E, F, G, H), Er>
where
    Er: ParseError<&'i str>,
    A: FromStr,
    B: FromStr,
    C: FromStr,
    D: FromStr,
    E: FromStr,
    F: FromStr,
    G: FromStr,
    H: FromStr,
{
    let parser = |i| {
        let (i, _) = inlinespace0(i)?;
        let (i, a) = parse_to::<A, _>(i)?;
        let (i, _) = inlinespace0(i)?;
        let (i, b) = parse_to::<B, _>(i)?;
        let (i, _) = inlinespace0(i)?;
        let (i, c) = parse_to::<C, _>(i)?;
        let (i, _) = inlinespace0(i)?;
        let (i, d) = parse_to::<D, _>(i)?;
        let (i, _) = inlinespace0(i)?;
        let (i, e) = parse_to::<E, _>(i)?;
        let (i, _) = inlinespace0(i)?;
        let (i, f) = parse_to::<F, _>(i)?;
        let (i, _) = inlinespace0(i)?;
        let (i, g) = parse_to::<G, _>(i)?;
        let (i, _) = inlinespace0(i)?;
        let (i, h) = parse_to::<H, _>(i)?;
        let (i, _) = inlinespace0(i)?;

        Ok((i, (a, b, c, d, e, f, g, h)))
    };

    preceded(
        many0_count(line(inlinespace0, comment(';'))),
        line(parser, comment(';')),
    )(input)
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

#[derive(Debug, PartialEq, Clone)]
pub enum Directive {
    Defaults(Defaults),
    Atomtypes(Atomtypes),
}

impl From<Defaults> for Directive {
    fn from(value: Defaults) -> Self {
        Self::Defaults(value)
    }
}

impl From<Atomtypes> for Directive {
    fn from(value: Atomtypes) -> Self {
        Self::Atomtypes(value)
    }
}

impl Directive {
    fn parse(input: &str) -> IResult<&str, Self> {
        alt((
            map(Defaults::parse, Self::from),
            map(Atomtypes::parse, Self::from),
        ))(input)
    }

    fn parse_header<'i>(input: &'i str, name: &str) -> IResult<&'i str, &'i str>
    where
        Self: Sized,
    {
        context(
            "Directive name not recognised",
            verify(
                context(
                    "Expected directive not found",
                    line(
                        delimited(ws(tag("[")), is_not_ws_or("]"), ws(tag("]"))),
                        comment(';'),
                    ),
                ),
                |s: &str| equals_lowercase(s, name),
            ),
        )(input)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Atomtypes {
    names: Vec<String>,
    atomic_numbers: Vec<i64>,
    masses: Vec<f64>,
    charges: Vec<f64>,
    particle_types: Vec<ParticleType>,
    lj_vs: Vec<f64>,
    lj_ws: Vec<f64>,
}

impl Atomtypes {
    fn parse(input: &str) -> IResult<&str, Self> {
        let (input, _) = Directive::parse_header(input, "atomtypes")?;

        let mut out = Self {
            names: Vec::new(),
            atomic_numbers: Vec::new(),
            masses: Vec::new(),
            charges: Vec::new(),
            particle_types: Vec::new(),
            lj_vs: Vec::new(),
            lj_ws: Vec::new(),
        };

        let (rest, _) = many1_count(map(
            data_line,
            |(name, atomic_number, mass, charge, particle_type, lj_v, lj_w, _): (
                String,
                i64,
                f64,
                f64,
                ParticleType,
                f64,
                f64,
                NoField,
            )| {
                out.names.push(name);
                out.atomic_numbers.push(atomic_number);
                out.masses.push(mass);
                out.charges.push(charge);
                out.particle_types.push(particle_type);
                out.lj_vs.push(lj_v);
                out.lj_ws.push(lj_w);
            },
        ))(input)?;

        Ok((rest, out))
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Defaults {
    nb_func: i64,
    combo_rule: i64,
    generate_pairs: bool,
    fudge_lj: f64,
    fudge_qq: f64,
}

impl Defaults {
    fn parse(input: &str) -> IResult<&str, Self> {
        let (input, _) = Directive::parse_header(input, "defaults")?;
        let (rest, (nb_func, combo_rule, generate_pairs, fudge_lj, fudge_qq, _, _, _)) =
            data_line::<i64, i64, YesOrNo, f64, f64, NoField, NoField, NoField, _>(input)?;

        Ok((
            rest,
            Self {
                nb_func,
                combo_rule,
                generate_pairs: generate_pairs.into(),
                fudge_lj,
                fudge_qq,
            },
        ))
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct ParsePtypeError;
impl std::fmt::Display for ParsePtypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "String was not A, S, V or D")
    }
}
impl std::error::Error for ParsePtypeError {}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum ParticleType {
    /// A regular atom
    Atom,
    /// A shell for polarizable models, currently unused in GROMACS
    Shell,
    /// A virtual site, whose position is constructed from other atoms
    VirtualSite,
}

impl FromStr for ParticleType {
    type Err = ParsePtypeError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "A" => Ok(Self::Atom),
            "S" => Ok(Self::Shell),
            "V" | "D" => Ok(Self::VirtualSite),
            _ => Err(ParsePtypeError),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Topology(Vec<Directive>);

impl Topology {
    pub fn parse(input: &str) -> Result<Topology, nom::Err<Error<&str>>> {
        let (_, directives) = all_consuming(many1(preceded(
            many0_count(line(inlinespace0, comment(';'))),
            terminated(line(Directive::parse, comment(';')), whitespace0),
        )))(input)?;

        Ok(Topology(directives))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_same_line_comment() -> Result<(), Box<dyn std::error::Error>> {
        let input = "\
            [deFaults ] ; same line comment \n\
            1       2     yes     0.5  0.8333\
        ";

        let (_, output) = Defaults::parse(input)?;

        assert_eq!(
            output,
            Defaults {
                nb_func: 1,
                combo_rule: 2,
                generate_pairs: true,
                fudge_lj: 0.5,
                fudge_qq: 0.8333
            }
        );

        Ok(())
    }

    #[test]
    fn test_comment_line() -> Result<(), Box<dyn std::error::Error>> {
        let input = "\
            [ defaults ] \n\
            ; nb   combo  pairs   fudge_lj  fudge_qq
            1       2     yes     0.5  0.8333\
        ";

        let (_, output) = Defaults::parse(input)?;

        assert_eq!(
            output,
            Defaults {
                nb_func: 1,
                combo_rule: 2,
                generate_pairs: true,
                fudge_lj: 0.5,
                fudge_qq: 0.8333
            }
        );

        Ok(())
    }

    #[test]
    fn test_parse_topology() -> Result<(), Box<dyn std::error::Error>> {
        let input = "\
            [ defaults ]
            ; nb   combo  pairs   fudge_lj  fudge_qq
            1       2     yes     0.5  0.8333

            [ atomtypes ]
            ; name      at.num  mass     charge ptype  sigma      epsilon
            Br          35      79.90    0.0000  A   0.00000e+00  0.00000e+00
            C            6      12.01    0.0000  A   3.39967e-01  3.59824e-01
            CA           6      12.01    0.0000  A   3.39967e-01  3.59824e-01
            F            9      19.00    0.0000  A   3.11815e-01  2.55224e-01
            H            1       1.008   0.0000  A   1.06908e-01  6.56888e-02
            Cl          17      35.45    0.0000  A   4.40104e-01  4.18400e-01
            Na          11      22.99    0.0000  A   3.32840e-01  1.15897e-02


        ";

        let output = Topology::parse(input)?;

        assert_eq!(
            output,
            Topology(vec![
                Directive::Defaults(Defaults {
                    nb_func: 1,
                    combo_rule: 2,
                    generate_pairs: true,
                    fudge_lj: 0.5,
                    fudge_qq: 0.8333
                }),
                Directive::Atomtypes(Atomtypes {
                    names: vec![
                        "Br".into(),
                        "C".into(),
                        "CA".into(),
                        "F".into(),
                        "H".into(),
                        "Cl".into(),
                        "Na".into()
                    ],
                    atomic_numbers: vec![35, 6, 6, 9, 1, 17, 11],
                    masses: vec![79.9, 12.01, 12.01, 19.0, 1.008, 35.45, 22.99],
                    charges: vec![0.0; 7],
                    particle_types: vec![ParticleType::Atom; 7],
                    lj_vs: vec![0.0, 0.339967, 0.339967, 0.311815, 0.106908, 0.440104, 0.332840],
                    lj_ws: vec![0.0, 0.359824, 0.359824, 0.255224, 0.0656888, 0.418400, 0.0115897]
                })
            ])
        );

        Ok(())
    }

    #[test]
    fn test_parse_defaults() -> Result<(), Box<dyn std::error::Error>> {
        let input = "\
            [deFaults ] \n\
            1       2     yes     0.5  0.8333\
        ";

        let (_, output) = Defaults::parse(input)?;

        assert_eq!(
            output,
            Defaults {
                nb_func: 1,
                combo_rule: 2,
                generate_pairs: true,
                fudge_lj: 0.5,
                fudge_qq: 0.8333
            }
        );

        Ok(())
    }

    #[test]
    fn test_parse_atomtypes() -> Result<(), Box<dyn std::error::Error>> {
        let input = "\
            [ atomtypes ]
            ; name      at.num  mass     charge ptype  sigma      epsilon
            Br          35      79.90    0.0000  A   0.00000e+00  0.00000e+00
            C            6      12.01    0.0000  A   3.39967e-01  3.59824e-01
            CA           6      12.01    0.0000  A   3.39967e-01  3.59824e-01
            F            9      19.00    0.0000  A   3.11815e-01  2.55224e-01
            H            1       1.008   0.0000  A   1.06908e-01  6.56888e-02
            Cl          17      35.45    0.0000  A   4.40104e-01  4.18400e-01
            Na          11      22.99    0.0000  A   3.32840e-01  1.15897e-02\
        ";

        let (_, output) = Atomtypes::parse(input)?;

        let expected = Atomtypes {
            names: vec![
                "Br".into(),
                "C".into(),
                "CA".into(),
                "F".into(),
                "H".into(),
                "Cl".into(),
                "Na".into(),
            ],
            atomic_numbers: vec![35, 6, 6, 9, 1, 17, 11],
            masses: vec![79.9, 12.01, 12.01, 19.0, 1.008, 35.45, 22.99],
            charges: vec![0.0; 7],
            particle_types: vec![ParticleType::Atom; 7],
            lj_vs: vec![
                0.0, 0.339967, 0.339967, 0.311815, 0.106908, 0.440104, 0.332840,
            ],
            lj_ws: vec![
                0.0, 0.359824, 0.359824, 0.255224, 0.0656888, 0.418400, 0.0115897,
            ],
        };

        assert_eq!(output.names, expected.names);
        assert_eq!(output.atomic_numbers, expected.atomic_numbers);
        assert_eq!(output.masses, expected.masses);
        assert_eq!(output.charges, expected.charges);
        assert_eq!(output.particle_types, expected.particle_types);
        assert_eq!(output.lj_vs, expected.lj_vs);
        assert_eq!(output.lj_ws, expected.lj_ws);
        assert_eq!(output, expected);

        Ok(())
    }
}
