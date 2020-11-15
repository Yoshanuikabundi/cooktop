use crate::parser::*;

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

/// A pair of atom types whose order doesn't matter
#[derive(Debug, Clone)]
struct AtomPair(String, String);
impl PartialEq for AtomPair {
    fn eq(&self, other: &Self) -> bool {
        (self.0 == other.0 && self.1 == other.1) || (self.0 == other.1 && self.0 == other.1)
    }
}
impl From<(&str, &str)> for AtomPair {
    fn from(value: (&str, &str)) -> Self {
        AtomPair(value.0.into(), value.1.into())
    }
}

#[derive(Debug, PartialEq, Clone)]
enum BondParams {
    HarmBond {
        b_0: f64,
        k_b: f64,
    },
    G96Bond {
        b_0: f64,
        k_b: f64,
    },
    MorseBond {
        b_0: f64,
        d: f64,
        beta: f64,
    },
    CubicBond {
        b_0: f64,
        c_i23: f64,
    },
    Connection,
    HarmPot {
        b_0: f64,
        k_b: f64,
    },
    FeneBond {
        b_m: f64,
        k_b: f64,
    },
    TabBond {
        tab_n: usize,
        k: f64,
    },
    TabPot {
        tab_n: usize,
        k: f64,
    },
    RestrPot {
        low: f64,
        up_1: f64,
        up_2: f64,
        k_dr: f64,
    },
}
impl FromStr for BondParams {
    type Err = &'static str;

    #[allow(unreachable_code)]
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let func = todo!();
        use BondParams::*;
        let params = match func {
            "1" => HarmBond {
                b_0: todo!(),
                k_b: todo!(),
            },
            "2" => G96Bond {
                b_0: todo!(),
                k_b: todo!(),
            },
            "3" => MorseBond {
                b_0: todo!(),
                d: todo!(),
                beta: todo!(),
            },
            "4" => CubicBond {
                b_0: todo!(),
                c_i23: todo!(),
            },
            "5" => Connection,
            "6" => HarmPot {
                b_0: todo!(),
                k_b: todo!(),
            },
            "7" => FeneBond {
                b_m: todo!(),
                k_b: todo!(),
            },
            "8" => TabBond {
                tab_n: todo!(),
                k: todo!(),
            },
            "9" => TabPot {
                tab_n: todo!(),
                k: todo!(),
            },
            "10" => RestrPot {
                low: todo!(),
                up_1: todo!(),
                up_2: todo!(),
                k_dr: todo!(),
            },
            _ => return Err(todo!()),
        };
        Ok(params)
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
pub enum Directive {
    Defaults(Defaults),
    Atomtypes(Atomtypes),
    Bondtypes(Bondtypes),
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

impl From<Bondtypes> for Directive {
    fn from(value: Bondtypes) -> Self {
        Self::Bondtypes(value)
    }
}

impl Directive {
    fn parse(input: &str) -> IResult<&str, Self> {
        alt((
            map(Defaults::parse, Self::from),
            map(Atomtypes::parse, Self::from),
            map(Bondtypes::parse, Self::from),
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
pub struct Bondtypes {
    pairs: Vec<AtomPair>,
    params: Vec<BondParams>,
}

impl Bondtypes {
    fn parse(input: &str) -> IResult<&str, Self> {
        let (input, _) = Directive::parse_header(input, "bondtypes")?;

        let mut out = Self {
            pairs: Vec::new(),
            params: Vec::new(),
        };

        let (rest, _) = many1_count(map(
            data_line,
            |(atom1, atom2, params, _, _, _, _, _): (
                String,
                String,
                BondParams,
                NoField,
                NoField,
                NoField,
                NoField,
                NoField,
            )| {
                out.pairs.push(AtomPair(atom1, atom2));
                out.params.push(params);
            },
        ))(input)?;

        Ok((rest, out))
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
            HA           1       1.008   0.0000  A   2.59964e-01  6.27600e-02
            Cl          17      35.45    0.0000  A   4.40104e-01  4.18400e-01
            Na          11      22.99    0.0000  A   3.32840e-01  1.15897e-02

            [ bondtypes ]
             Br  CA      1    0.18900  143929.6  ; Amber99Sb-disp
             C   C       1    0.15250  259408.0  ; Amber99Sb-disp
             C   CA      1    0.14090  392459.2  ; Amber99Sb-disp
             F   CA      1    0.13590  323004.8  ; Amber99Sb-disp
             CA  HA      1    0.10800  307105.6  ; Amber99Sb-disp
             Cl  CA      1    0.17270  161502.4  ; Amber99Sb-disp


        ";

        let output = Topology::parse(input)?;

        let mut directives = output.0.iter();

        if let Some(Directive::Defaults(_)) = directives.next() {
        } else {
            panic!("Defaults directive missed")
        };

        if let Some(Directive::Atomtypes(_)) = directives.next() {
        } else {
            panic!("Atomtypes directive missed")
        };

        if let Some(Directive::Bondtypes(_)) = directives.next() {
        } else {
            panic!("Bondtypes directive missed")
        };

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

    #[test]
    fn test_parse_bondtypes() -> Result<(), Box<dyn std::error::Error>> {
        let input = "\
            [ bondtypes ]
             Br  CA      1    0.18900  143929.6  ; Amber99Sb-disp
             C   C       1    0.15250  259408.0  ; Amber99Sb-disp
             C   CA      1    0.14090  392459.2  ; Amber99Sb-disp
             F   CA      1    0.13590  323004.8  ; Amber99Sb-disp
             CA  HA      1    0.10800  307105.6  ; Amber99Sb-disp
             Cl  CA      1    0.17270  161502.4  ; Amber99Sb-disp\
        ";

        let (_, output) = Bondtypes::parse(input)?;

        let expected = Bondtypes {
            pairs: vec![
                ("Br", "CA").into(),
                ("C", "C").into(),
                ("CA", "C").into(),
                ("F", "CA").into(),
                ("HA", "CA").into(),
                ("Cl", "CA").into(),
            ],
            params: vec![
                BondParams::HarmBond {
                    b_0: 0.18900,
                    k_b: 143929.6,
                },
                BondParams::HarmBond {
                    b_0: 0.15250,
                    k_b: 259408.0,
                },
                BondParams::HarmBond {
                    b_0: 0.14090,
                    k_b: 392459.2,
                },
                BondParams::HarmBond {
                    b_0: 0.13590,
                    k_b: 323004.8,
                },
                BondParams::HarmBond {
                    b_0: 0.10800,
                    k_b: 307105.6,
                },
                BondParams::HarmBond {
                    b_0: 0.17270,
                    k_b: 161502.4,
                },
            ],
        };
        assert_eq!(output, expected);

        Ok(())
    }
}
