use super::Directive;
use crate::gromacs::lexer::{GmxLexer, Item};
use std::convert::TryFrom;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum CombinationRule {
    C6C12GeometricMean,
    SigmaEpsilonLorentzBerthelot,
    SigmaEpsilonGeometricMean,
}

impl TryFrom<i32> for CombinationRule {
    type Error = &'static str;
    fn try_from(int: i32) -> Result<Self, Self::Error> {
        match int {
            1 => Ok(Self::C6C12GeometricMean),
            2 => Ok(Self::SigmaEpsilonLorentzBerthelot),
            3 => Ok(Self::SigmaEpsilonGeometricMean),
            _ => Err("Invalid combination rule"),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum NonBondedFunction {
    // 1
    LennardJones(CombinationRule),
    //2
    Buckingham,
}

impl TryFrom<(i32, i32)> for NonBondedFunction {
    type Error = &'static str;
    fn try_from(
        (nbfunc, combo_rule): (i32, i32),
    ) -> Result<Self, <CombinationRule as TryFrom<i32>>::Error> {
        match (nbfunc, combo_rule) {
            (1, n) => Ok(Self::LennardJones(CombinationRule::try_from(n)?)),
            (2, 1) => Ok(Self::Buckingham),
            (2, _) => Err("Buckingham potential has only 1 valid combination rule"),
            _ => Err("Only possibly values of nbfunc are 1 for Lennard-Jones or 2 for Buckingham"),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum GeneratePairs {
    /// Generate 1-4 pairs absent from the pair list with normal LJ parameters and fudge_lj
    Yes { fudge_lj: f32 },
    /// Take 1-4 parameters from pairtypes list, and fatally error if a parameter is missing
    No,
}

impl Default for GeneratePairs {
    fn default() -> Self {
        GeneratePairs::Yes { fudge_lj: 1.0 }
    }
}

impl TryFrom<&str> for GeneratePairs {
    type Error = &'static str;
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match &value.to_lowercase()[..] {
            "yes" => Ok(Self::Yes { fudge_lj: 1.0 }),
            "no" => Ok(Self::No),
            _ => Err("Unrecognised genpairs"),
        }
    }
}

impl TryFrom<(&str, f32)> for GeneratePairs {
    type Error = &'static str;
    fn try_from((genpairs, fudge_lj): (&str, f32)) -> Result<Self, Self::Error> {
        match &genpairs.to_lowercase()[..] {
            "yes" => Ok(Self::Yes { fudge_lj }),
            "no" => Ok(Self::No),
            _ => Err("Unrecognised genpairs"),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Defaults {
    nbfunc: NonBondedFunction,
    /// Whether to automatically generate 1-4 pairs
    gen_pairs: GeneratePairs,
    /// Factor by which to multiply electrostatic 1-4 interactions, default 1.0
    fudge_qq: f32,
    /// Power for the repulsion term  in a 6-N potential
    n: i32,
}

impl TryFrom<Vec<&str>> for Defaults {
    type Error = &'static str;
    fn try_from(vec: Vec<&str>) -> Result<Self, Self::Error> {
        let nbfunc = if let [nbfunc, combo_rule] = vec.as_slice() {
            NonBondedFunction::try_from((
                nbfunc
                    .parse()
                    .map_err(|_| "Could not parse nbfunc as i32")?,
                combo_rule
                    .parse()
                    .map_err(|_| "Could not parse combo_rule as i32")?,
            ))?
        } else {
            return Err("nbfunc and combo_rule must be defined in Defaults directive");
        };

        let gen_pairs = if let [_, _, gen_pairs, fudge_lj] = vec.as_slice() {
            GeneratePairs::try_from((
                *gen_pairs,
                fudge_lj
                    .parse()
                    .map_err(|_| "Could not parse fudge_lj as f32")?,
            ))?
        } else if let [_, _, gen_pairs] = vec.as_slice() {
            GeneratePairs::try_from(*gen_pairs)?
        } else {
            return Err("nbfunc and combo_rule must be defined in Defaults directive");
        };

        let fudge_qq = if let [_, _, _, _, fudge_qq] = vec.as_slice() {
            fudge_qq
                .parse()
                .map_err(|_| "Could not parse fudge_qq as f32")?
        } else {
            1.0
        };

        let n = if let [_, _, _, _, _, n] = vec.as_slice() {
            n.parse().map_err(|_| "Could not parse n as i32")?
        } else {
            12
        };

        Ok(Self {
            nbfunc,
            gen_pairs,
            fudge_qq,
            n,
        })
    }
}

impl Directive for Defaults {
    type Error = &'static str;

    fn parse<'s>(lexer: &mut GmxLexer<'_>) -> Result<Self, Self::Error> {
        let dataline = lexer.next().ok_or("EOF")??;

        match dataline {
            Item::DataLine(vec) => Ok(Self::try_from(vec)?),
            _ => return Err("Directive Defaults should have exactly one data line"),
        }
    }
}
