//! This module is for code relating to rolling the dice - generating random values and mapping
//! that to success values.

use rand::{thread_rng, Rng};
use rand_distr::{Distribution, Uniform};
use serde::{Deserialize, Serialize};

use crate::commands::roll::{Dice, Thorns};

/// The possible results of a roll in the Moxie system.
///
/// Wraps the actual roll value.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Roll {
    Disaster(u8),
    Grim(u8),
    Messy(u8),
    Perfect(u8),
    Critical,
    MultiCritical,
}
impl Roll {
    pub fn is_grim(self) -> bool {
        matches!(self, Roll::Grim(_))
    }
    pub fn is_perfect(self) -> bool {
        matches!(self, Roll::Perfect(_))
    }
    // pub fn is_critical(self) -> bool {
    //     matches!(self, Roll::Critical | Roll::MultiCritical)
    // }
    pub fn as_number(self) -> u8 {
        match self {
            Roll::Disaster(r) | Roll::Grim(r) | Roll::Messy(r) | Roll::Perfect(r) => r,
            Roll::Critical | Roll::MultiCritical => 6,
        }
    }
    pub fn replace_roll(self, replacements: &[(Roll, Roll)]) -> Self {
        replacements
            .iter()
            .find_map(|(from, to)| self.replace_one_roll(*from, *to))
            .unwrap_or(self)
    }
    pub fn replace_one_roll(self, from: Roll, to: Roll) -> Option<Self> {
        (self == from).then(|| {
            let from_number = from.as_number();
            match to {
                Roll::Disaster(_) => Roll::Disaster(from_number),
                Roll::Grim(_) => Roll::Grim(from_number),
                Roll::Messy(_) => Roll::Messy(from_number),
                Roll::Perfect(_) => Roll::Perfect(from_number),
                crits => crits,
            }
        })
    }
    pub fn cut(self, thorn: Thorn) -> Roll {
        if thorn.is_cut() {
            match self {
                Roll::Disaster(r) => Roll::Disaster(r),
                Roll::Grim(r) => Roll::Disaster(r),
                Roll::Messy(r) => Roll::Grim(r),
                Roll::Perfect(r) => Roll::Messy(r),
                crit => crit,
            }
        } else {
            self
        }
    }
}

impl TryFrom<u8> for Roll {
    type Error = u8;

    fn try_from(roll: u8) -> Result<Self, Self::Error> {
        match roll {
            r @ 1..=3 => Ok(Roll::Grim(r)),
            r @ (4 | 5) => Ok(Roll::Messy(r)),
            r @ 6 => Ok(Roll::Perfect(r)),
            other => Err(other),
        }
    }
}
impl std::fmt::Display for Roll {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Roll::Disaster(_) => "Disaster",
            Roll::Grim(_) => "Grim",
            Roll::Messy(_) => "Messy",
            Roll::Perfect(_) => "Perfect",
            Roll::Critical => "Critical!",
            Roll::MultiCritical => "Multi critical! Take spark!",
        }
        .fmt(f)
    }
}

pub fn roll_result(rolls: impl IntoIterator<Item = Roll>, mastery_dice: Dice) -> Roll {
    let mut current_max = Roll::Disaster(1);
    let mut any_mastery_crit = false;
    for (index, roll) in rolls.into_iter().enumerate() {
        let this_roll_mastery_crit = index < mastery_dice.into() && roll.is_perfect();
        if this_roll_mastery_crit && any_mastery_crit {
            tracing::info!("Multi crit from mastery dice");
            current_max = Roll::MultiCritical;
            break;
        }
        any_mastery_crit |= this_roll_mastery_crit;
        tracing::info!(
            index,
            ?roll,
            any_mastery_crit,
            this_roll_mastery_crit,
            "roll"
        );
        // need to check at least two rolls to see if it's a multicrit
        if any_mastery_crit && !this_roll_mastery_crit && roll.is_perfect() {
            tracing::info!("Multi crit");
            current_max = Roll::MultiCritical;
            break;
        } else if this_roll_mastery_crit || (current_max.is_perfect() && roll.is_perfect()) {
            tracing::info!("crit");
            current_max = Roll::Critical;
            if !this_roll_mastery_crit {
                break;
            }
        } else {
            current_max = current_max.max(roll);
        }
    }
    current_max
}

pub fn replace_rolls(
    rolls: impl IntoIterator<Item = Roll>,
    roll_replacements: &[(Roll, Roll)],
) -> Vec<Roll> {
    rolls
        .into_iter()
        .map(|roll| roll.replace_roll(roll_replacements))
        .collect()
}
pub fn roll_replacements(
    fives_count_as_sixes: bool,
    fours_count_as_ones: bool,
) -> Vec<(Roll, Roll)> {
    let mut roll_replacements = vec![];
    if fives_count_as_sixes {
        roll_replacements.push((Roll::Messy(5), Roll::Perfect(5)))
    }
    if fours_count_as_ones {
        roll_replacements.push((Roll::Messy(4), Roll::Grim(4)))
    }
    roll_replacements
}
pub struct RollDistribution {
    dist: Uniform<u8>,
}
impl RollDistribution {
    pub fn new() -> Self {
        Self {
            dist: Uniform::new_inclusive(1, 6),
        }
    }
    pub fn roll_n<'a, R: Rng + ?Sized>(
        &'a self,
        rng: &'a mut R,
        num_dice: Dice,
    ) -> impl Iterator<Item = Roll> + 'a {
        self.sample_iter(rng).take(num_dice.dice as usize)
    }
}
impl Distribution<Roll> for RollDistribution {
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> Roll {
        let roll = self.dist.sample(rng);
        roll.try_into()
            .expect("RollDistribution will only produce values in the acceptable range")
    }
}

#[derive(Copy, Clone)]
pub enum Thorn {
    Cut(u8),
    None(u8),
}
impl Thorn {
    pub fn is_cut(self) -> bool {
        matches!(self, Thorn::Cut(_))
    }
    pub fn as_number(self) -> u8 {
        match self {
            Thorn::Cut(r) | Thorn::None(r) => r,
        }
    }
}

pub struct ThornDistribution {
    dist: Uniform<u8>,
}
impl ThornDistribution {
    pub fn new() -> Self {
        Self {
            dist: Uniform::new_inclusive(1, 8),
        }
    }
    pub fn roll_n<'a, R: Rng + ?Sized>(
        &'a self,
        rng: &'a mut R,
        num_thorns: Thorns,
    ) -> impl Iterator<Item = Thorn> + 'a {
        self.sample_iter(rng).take(num_thorns.thorns as usize)
    }
}
impl Distribution<Thorn> for ThornDistribution {
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> Thorn {
        let roll = self.dist.sample(rng);
        match roll {
            r @ 1..=6 => Thorn::None(r),
            r @ (7 | 8) => Thorn::Cut(r),
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, Copy, Deserialize, Serialize)]
pub struct Pool {
    dice: Dice,
}
impl Pool {
    pub fn new(dice: Dice) -> Self {
        Self { dice }
    }
    pub fn dice(&self) -> Dice {
        self.dice
    }
    pub fn set_dice(&mut self, new_dice: Dice) {
        self.dice = new_dice;
    }
    pub fn roll(&mut self, rolls: &RollDistribution) -> Vec<Roll> {
        let mut rng = thread_rng();
        self.roll_rng(&mut rng, rolls)
    }
    pub fn roll_rng<R: Rng + ?Sized>(
        &mut self,
        rng: &mut R,
        rolls: &RollDistribution,
    ) -> Vec<Roll> {
        let rolls: Vec<_> = rolls
            .roll_n(rng, self.dice)
            .inspect(|roll| {
                if roll.is_grim() {
                    self.dice -= 1;
                }
            })
            .collect();
        rolls
    }
}

#[cfg(test)]
mod tests {
    use crate::rolls::{replace_rolls, roll_replacements, Dice};

    use super::{roll_result, Roll};

    #[test]
    fn test_roll_results() {
        let inputs = [
            (vec![Roll::Perfect(6)], Roll::Perfect(6)),
            (
                vec![Roll::Messy(5), Roll::Messy(5), Roll::Grim(1)],
                Roll::Messy(5),
            ),
            (
                vec![Roll::Perfect(6), Roll::Perfect(6), Roll::Grim(1)],
                Roll::Critical,
            ),
        ];
        for (input, expected) in inputs {
            assert_eq!(roll_result(input, Dice::default()), expected);
        }
    }

    #[test]
    fn mastery() {
        let inputs = [
            (vec![Roll::Perfect(6)], Roll::Critical),
            (
                vec![Roll::Perfect(6), Roll::Perfect(6)],
                Roll::MultiCritical,
            ),
            (vec![Roll::Grim(1), Roll::Perfect(6)], Roll::Perfect(6)),
            (
                vec![Roll::Grim(1), Roll::Perfect(6), Roll::Perfect(6)],
                Roll::Critical,
            ),
            (
                vec![Roll::Perfect(6), Roll::Perfect(6), Roll::Grim(1)],
                Roll::MultiCritical,
            ),
        ];
        for (input, expected) in inputs {
            assert_eq!(roll_result(input, Dice::from(1)), expected);
        }
        let inputs = [
            (
                vec![Roll::Messy(5), Roll::Perfect(6)],
                Roll::Critical,
            ),
            (
                vec![Roll::Perfect(6), Roll::Grim(1), Roll::Perfect(6)],
                Roll::MultiCritical,
            ),
            (
                vec![Roll::Perfect(6), Roll::Grim(1), Roll::Perfect(6)],
                Roll::MultiCritical,
            ),
        ];
        for (input, expected) in inputs {
            assert_eq!(roll_result(input, Dice::from(2)), expected);
        }
        let inputs = [
            (
                vec![Roll::Perfect(6), Roll::Grim(1), Roll::Perfect(6)],
                Roll::MultiCritical,
            ),
            (
                vec![Roll::Perfect(6), Roll::Grim(1), Roll::Messy(5), Roll::Perfect(6)],
                Roll::MultiCritical,
            ),
        ];
        for (input, expected) in inputs {
            assert_eq!(roll_result(input, Dice::from(3)), expected);
        }
    }
    #[test]
    fn fives_count_as_sixes() {
        let inputs = [
            (vec![Roll::Messy(5)], Roll::Perfect(5)),
            (vec![Roll::Messy(5), Roll::Messy(5)], Roll::Critical),
            (vec![Roll::Perfect(6), Roll::Messy(5)], Roll::Critical),
        ];
        for (input, expected) in inputs {
            let input = replace_rolls(input, &[(Roll::Messy(5), Roll::Perfect(5))]);
            assert_eq!(roll_result(input, Dice::default()), expected);
        }
    }
    #[test]
    fn mastery_and_fives_count_as_sixes() {
        let inputs = [
            (vec![Roll::Messy(5)], Roll::Critical),
            (vec![Roll::Messy(5), Roll::Perfect(6)], Roll::MultiCritical),
        ];
        for (input, expected) in inputs {
            let input = replace_rolls(input, &[(Roll::Messy(5), Roll::Perfect(5))]);
            assert_eq!(roll_result(input, Dice::from(1)), expected);
        }
    }
    #[test]
    fn fives_count_as_sixes_and_fours_count_as_ones() {
        let inputs = [
            (vec![Roll::Messy(4)], Roll::Grim(4)),
            (vec![Roll::Messy(4), Roll::Grim(1)], Roll::Grim(4)),
            (vec![Roll::Messy(4), Roll::Messy(5)], Roll::Perfect(5)),
        ];
        for (input, expected) in inputs {
            let input = replace_rolls(input, &roll_replacements(true, true));
            assert_eq!(roll_result(input, Dice::default()), expected);
        }
    }
}
