use derive_setters::Setters;
use nom::error::Error as NomError;
use nom::Finish;
use rand::thread_rng;
use std::fmt::Write;

use crate::commands::fmt_dice;
use crate::commands::pool::{rolls_str, thorns_str};
use crate::rolls::{
    replace_rolls, roll_replacements, roll_result, Roll, RollDistribution, Thorn, ThornDistribution,
};
use crate::{write_s, Context, Error};

#[derive(Debug, Clone, Copy, PartialEq)]
struct RollExpr {
    dice: u8,
    thorns: u8,
}
impl RollExpr {
    fn roll(
        self,
        roll_dist: &RollDistribution,
        thorn_dist: &ThornDistribution,
    ) -> (Vec<Roll>, Vec<Thorn>) {
        let mut rng = thread_rng();
        let rolls = roll_dist.roll_n(&mut rng, self.dice as usize).collect();
        let thorns = thorn_dist.roll_n(&mut rng, self.thorns as usize).collect();
        (rolls, thorns)
    }
}
impl std::str::FromStr for RollExpr {
    type Err = NomError<String>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match parse::parse_roll_expression(s).finish() {
            Ok((_remaining, roll_expr)) => Ok(roll_expr),
            Err(NomError { input, code }) => Err(NomError {
                input: input.to_string(),
                code,
            }),
        }
    }
}
impl std::fmt::Display for RollExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}d", self.dice)?;
        if self.thorns > 0 {
            write!(f, "{}t", self.thorns)?;
        }
        Ok(())
    }
}

mod parse {
    use super::RollExpr;
    use nom::bytes::complete::tag;
    use nom::character::complete::u8;
    use nom::combinator::{all_consuming, opt};
    use nom::sequence::{terminated, tuple};
    use nom::IResult;

    pub fn parse_roll_expression(roll_expr: &str) -> IResult<&str, RollExpr> {
        let (remaining, (dice, thorns)) = all_consuming(tuple((
            terminated(u8, tag("d")),
            opt(terminated(u8, tag("t"))),
        )))(roll_expr)?;
        Ok((
            remaining,
            RollExpr {
                dice,
                thorns: thorns.unwrap_or_default(),
            },
        ))
    }
    #[cfg(test)]
    mod tests {
        use crate::commands::roll::RollExpr;

        #[test]
        fn parse_roll_expressions() {
            let good = [
                ("1d", RollExpr { dice: 1, thorns: 0 }),
                ("1d2t", RollExpr { dice: 1, thorns: 2 }),
                (
                    "100d255t",
                    RollExpr {
                        dice: 100,
                        thorns: 255,
                    },
                ),
                ("0d0t", RollExpr { dice: 0, thorns: 0 }),
            ];
            for (input, expected) in good {
                assert_eq!(input.parse::<RollExpr>().unwrap(), expected);
            }
            let bad = ["1000000000d", "1d10000000000000t", "-1d", "1.5d", "1d5.6"];
            for input in bad {
                let res = input.parse::<RollExpr>();
                assert!(res.is_err(), "{input}: {res:?}");
            }
        }
    }
}

/// Roll dice!
///
/// Use an expression like `/roll 3d2t`.
#[poise::command(slash_command, prefix_command)]
#[allow(clippy::too_many_arguments)]
pub async fn roll(
    ctx: Context<'_>,
    #[description = "A roll expression, like `2d` or `3d1t`"] mut dice: RollExpr,
    #[description = "One of your dice will crit on a 6. Does not add +1d."] mastery: Option<bool>,
    #[description = "One of your dice will crit on a 6. Adds +1d."] mastery_plus_1d: Option<bool>,
    #[description = "Treats rolls of 5 as 6"] fives_count_as_sixes: Option<bool>,
    #[description = "Treats rolls of 4 as 1"] fours_count_as_ones: Option<bool>,
    #[description = "Treats 5s as 6s and 4s as 1s (same as the individual options)"] wild: Option<
        bool,
    >,
    #[description = "Prints out the given name with the roll to show what this roll is for."] name: Option<String>,
    #[description = "Prints out that this was rolled with potency. Does not modify the roll."]
    potency: Option<bool>,
) -> Result<(), Error> {
    let mastery_plus_1d = mastery_plus_1d.unwrap_or_default();
    if mastery_plus_1d {
        dice.dice += 1;
    }
    let (rolls, thorns) = dice.roll(&ctx.data().roll_dist, &ctx.data().thorn_dist);

    let mastery = mastery.unwrap_or_default() || mastery_plus_1d;
    let wild = wild.unwrap_or_default();
    let fives_count_as_sixes = wild || fives_count_as_sixes.unwrap_or_default();
    let fours_count_as_ones = wild || fours_count_as_ones.unwrap_or_default();

    let rolls = replace_rolls(
        rolls,
        &roll_replacements(fives_count_as_sixes, fours_count_as_ones),
    );
    let message = RollOutcomeMessageBuilder::new(&rolls)
        .username(&ctx)
        .thorns(thorns)
        .mastery(mastery)
        .roll_name(name)
        .potency(potency.unwrap_or_default())
        .finish();

    ctx.say(message).await?;

    Ok(())
}

#[derive(Default, Setters)]
#[must_use]
pub struct RollOutcomeMessageBuilder<'a> {
    #[setters(skip)] // required
    rolls: &'a [Roll],
    #[setters(skip)] // custom setter by Context
    user_id: Option<serenity::all::UserId>,
    #[setters(into)]
    thorns: Option<Vec<Thorn>>,
    mastery: bool,
    #[setters(into)]
    roll_name: Option<String>,
    #[setters(into)]
    pool_name: Option<String>,
    #[setters(into)]
    pool_remaining: Option<u8>,
    potency: bool,
    hide_outcome: bool,
}
impl<'a> RollOutcomeMessageBuilder<'a> {
    pub fn new(rolls: &'a [Roll]) -> Self {
        Self {
            rolls,
            ..Default::default()
        }
    }
    pub fn username(mut self, ctx: &'a Context<'_>) -> Self {
        let _ = self.user_id.insert(ctx.author().id);
        self
    }
    pub fn finish(self) -> String {
        let mut message = String::new();

        if let Some(user_id) = self.user_id {
            write_s!(message, "<@{user_id}> rolled");
        } else {
            write_s!(message, "Rolled");
        }

        if let Some(pool_name) = self.pool_name {
            write_s!(message, " pool `{pool_name}`");
        } else {
            let roll_expr = RollExpr {
                dice: self.rolls.len() as u8,
                thorns: self.thorns.as_ref().map(Vec::len).unwrap_or_default() as u8,
            };
            write_s!(message, " {roll_expr}");
        }

        if let Some(ref roll_name) = self.roll_name {
            write_s!(message, " for `{roll_name}`");
        }
        if self.potency {
            write_s!(message, " with **potency**");
        }
        writeln!(message, "!").unwrap();

        if !self.rolls.is_empty() {
            let roll_str = rolls_str(self.rolls, self.mastery);
            write_s!(message, "# {roll_str}");
        }

        if let Some(ref thorns) = self.thorns {
            if !thorns.is_empty() {
                write_s!(message, " • {}", thorns_str(thorns));
            }
        }
        if let Some(pool_remaining) = self.pool_remaining {
            if !self.rolls.is_empty() {
                write_s!(
                    message,
                    "\n### {} → {}",
                    fmt_dice(self.rolls.len() as u8),
                    fmt_dice(pool_remaining),
                );

                if pool_remaining == 0 {
                    write_s!(message, "\n### Pool depleted!");
                }
            }
        }

        if !self.hide_outcome {
            let roll = roll_result(self.rolls.iter().copied(), self.mastery);
            let final_roll = self.thorns.into_iter().flatten().fold(roll, Roll::cut);
            if roll != final_roll {
                write_s!(message, "\n### `{roll}`, cut to...");
            }
            write_s!(message, "\n# `{final_roll}`");
        }

        message
    }
}
