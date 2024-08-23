use nom::error::Error as NomError;
use nom::Finish;
use rand::thread_rng;
use std::fmt::Write;

use crate::commands::pool::{rolls_str, thorns_str};
use crate::rolls::{
    replace_rolls, roll_replacements, roll_result, Roll, RollDistribution, Thorn, ThornDistribution,
};
use crate::{Context, Error};

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
            Ok((_remaining, rollexpr)) => Ok(rollexpr),
            Err(NomError { input, code }) => Err(NomError {
                input: input.to_string(),
                code,
            }),
        }
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
    #[description = "A roll expression, like `2d` or `3d1t`"] dice: RollExpr,
    #[description = "One of your dice will crit on a 6. Does not add +1d."] mastery: Option<bool>,
    #[description = "Treats rolls of 5 as 6"] fives_count_as_sixes: Option<bool>,
    #[description = "Treats rolls of 4 as 1"] fours_count_as_ones: Option<bool>,
    #[description = "Treats 5s as 6s and 4s as 1s (same as the individual options)"]
    maximum_drama: Option<bool>,
    #[description = "Treats rolls of 1 as 4"] ones_count_as_fours: Option<bool>,
    #[description = "Treats 5s as 6s and 1s as 4s (same as the individual options)"]
    really_good_at_this: Option<bool>,
) -> Result<(), Error> {
    let (rolls, thorns) = dice.roll(&ctx.data().roll_dist, &ctx.data().thorn_dist);
    let mastery = mastery.unwrap_or_default();
    let maximum_drama = maximum_drama.unwrap_or_default();
    let really_good_at_this = really_good_at_this.unwrap_or_default();
    let fives_count_as_sixes =
        maximum_drama || really_good_at_this || fives_count_as_sixes.unwrap_or_default();
    let fours_count_as_ones = maximum_drama || fours_count_as_ones.unwrap_or_default();
    let ones_count_as_fours = really_good_at_this || ones_count_as_fours.unwrap_or_default();

    let rolls = replace_rolls(
        rolls,
        &roll_replacements(
            fives_count_as_sixes,
            fours_count_as_ones,
            ones_count_as_fours,
        ),
    );

    let roll_str = rolls_str(&rolls, mastery);
    let mut message = format!("# {roll_str}");
    if !thorns.is_empty() {
        write!(message, " • {}", thorns_str(&thorns)).unwrap()
    }

    let roll = roll_result(rolls, mastery);
    let final_roll = thorns.into_iter().fold(roll, Roll::cut);
    if roll != final_roll {
        write!(message, "\n### `{roll}`, cut to...").unwrap();
    }
    write!(message, "\n# `{final_roll}`").unwrap();

    ctx.say(message).await?;

    Ok(())
}
