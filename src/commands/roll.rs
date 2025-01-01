use derive_setters::Setters;
use nom::error::Error as NomError;
use nom::Finish;
use poise::CreateReply;
use rand::thread_rng;
use serenity::all::{
    ButtonStyle, ComponentInteractionCollector, CreateActionRow, CreateButton,
    CreateInteractionResponseMessage,
};
use std::fmt::Write;
use strum::{EnumString, IntoStaticStr};

use crate::commands::pool::{rolls_str, thorns_str};
use crate::pools_in_database::{PoolId, PoolInDb};
use crate::rolls::{
    replace_rolls, roll_replacements, roll_result, Roll, RollDistribution, Thorn, ThornDistribution,
};
use crate::{write_s, Context, Error};

use super::pool::{delete_message, reset_message};

#[derive(Debug, Clone, Copy, PartialEq)]
struct RollExpr {
    dice: Dice,
    thorns: Thorns,
}
impl RollExpr {
    fn new(dice: impl Into<Dice>, thorns: impl Into<Thorns>) -> Self {
        Self {
            dice: dice.into(),
            thorns: thorns.into(),
        }
    }
    fn roll(
        self,
        roll_dist: &RollDistribution,
        thorn_dist: &ThornDistribution,
    ) -> (Vec<Roll>, Vec<Thorn>) {
        let mut rng = thread_rng();
        let rolls = roll_dist.roll_n(&mut rng, self.dice).collect();
        let thorns = thorn_dist.roll_n(&mut rng, self.thorns).collect();
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
        write!(f, "{}", self.dice)?;
        if !self.thorns.is_empty() {
            write!(f, "{}", self.thorns)?;
        }
        Ok(())
    }
}

/// Represents a number of dice.
///
/// Comes with logic for parsing and formatting.
#[derive(Debug, Clone, Copy, PartialEq, serde::Deserialize, serde::Serialize)]
#[serde(transparent)]
pub struct Dice {
    pub dice: u8,
}
impl Dice {
    fn is_empty(&self) -> bool {
        self.dice == 0
    }
}
impl From<u8> for Dice {
    fn from(dice: u8) -> Dice {
        Dice { dice }
    }
}
impl std::str::FromStr for Dice {
    type Err = NomError<String>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match parse::parse_dice_expression(s).finish() {
            Ok((_remaining, roll_expr)) => Ok(roll_expr),
            Err(NomError { input, code }) => Err(NomError {
                input: input.to_string(),
                code,
            }),
        }
    }
}
impl std::fmt::Display for Dice {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}d", self.dice)?;
        Ok(())
    }
}
impl From<Dice> for Option<u8> {
    fn from(value: Dice) -> Self {
        Some(value.dice)
    }
}
impl std::ops::Add for Dice {
    type Output = Dice;

    fn add(self, rhs: Self) -> Self::Output {
        Dice::from(self.dice.saturating_add(rhs.dice))
    }
}
impl std::ops::Sub for Dice {
    type Output = Dice;

    fn sub(self, rhs: Self) -> Self::Output {
        Dice::from(self.dice.saturating_sub(rhs.dice))
    }
}
impl std::ops::SubAssign<u8> for Dice {
    fn sub_assign(&mut self, rhs: u8) {
        *self = *self - Dice::from(rhs);
    }
}
impl std::ops::AddAssign<u8> for Dice {
    fn add_assign(&mut self, rhs: u8) {
        *self = *self + Dice::from(rhs);
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Thorns {
    pub thorns: u8,
}
impl Thorns {
    fn is_empty(&self) -> bool {
        self.thorns == 0
    }
}
impl From<u8> for Thorns {
    fn from(thorns: u8) -> Self {
        Thorns { thorns }
    }
}
impl std::str::FromStr for Thorns {
    type Err = NomError<String>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match parse::parse_thorns_expression(s).finish() {
            Ok((_remaining, expr)) => Ok(expr),
            Err(NomError { input, code }) => Err(NomError {
                input: input.to_string(),
                code,
            }),
        }
    }
}
impl std::fmt::Display for Thorns {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}t", self.thorns)?;
        Ok(())
    }
}
mod parse {
    use super::{Dice, RollExpr, Thorns};
    use nom::bytes::complete::tag;
    use nom::character::complete::{multispace0, u8};
    use nom::combinator::{all_consuming, opt};
    use nom::sequence::{delimited, preceded, terminated, tuple};
    use nom::IResult;

    pub fn parse_dice_expression(dice_expr: &str) -> IResult<&str, Dice> {
        let (remaining, dice) = all_consuming(preceded(
            opt(tag("+")),
            terminated(u8, opt(preceded(multispace0, tag("d")))),
        ))(dice_expr)?;
        Ok((remaining, Dice { dice }))
    }
    pub fn parse_thorns_expression(thorns_expr: &str) -> IResult<&str, Thorns> {
        let (remaining, thorns) = all_consuming(preceded(
            opt(tag("+")),
            terminated(u8, opt(preceded(multispace0, tag("t")))),
        ))(thorns_expr)?;
        Ok((remaining, Thorns { thorns }))
    }
    pub fn parse_roll_expression(roll_expr: &str) -> IResult<&str, RollExpr> {
        let (remaining, (dice, thorns)) = all_consuming(tuple((
            terminated(
                opt(terminated(u8, tag("d"))),
                delimited(multispace0, opt(tag("+")), multispace0),
            ),
            opt(terminated(u8, tag("t"))),
        )))(roll_expr)?;
        Ok((
            remaining,
            RollExpr {
                dice: Dice::from(dice.unwrap_or_default()),
                thorns: Thorns::from(thorns.unwrap_or_default()),
            },
        ))
    }
    #[cfg(test)]
    mod tests {
        use crate::commands::roll::{Dice, RollExpr, Thorns};

        #[test]
        fn parse_roll_expressions() {
            let good = [
                ("1d", RollExpr::new(1, 0)),
                ("1d2t", RollExpr::new(1, 2)),
                ("1d 2t", RollExpr::new(1, 2)),
                ("1d    \n\t   2t", RollExpr::new(1, 2)),
                ("1d+2t", RollExpr::new(1, 2)),
                ("1d + 2t", RollExpr::new(1, 2)),
                ("2t", RollExpr::new(0, 2)),
                ("100d255t", RollExpr::new(100, 255)),
                ("0d0t", RollExpr::new(0, 0)),
                ("", RollExpr::new(0, 0)),
            ];
            for (input, expected) in good {
                assert_eq!(input.parse::<RollExpr>().unwrap(), expected);
            }
            let bad = [
                "1000000000d",
                "1d10000000000000t",
                "-1d",
                "1.5d",
                "1d5.6",
                "1t2d",
                "1d beans 2t",
                "1d-2t",
            ];
            for input in bad {
                let res = input.parse::<RollExpr>();
                assert!(res.is_err(), "{input}: {res:?}");
            }
        }
        #[test]
        fn parse_dice_expressions() {
            let good = [
                ("1d", Dice { dice: 1 }),
                ("3", Dice { dice: 3 }),
                ("4  d", Dice { dice: 4 }),
            ];
            for (input, expected) in good {
                assert_eq!(input.parse::<Dice>().unwrap(), expected);
            }
            let bad = ["1000000000d", "1d1t", "-1d", "1.5d", "1.5dd"];
            for input in bad {
                let res = input.parse::<Dice>();
                assert!(res.is_err(), "{input}: {res:?}");
            }
        }
        #[test]
        fn parse_thorns_expressions() {
            let good = [
                ("1t", Thorns { thorns: 1 }),
                ("3", Thorns { thorns: 3 }),
                ("4  t", Thorns { thorns: 4 }),
            ];
            for (input, expected) in good {
                assert_eq!(input.parse::<Thorns>().unwrap(), expected);
            }
            let bad = ["1000000000t", "1d1t", "-1t", "1.5t", "1.5tt"];
            for input in bad {
                let res = input.parse::<Thorns>();
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

    ctx.send(message).await?;

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
    pool_remaining: Option<Dice>,
    #[setters(into)]
    pool_buttons: Option<PoolId>,
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
    pub fn pool(self, pool: &PoolInDb) -> Self {
        self.pool_remaining(pool.pool.dice())
            .pool_name(pool.name.clone())
            .pool_buttons(pool.id)
    }
    pub fn username(mut self, ctx: &'a Context<'_>) -> Self {
        let _ = self.user_id.insert(ctx.author().id);
        self
    }
    pub fn finish(self) -> CreateReply {
        let mut message = String::new();

        if let Some(user_id) = self.user_id {
            write_s!(message, "<@{user_id}> rolled");
        } else {
            write_s!(message, "Rolled");
        }

        if let Some(pool_name) = self.pool_name {
            write_s!(message, " pool `{pool_name}`");
        } else {
            let roll_expr = RollExpr::new(
                self.rolls.len() as u8,
                self.thorns.as_ref().map(Vec::len).unwrap_or_default() as u8,
            );
            write_s!(message, " `{roll_expr}`");
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

        let thorns = self.thorns.unwrap_or_default();
        if !thorns.is_empty() {
            if !self.rolls.is_empty() {
                write_s!(message, " • ");
            } else {
                write_s!(message, "# ");
            }
            write_s!(message, "{}", thorns_str(&thorns));
        }
        let mut components = None;
        if let Some(pool_remaining) = self.pool_remaining {
            write_s!(
                message,
                "\n### `{}` → `{}`",
                Dice::from(self.rolls.len() as u8),
                pool_remaining,
            );

            if let Some(pool) = self.pool_buttons {
                if pool_remaining.is_empty() {
                    write_s!(message, " | Pool depleted!");
                    components
                        .get_or_insert_with(Vec::new)
                        .push(CreateActionRow::Buttons(vec![
                            CreateButton::new(PoolButtonInteraction::new(
                                ButtonAction::Delete,
                                pool,
                            ))
                            .label(ButtonAction::Delete)
                            .style(ButtonStyle::Danger),
                            CreateButton::new(PoolButtonInteraction::new(
                                ButtonAction::Reset,
                                pool,
                            ))
                            .label(ButtonAction::Reset)
                            .style(ButtonStyle::Primary),
                        ]));
                }
            }
        }

        if !self.hide_outcome {
            let roll = roll_result(self.rolls.iter().copied(), self.mastery);
            let final_roll = thorns.into_iter().fold(roll, Roll::cut);
            if roll != final_roll {
                write_s!(message, "\n### `{roll}`, cut to...");
            }
            write_s!(message, "\n# `{final_roll}`");
        }

        let mut message = CreateReply::default().content(message);
        if let Some(components) = components {
            message = message.components(components);
        }
        message
    }
}

struct PoolButtonInteraction {
    action: ButtonAction,
    pool_id: PoolId,
}
impl PoolButtonInteraction {
    pub fn new(action: ButtonAction, pool: PoolId) -> Self {
        Self {
            action,
            pool_id: pool,
        }
    }
}
impl std::str::FromStr for PoolButtonInteraction {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (action, pool_id) = s
            .split_once('/')
            .ok_or_else(|| format!("Failed to parse input (no `/`): {s}"))?;
        let action = action.parse()?;
        let pool_id = pool_id.parse()?;

        Ok(Self { action, pool_id })
    }
}
impl std::fmt::Display for PoolButtonInteraction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let action: &str = (&self.action).into();
        let id = &self.pool_id;
        write!(f, "{action}/{id}")
    }
}
impl From<PoolButtonInteraction> for String {
    fn from(pbi: PoolButtonInteraction) -> Self {
        pbi.to_string()
    }
}
#[derive(Debug, EnumString, IntoStaticStr)]
enum ButtonAction {
    #[strum(serialize = "d")]
    Delete,
    #[strum(serialize = "r")]
    Reset,
}
impl From<ButtonAction> for String {
    fn from(value: ButtonAction) -> Self {
        format!("{value:?}")
    }
}

pub fn needs_to_handle_buttons(reply: &CreateReply) -> bool {
    reply
        .components
        .as_ref()
        .is_some_and(|components| !components.is_empty())
}

pub async fn handle_buttons(ctx: &Context<'_>, pool: &PoolInDb) -> Result<(), Error> {
    ctx.defer().await?;
    let pool_id = pool.id;
    while let Some(mci) = ComponentInteractionCollector::new(ctx.serenity_context())
        .timeout(std::time::Duration::from_secs(120))
        .filter(move |mci| {
            mci.data
                .custom_id
                .parse::<PoolButtonInteraction>()
                .is_ok_and(|pbi| pbi.pool_id == pool_id)
        })
        .await
    {
        tracing::debug!(?mci, ctx_id = ctx.id(), "Got interaction");
        let pools = &ctx.data().pools;

        let message = match mci
            .data
            .custom_id
            .parse::<PoolButtonInteraction>()
            .expect("Custom ID parsed in filter")
            .action
        {
            ButtonAction::Delete => {
                let deleted_pool = pool.delete(pools).await?;
                delete_message(&pool.name, deleted_pool)
            }
            ButtonAction::Reset => {
                let num_dice = pool.reset(pools).await?;
                reset_message(&pool.name, num_dice)
            }
        };
        // mci.create_response(ctx, serenity::all::CreateInteractionResponse::Acknowledge)
        mci.create_response(
            ctx,
            serenity::all::CreateInteractionResponse::Message(
                CreateInteractionResponseMessage::new().content(message),
            ),
        )
        .await?;
    }
    Ok(())
}
