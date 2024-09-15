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

use crate::commands::fmt_dice;
use crate::commands::pool::{rolls_str, thorns_str};
use crate::pools_in_database::{PoolId, PoolInDb};
use crate::rolls::{
    replace_rolls, roll_replacements, roll_result, Roll, RollDistribution, Thorn, ThornDistribution,
};
use crate::{write_s, Context, Error};

use super::pool::{delete_message, reset_message};

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
    pool_remaining: Option<u8>,
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
    pub fn pool(self, pool: PoolInDb) -> Self {
        self.pool_remaining(pool.pool.dice())
            .pool_name(pool.name)
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
                "\n### {} → {}",
                fmt_dice(self.rolls.len() as u8),
                fmt_dice(pool_remaining),
            );

            if let Some(pool) = self.pool_buttons {
                if pool_remaining == 0 {
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

pub async fn handle_buttons(ctx: &Context<'_>, pool: PoolInDb) -> Result<(), Error> {
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
        tracing::warn!(?mci, ctx_id = ctx.id(), "Got interaction");
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
