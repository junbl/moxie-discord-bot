use derive_setters::Setters;
use nom::error::Error as NomError;
use nom::Finish;
use poise::{CreateReply, ReplyHandle};
use rand::rng;
use rand_distr::Distribution;
use serenity::all::{
    ButtonStyle, ComponentInteraction, CreateActionRow, CreateButton, CreateEmbed,
    CreateInteractionResponseMessage, UserId,
};
use std::boxed::Box;
use std::fmt::Write;
use std::str::FromStr;
use strum::{EnumString, IntoStaticStr};
use tracing::instrument;

use crate::commands::{handle_buttons, ButtonInteraction};
use crate::commands::{rolls_str, thorns_str};
use crate::pools_in_database::{PoolId, PoolInDb};
use crate::rolls::{
    replace_rolls, roll_replacements, roll_result, Roll, RollDistribution, Thorn, ThornDistribution,
};
use crate::{write_s, Context, Error};

use super::pool::{delete_message, reset_message, roll_inner};
use super::{
    get_rolls_from_message, interaction_reponse_message, ButtonHandler, ButtonHandlerFuture,
    InteractionTarget,
};

/// An expression representing a roll of the dice.
#[derive(Debug, Clone, Copy, PartialEq)]
struct RollExpr {
    /// The number of six-sided dice to be rolled.
    dice: Dice,
    /// The number of eight-sided thorns to be rolled.
    thorns: Thorns,
    /// The number of mastery dice to add (as in the fighter's core talent).
    mastery: Dice,
}
impl RollExpr {
    fn new(dice: impl Into<Dice>, thorns: impl Into<Thorns>, mastery: impl Into<Dice>) -> Self {
        Self {
            dice: dice.into(),
            thorns: thorns.into(),
            mastery: mastery.into(),
        }
    }
    fn roll(
        self,
        roll_dist: &RollDistribution,
        thorn_dist: &ThornDistribution,
    ) -> (Vec<Roll>, Vec<Thorn>) {
        let mut rng = rng();
        let rolls = roll_dist
            .roll_n(&mut rng, self.dice + self.mastery)
            .collect();
        let thorns = thorn_dist.roll_n(&mut rng, self.thorns).collect();
        (rolls, thorns)
    }
    fn is_empty(&self) -> bool {
        let Self {
            dice,
            thorns,
            mastery,
        } = self;
        dice.is_empty() && thorns.is_empty() && mastery.is_empty()
    }
}
impl FromStr for RollExpr {
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
        let Self {
            dice,
            thorns,
            mastery,
        } = self;
        write!(f, "{}", *dice + *mastery)?;
        if !thorns.is_empty() {
            write!(f, "{}", thorns)?;
        }
        Ok(())
    }
}

/// Represents a number of dice.
///
/// Comes with logic for parsing and formatting.
#[derive(
    Debug, Clone, Copy, Default, PartialEq, PartialOrd, serde::Deserialize, serde::Serialize,
)]
#[serde(transparent)]
pub struct Dice {
    pub dice: u8,
}
impl Dice {
    pub fn is_empty(&self) -> bool {
        self.dice == 0
    }
}
impl From<u8> for Dice {
    fn from(dice: u8) -> Dice {
        Dice { dice }
    }
}
impl From<Dice> for usize {
    fn from(dice: Dice) -> usize {
        dice.dice.into()
    }
}
impl FromStr for Dice {
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
impl std::ops::AddAssign for Dice {
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs;
    }
}
impl std::ops::SubAssign for Dice {
    fn sub_assign(&mut self, rhs: Self) {
        *self = *self - rhs;
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
impl FromStr for Thorns {
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
    use nom::sequence::{delimited, preceded, terminated};
    use nom::{IResult, Parser};

    pub fn parse_dice_expression(dice_expr: &str) -> IResult<&str, Dice> {
        let (remaining, dice) = all_consuming(preceded(
            opt(tag("+")),
            terminated(u8, opt(preceded(multispace0, tag("d")))),
        ))
        .parse(dice_expr)?;
        Ok((remaining, Dice { dice }))
    }
    pub fn parse_thorns_expression(thorns_expr: &str) -> IResult<&str, Thorns> {
        let (remaining, thorns) = all_consuming(preceded(
            opt(tag("+")),
            terminated(u8, opt(preceded(multispace0, tag("t")))),
        ))
        .parse(thorns_expr)?;
        Ok((remaining, Thorns { thorns }))
    }
    pub fn parse_roll_expression(roll_expr: &str) -> IResult<&str, RollExpr> {
        let optional_plus_before =
            |p| preceded(delimited(multispace0, opt(tag("+")), multispace0), p);
        assert!(optional_plus_before(opt(terminated(u8, tag("m"))))
            .parse("")
            .is_ok());
        let (remaining, (dice, thorns, mastery)) = all_consuming((
            optional_plus_before(opt(terminated(u8, tag("d")))),
            optional_plus_before(opt(terminated(u8, tag("t")))),
            optional_plus_before(opt(terminated(u8, tag("m")))),
        ))
        .parse(roll_expr.trim())?;
        Ok((
            remaining,
            RollExpr::new(
                dice.unwrap_or_default(),
                thorns.unwrap_or_default(),
                mastery.unwrap_or_default(),
            ),
        ))
    }
    #[cfg(test)]
    mod tests {
        use crate::commands::roll::{Dice, RollExpr, Thorns};

        #[test]
        fn parse_roll_expressions() {
            let good = [
                ("1d", RollExpr::new(1, 0, 0)),
                ("1d2t", RollExpr::new(1, 2, 0)),
                ("1d 2t", RollExpr::new(1, 2, 0)),
                ("1d    \n\t   2t", RollExpr::new(1, 2, 0)),
                ("1d+2t", RollExpr::new(1, 2, 0)),
                ("1d + 2t", RollExpr::new(1, 2, 0)),
                ("2m", RollExpr::new(0, 0, 2)),
                ("1t", RollExpr::new(0, 1, 0)),
                ("100d255t", RollExpr::new(100, 255, 0)),
                ("0d0t", RollExpr::new(0, 0, 0)),
                ("0d0t0m", RollExpr::new(0, 0, 0)),
                ("1d2t3m", RollExpr::new(1, 2, 3)),
                ("", RollExpr::new(0, 0, 0)),
            ];
            for (input, expected) in good {
                match input.parse::<RollExpr>() {
                    Ok(result) => assert_eq!(result, expected, "{input}"),
                    Err(error) => panic!("failed parsing: {input} ({error})"),
                }
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
#[instrument(skip(ctx), fields(channel=?ctx.channel_id(), user=ctx.author().name))]
pub async fn roll(
    ctx: Context<'_>,
    #[description = "A roll expression, like `2d` or `3d1t`"] mut dice: RollExpr,
    #[description = "One of your dice will crit on a 6. Does not add 1d."] mastery: Option<bool>,
    #[description = "The first n dice of your roll are mastery dice"] mastery_dice: Option<Dice>,
    #[description = "Adds the specified number of mastery dice to your roll"]
    plus_mastery_dice: Option<Dice>,
    #[description = "Treats 5s as 6s and 4s as 1s"] wild: Option<bool>,
    #[description = "Prints out the given name with the roll to show what this roll is for"]
    name: Option<String>,
    #[description = "Prints out that this was rolled with potency"] potency: Option<bool>,
) -> Result<(), Error> {
    // update the mastery dice in the roll expression
    let replace_with_mastery_dice = mastery_dice
        .or(mastery.map(|m| Dice::from(if m { 1 } else { 0 })))
        .unwrap_or_default();
    if replace_with_mastery_dice > dice.dice {
        return Err(format!(
            "Got request to treat more dice as mastery dice than were in the pool: \
                `{replace_with_mastery_dice}` > `{d}`\nDid you mean to use `plus_mastery_dice`?",
            d = dice.dice,
        )
        .into());
    }
    dice.dice -= replace_with_mastery_dice;
    dice.mastery += replace_with_mastery_dice;
    dice.mastery += plus_mastery_dice.unwrap_or_default();

    if dice.is_empty() {
        return Err("*a lonely wind gusts across an empty table*".into());
    }

    tracing::info!(rollexpr=?dice, "rolling");
    let (rolls, thorns) = dice.roll(&ctx.data().roll_dist, &ctx.data().thorn_dist);

    let wild = wild.unwrap_or_default();
    let rolls = replace_rolls(rolls, &roll_replacements(wild, wild));
    let roll_id = RollId::new();
    let message_builder = RollOutcomeMessageBuilder::new(&rolls)
        .username(&ctx)
        .thorns(thorns)
        .mastery(dice.mastery)
        .roll_name(name)
        .roll_buttons(roll_id)
        .potency(potency.unwrap_or_default());
    let message = message_builder.clone().finish();

    let needs_to_handle_buttons = super::needs_to_handle_buttons(&message);
    let reply_handle = ctx.send(message).await?;
    if needs_to_handle_buttons {
        handle_buttons::<RollButtonAction>(ctx, (reply_handle, roll_id, message_builder)).await?;
    }

    Ok(())
}

#[derive(Default, Setters, Clone)]
#[must_use]
pub struct RollOutcomeMessageBuilder<'a> {
    rolls: &'a [Roll],
    #[setters(skip)] // custom setter by Context
    user_id: Option<serenity::all::UserId>,
    #[setters(into)]
    thorns: Option<Vec<Thorn>>,
    mastery: Dice,
    #[setters(into)]
    roll_name: Option<String>,
    #[setters(into)]
    pool_name: Option<String>,
    #[setters(into)]
    pool_remaining: Option<Dice>,
    #[setters(into)]
    pool_size_pre_roll: Option<Dice>,
    #[setters(into)]
    roll_buttons: Option<RollId>,
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
                self.rolls.len() as u8 - self.mastery.dice,
                self.thorns.as_ref().map(Vec::len).unwrap_or_default() as u8,
                self.mastery,
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

        // where to put the assists
        message.push(MARKER);

        let mut components = None;
        if let Some(pool_remaining) = self.pool_remaining {
            write_s!(
                message,
                "\n### `{}` → `{}`",
                self.pool_size_pre_roll
                    .unwrap_or(Dice::from(self.rolls.len() as u8)),
                pool_remaining,
            );

            if let Some(pool) = self.pool_buttons {
                if pool_remaining.is_empty() {
                    write_s!(message, " | Pool depleted!");
                    components
                        .get_or_insert_with(Vec::new)
                        .push(CreateActionRow::Buttons(vec![
                            CreateButton::new(ButtonInteraction::new(
                                PoolButtonAction::Delete,
                                pool,
                            ))
                            .label(PoolButtonAction::Delete)
                            .style(ButtonStyle::Danger),
                            CreateButton::new(ButtonInteraction::new(
                                PoolButtonAction::Reset,
                                pool,
                            ))
                            .label(PoolButtonAction::Reset)
                            .style(ButtonStyle::Primary),
                        ]));
                }
            }
        }
        if let Some(roll_id) = self.roll_buttons {
            components
                .get_or_insert_with(Vec::new)
                .push(CreateActionRow::Buttons(vec![CreateButton::new(
                    ButtonInteraction::new(RollButtonAction::Assist, roll_id),
                )
                .label(RollButtonAction::Assist)
                .style(ButtonStyle::Primary)]));
        }

        let embed = (!self.hide_outcome).then(|| {
            let all_rolls = self.rolls.iter().copied();
            roll_result_embed(all_rolls, thorns, self.mastery)
        });

        let mut message = CreateReply::default().content(message);
        if let Some(embed) = embed {
            message = message.embed(embed);
        }
        if let Some(components) = components {
            message = message.components(components);
        }
        message
    }
}

/// Zero-width space used to determine where to insert new assists when editing a message.
pub const MARKER: char = '\u{200B}';

pub fn assist(user_id: UserId, roll: Roll, sent_message: &mut String) -> Result<(), Error> {
    let assist_result = rolls_str(&[roll], Dice::default());
    let assist_message = format!("\n<@{user_id}> assisted! \n# {assist_result}");
    let idx = sent_message
        .find(MARKER)
        .ok_or("Message missing replacement marker to add assists!")?;
    sent_message.insert_str(idx, &assist_message);
    Ok(())
}

pub fn roll_result_embed(
    rolls: impl IntoIterator<Item = Roll>,
    thorns: impl IntoIterator<Item = Thorn>,
    mastery: Dice,
) -> CreateEmbed {
    let mut final_result_message = String::new();

    let roll = roll_result(rolls, mastery);
    let final_roll = thorns.into_iter().fold(roll, Roll::cut);
    if roll != final_roll {
        write_s!(final_result_message, "### **{roll}**, cut to...\n");
    }
    write_s!(final_result_message, "\n# **{final_roll}**");
    let color = match final_roll {
        Roll::Disaster(_) => 0xff6673,
        Roll::Grim(_) => 0xa2a3a6,
        Roll::Messy(_) => 0xffc966,
        // Roll::Perfect(_) => 0xa9d274,
        Roll::Perfect(_) => 0x629740,
        Roll::Critical => 0x75ade6,
        Roll::MultiCritical => 0xbf94e6,
    };
    CreateEmbed::default()
        .description(final_result_message)
        .color(color)
}

/// The different options for what a button on a pool message can do.
///
/// Note that the serialize options need be unique between this and any other implementors of
/// [`ButtonHandler`] because all the button interactions go into the same pipe and have to get
/// sorted out using this.
#[derive(Debug, EnumString, IntoStaticStr)]
pub enum PoolButtonAction {
    #[strum(serialize = "d")]
    Delete,
    #[strum(serialize = "r")]
    Reset,
    #[strum(serialize = "o")]
    Roll,
}
impl From<PoolButtonAction> for String {
    fn from(value: PoolButtonAction) -> Self {
        format!("{value:?}")
    }
}
impl ButtonHandler for PoolButtonAction {
    type Target<'a> = &'a PoolInDb;
    fn handle<'a: 'b, 'b>(
        self,
        ctx: Context<'a>,
        _mci: &'b ComponentInteraction,
        target: Self::Target<'a>,
    ) -> ButtonHandlerFuture<'b> {
        Box::pin(async move {
            let pool = target;
            let pools = &ctx.data().pools;
            match self {
                PoolButtonAction::Delete => {
                    let deleted_pool = pool.delete(pools).await?;
                    let message = delete_message(&pool.name, deleted_pool);
                    Ok(CreateReply::default().content(message))
                }
                PoolButtonAction::Reset => {
                    let num_dice = pool.reset(pools).await?;
                    let message = reset_message(&pool.name, num_dice);
                    Ok(CreateReply::default().content(message))
                }
                PoolButtonAction::Roll => {
                    let mut pool = pool.sync(ctx.data().pools.conn()).await?;
                    roll_inner(&ctx, &mut pool, None, None, None, None).await
                }
            }
            .map(interaction_reponse_message)
            .map(serenity::all::CreateInteractionResponse::Message)
        })
    }
}

impl<'a> InteractionTarget for &'a PoolInDb {
    fn id(&self) -> super::InteractionId {
        self.id.into()
    }
}

/// The different options for what a button on a message can do.
///
/// Note that the serialize options need be unique between this and any other implementors of
/// [`ButtonHandler`] because all the button interactions go into the same pipe and have to get
/// sorted out using this.
#[derive(Debug, EnumString, IntoStaticStr)]
pub enum RollButtonAction {
    #[strum(serialize = "a")]
    Assist,
}
impl From<RollButtonAction> for String {
    fn from(value: RollButtonAction) -> Self {
        format!("{value:?}")
    }
}
impl ButtonHandler for RollButtonAction {
    type Target<'a> = (ReplyHandle<'a>, RollId, RollOutcomeMessageBuilder<'a>);
    fn handle<'a: 'b, 'b>(
        self,
        ctx: Context<'a>,
        mci: &'b ComponentInteraction,
        _target: Self::Target<'a>,
    ) -> ButtonHandlerFuture<'b> {
        let rolls = &ctx.data().roll_dist;
        let user_id = mci.user.id;
        let mut sent_message = mci.message.clone();
        Box::pin(async move {
            match self {
                RollButtonAction::Assist => {
                    let roll = {
                        let mut rng = rng();
                        rolls.sample(&mut rng)
                    };
                    let (rolls, thorns, mastery_dice, assists) =
                        get_rolls_from_message(&sent_message.content);
                    tracing::info!(
                        message = sent_message.content,
                        ?rolls,
                        ?thorns,
                        ?mastery_dice,
                        ?assists,
                        "Parsed rolls from message"
                    );

                    assist(user_id, roll, &mut sent_message.content)?;

                    let final_result = roll_result_embed(
                        rolls
                            .iter()
                            .copied()
                            .chain(assists)
                            .chain(std::iter::once(roll)),
                        thorns,
                        mastery_dice,
                    );

                    // reply_handle.edit(ctx, new_message).await?;

                    Ok(serenity::all::CreateInteractionResponse::UpdateMessage(
                        CreateInteractionResponseMessage::new()
                            .content(sent_message.content)
                            .components(
                                sent_message
                                    .components
                                    .into_iter()
                                    .map(|component| {
                                        CreateActionRow::Buttons(
                                            component
                                                .components
                                                .into_iter()
                                                .filter_map(|button| match button {
                                                    serenity::all::ActionRowComponent::Button(
                                                        button,
                                                    ) => Some(button.into()),
                                                    _ => None,
                                                })
                                                .collect(),
                                        )
                                    })
                                    .collect(),
                            )
                            .embed(final_result),
                    ))
                }
            }
        })
    }
}

impl<'a> InteractionTarget for (ReplyHandle<'a>, RollId, RollOutcomeMessageBuilder<'a>) {
    fn id(&self) -> super::InteractionId {
        self.1.into()
    }
}

#[derive(PartialEq, Copy, Clone, Debug, derive_more::Display)]
pub struct RollId(uuid::Uuid);
impl RollId {
    pub fn new() -> Self {
        Self(uuid::Uuid::new_v4())
    }
}

impl FromStr for RollId {
    type Err = uuid::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        uuid::Uuid::try_parse(s).map(Self)
    }
}
