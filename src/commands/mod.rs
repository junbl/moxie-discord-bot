//! This module contains each of the commands that the bot supports.
use std::{
    future::{ready, Future},
    pin::Pin,
    str::FromStr,
};

use character::CharacterId;
use derive_more::{Display, From};
use itertools::Itertools;
use poise::CreateReply;
use roll::{Dice, RollId};
use serenity::{
    all::{
        ArgumentConvert, CacheHttp, ChannelId, ComponentInteraction, ComponentInteractionCollector,
        CreateInteractionResponse, CreateInteractionResponseMessage, GuildId,
    },
    FutureExt,
};
use thiserror::Error;
use tracing::info;

use crate::{
    database::PoolId,
    rolls::{Pool, Roll, Thorn},
    Context, Error,
};
pub mod character;
pub mod pool;
pub mod roll;
pub mod suspense;

#[derive(Debug, Clone, Copy)]
pub enum Scope {
    Server(GuildId),
    Channel(ChannelId),
    // User
}
#[derive(Debug, Error)]
pub enum ScopeParseError {
    #[error("couldn't parse input: {0}")]
    ParseError(String),
    #[error("no channel id found")]
    NoChannelId,
    #[error("no server id found")]
    NoServerId,
}
impl ArgumentConvert for Scope {
    type Err = ScopeParseError;

    #[must_use]
    #[allow(clippy::type_complexity, clippy::type_repetition_in_bounds)]
    fn convert<'life0, 'async_trait>(
        _ctx: impl 'async_trait + CacheHttp,
        guild_id: Option<GuildId>,
        channel_id: Option<ChannelId>,
        s: &'life0 str,
    ) -> ::core::pin::Pin<
        Box<
            dyn ::core::future::Future<Output = Result<Self, Self::Err>>
                + ::core::marker::Send
                + 'async_trait,
        >,
    >
    where
        'life0: 'async_trait,
        Self: 'async_trait,
    {
        ready(match s {
            "server" => guild_id.ok_or(Self::Err::NoServerId).map(Self::Server),
            "channel" => channel_id.ok_or(Self::Err::NoChannelId).map(Self::Channel),
            _ => Err(Self::Err::ParseError(s.to_string())),
        })
        .boxed()
    }
}

/// A struct that defines a particular button clicked for a particular pool.
///
/// Note that any `B` should implement [`ButtonAction`] and have a unique serialization. This gets
/// turned into a string "{action}/{pool}" which then gets parsed back into which button was
/// pressed.
#[derive(Debug)]
struct ButtonInteraction<B> {
    action: B,
    id: InteractionId,
}
impl<B> ButtonInteraction<B> {
    fn new(action: B, id: impl Into<InteractionId>) -> Self {
        Self {
            action,
            id: id.into(),
        }
    }
}
impl<B> FromStr for ButtonInteraction<B>
where
    B: FromStr,
    B::Err: std::error::Error + Send + Sync + 'static,
{
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (action, id) = s
            .split_once('/')
            .ok_or_else(|| format!("Failed to parse input (no `/`): {s}"))?;
        let action = action.parse()?;
        let id: InteractionId = id.parse()?;

        Ok(Self::new(action, id))
    }
}
impl<B> std::fmt::Display for ButtonInteraction<B>
where
    for<'a> &'a B: Into<&'static str>,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let action: &str = (&self.action).into();
        let id = &self.id;
        write!(f, "{action}/{id}")
    }
}
impl<B> From<ButtonInteraction<B>> for String
where
    for<'a> &'a B: Into<&'static str>,
{
    fn from(pbi: ButtonInteraction<B>) -> Self {
        pbi.to_string()
    }
}

#[derive(PartialEq, Clone, Copy, From, Debug, Display)]
pub enum InteractionId {
    Pool(PoolId),
    Roll(RollId),
    Character(CharacterId),
}
impl FromStr for InteractionId {
    type Err = InteractionIdParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        s.parse::<PoolId>()
            .map(InteractionId::Pool)
            .map_err(InteractionIdParseError::Pool)
            .or_else(|_| {
                s.parse::<RollId>()
                    .map(InteractionId::Roll)
                    .map_err(InteractionIdParseError::Roll)
            })
            .or_else(|_| {
                s.parse::<CharacterId>()
                    .map(InteractionId::Character)
                    .map_err(InteractionIdParseError::Character)
            })
    }
}

#[derive(Debug, thiserror::Error)]
pub enum InteractionIdParseError {
    #[error("{0}")]
    Pool(<PoolId as FromStr>::Err),
    #[error("{0}")]
    Roll(<RollId as FromStr>::Err),
    #[error("{0}")]
    Character(<CharacterId as FromStr>::Err),
}

pub fn needs_to_handle_buttons(reply: &poise::CreateReply) -> bool {
    reply
        .components
        .as_ref()
        .is_some_and(|components| !components.is_empty())
}

/// A trait for enums that represent different button options on a response.
///
/// This defines what action should be taken when that button is pressed.
pub trait ButtonHandler {
    type Target<'a>: Clone + InteractionTarget
    where
        Self: 'a;
    // We can't use RPITIT here because of <https://github.com/rust-lang/rust/issues/100013>
    fn handle<'a: 'b, 'b>(
        self,
        ctx: Context<'a>,
        mci: &'b ComponentInteraction,
        target: Self::Target<'a>,
    ) -> ButtonHandlerFuture<'b>;
}
/// Alias for return type of [`ButtonHandler::handle`] bc it's long as hell thanks to the RPITIT
/// limitation.
pub type ButtonHandlerFuture<'a> =
    Pin<Box<dyn Future<Output = Result<Option<CreateInteractionResponse>, Error>> + 'a + Send>>;

/// A trait implemented by [`ButtonHandler::Target`] to define what that associated type needs to
/// do to be used in [`handle_buttons`].
pub trait InteractionTarget {
    /// Get the unique ID for this interaction used to define it in a [`ButtonInteraction`].
    fn id(&self) -> InteractionId;
}
/// Listens for "Interactions" (clicks on buttons in messages) and responds to them as appropriate.
pub async fn handle_buttons<'a, B>(ctx: Context<'a>, target: B::Target<'a>) -> Result<(), Error>
where
    B: ButtonHandler + FromStr + std::fmt::Debug,
    B::Err: std::error::Error + Send + Sync + 'static,
{
    let id = target.id();
    info!(target=?id, "Looking for id");
    while let Some(mci) = ComponentInteractionCollector::new(ctx.serenity_context())
        .timeout(std::time::Duration::from_secs(120))
        .filter(move |mci| {
            let custom_id = &mci.data.custom_id;
            info!(?custom_id, target = ?id, "Got interaction");
            custom_id
                .parse::<ButtonInteraction<B>>()
                .is_ok_and(|bi| bi.id == id)
        })
        .await
    {
        info!(?mci, ctx_id = ctx.id(), "Got matching interaction");

        let action = mci
            .data
            .custom_id
            .parse::<ButtonInteraction<B>>()
            .expect("Custom ID parsed in filter")
            .action;

        let response = action.handle(ctx, &mci, target.clone()).await?;
        info!(?mci, ctx_id = ctx.id(), ?response, "Interaction response");
        if let Some(response) = response {
            mci.create_response(ctx, response).await?;
        }
        // else {
        //     mci.create_response(ctx, serenity::all::CreateInteractionResponse::Acknowledge)
        //         .await?;
        // }
    }
    Ok(())
}

fn interaction_reponse_message(reply: CreateReply) -> CreateInteractionResponseMessage {
    CreateInteractionResponseMessage::new()
        .content(reply.content.unwrap_or_default())
        .components(reply.components.unwrap_or_default())
}

/// Health check
#[poise::command(slash_command)]
pub async fn hello(ctx: Context<'_>) -> Result<(), Error> {
    info!("Received command: hello");
    ctx.say("<3").await?;
    Ok(())
}

/// Rolls a one-off pool without storing it.
#[poise::command(slash_command)]
pub async fn quickpool(
    ctx: Context<'_>,
    #[description = "Number of dice in the pool"] num_dice: Dice,
    #[description = "Show the outcome of the roll of the dice in the pool, like for a power pool"]
    show_outcome: Option<bool>,
) -> Result<(), Error> {
    info!("Got command: quickpool");
    let mut pool = Pool::new(num_dice);
    let rolls = pool.roll(&ctx.data().roll_dist, None);

    let message = crate::commands::roll::RollOutcomeMessageBuilder::new(&rolls)
        .pool_remaining(pool.dice())
        .hide_outcome(!show_outcome.unwrap_or_default())
        .finish();
    ctx.send(message).await?;
    Ok(())
}

/// Print out an empty line to signify a break in the scene.
#[poise::command(slash_command, prefix_command)]
pub async fn scenebreak(ctx: Context<'_>) -> Result<(), crate::Error> {
    info!("Got command: scenebreak");
    ctx.say("```\n \n```").await?;
    Ok(())
}

/// See command help.
#[poise::command(slash_command)]
pub async fn help(
    ctx: Context<'_>,
    #[description = "Specific command to show help about"] command: Option<String>,
) -> Result<(), Error> {
    info!("Got command: help");
    let config = poise::builtins::HelpConfiguration {
        extra_text_at_bottom: "\
            Type /help command for more info on a command.
            You can edit your message to the bot and the bot will edit its response.",
        ..Default::default()
    };
    poise::builtins::help(ctx, command.as_deref(), config).await?;
    Ok(())
}

macro_rules! emoji {
    ($emoji_const:ident) => {
        if std::env::var("SHUTTLE_PROJECT_NAME").is_ok_and(|var| var.contains("-staging")) {
            staging_emoji::$emoji_const
        } else {
            prod_emoji::$emoji_const
        }
    };
}

mod prod_emoji {
    pub const D61: &str = "<:d61:1270888983051636777>";
    pub const D62: &str = "<:d62:1270889005373984899>";
    pub const D63: &str = "<:d63:1270889037514670142>";
    pub const D64: &str = "<:d64:1270889188190978199>";
    pub const D64_GRIM: &str = "<:d64_grim:1331067368444661881>";
    pub const D65: &str = "<:d65:1270889051871776809>";
    pub const D65_PERFECT: &str = "<:d65_perfect:1270928629286567976>";
    pub const D66: &str = "<:d66:1270889063628537910>";

    pub const D65_PERFECT_MASTERY: &str = "<:d65_perfect_mastery:1331067597738741821>";
    pub const D64_GRIM_MASTERY: &str = "<:d64_grim_mastery:1331067565694386247>";
    pub const D61_MASTERY: &str = "<:d61_mastery:1331067540151079024>";
    pub const D62_MASTERY: &str = "<:d62_mastery:1331067517929783336>";
    pub const D63_MASTERY: &str = "<:d63_mastery:1331067487340593192>";
    pub const D64_MASTERY: &str = "<:d64_mastery:1331067466805416058>";
    pub const D65_MASTERY: &str = "<:d65_mastery:1331067427471233106>";
    pub const D66_MASTERY: &str = "<:d66_mastery:1331067398442192946>";

    pub const D88: &str = "<:d88:1270888678700355584>";
    pub const D87: &str = "<:d87:1270888640054038599>";
    pub const D86: &str = "<:d86:1270815442394677389>";
    pub const D85: &str = "<:d85:1270815474309140612>";
    pub const D84: &str = "<:d84:1270815484412956724>";
    pub const D83: &str = "<:d83:1270815503912271983>";
    pub const D82: &str = "<:d82:1270815516671344660>";
    pub const D81: &str = "<:d81:1270888657426845766>";
}

mod staging_emoji {

    pub const D61: &str = "<:d61:1330587892707233945>";
    pub const D62: &str = "<:d62:1330588022965534760>";
    pub const D63: &str = "<:d63:1330588035968143381>";
    pub const D64: &str = "<:d64:1330588042800664719>";
    pub const D64_GRIM: &str = "<:d64_grim:1331070199629353080>";
    pub const D65: &str = "<:d65:1330588051973345280>";
    pub const D65_PERFECT: &str = "<:d65:1330588051973345280>";
    pub const D66: &str = "<:d66:1330588061762846822>";

    pub const D65_PERFECT_MASTERY: &str = "<:d65_perfect_mastery:1331121313645072517>";
    pub const D64_GRIM_MASTERY: &str = "<:d64_grim_mastery:1331121334972977202>";
    pub const D61_MASTERY: &str = "<:d61_mastery:1331121254970687579>";
    pub const D62_MASTERY: &str = "<:d62_mastery:1331121237711126528>";
    pub const D63_MASTERY: &str = "<:d63_mastery:1331121219898052688>";
    pub const D64_MASTERY: &str = "<:d64_mastery:1331121199073198170>";
    pub const D65_MASTERY: &str = "<:d65_mastery:1331120913764319325>";
    pub const D66_MASTERY: &str = "<:d66_mastery:1331120843916316673>";

    pub const D88: &str = "<:d88:1330588239282704457>";
    pub const D87: &str = "<:d87:1330588226238550119>";
    pub const D86: &str = "<:d86:1330588215148675102>";
    pub const D85: &str = "<:d85:1330588201726906501>";
    pub const D84: &str = "<:d84:1330588187353153627>";
    pub const D83: &str = "<:d83:1330588115697537095>";
    pub const D82: &str = "<:d82:1330588084244578334>";
    pub const D81: &str = "<:d81:1330588072793866322>";
}

fn get_die_emoji(roll: Roll, mastery: bool) -> &'static str {
    match roll {
        Roll::Grim(1) if mastery => emoji!(D61_MASTERY),
        Roll::Grim(2) if mastery => emoji!(D62_MASTERY),
        Roll::Grim(3) if mastery => emoji!(D63_MASTERY),
        Roll::Grim(4) if mastery => emoji!(D64_GRIM_MASTERY),
        Roll::Messy(4) if mastery => emoji!(D64_MASTERY),
        Roll::Messy(5) if mastery => emoji!(D65_MASTERY),
        Roll::Perfect(5) if mastery => emoji!(D65_PERFECT_MASTERY),
        Roll::Perfect(6) if mastery => emoji!(D66_MASTERY),
        Roll::Grim(1) => emoji!(D61),
        Roll::Grim(2) => emoji!(D62),
        Roll::Grim(3) => emoji!(D63),
        Roll::Grim(4) => emoji!(D64_GRIM),
        Roll::Messy(4) => emoji!(D64),
        Roll::Messy(5) => emoji!(D65),
        Roll::Perfect(5) => emoji!(D65_PERFECT),
        Roll::Perfect(6) => emoji!(D66),
        other => get_die_emoji(
            other
                .as_number()
                .try_into()
                .expect("Rolls must only be in the 1-6 range"),
            mastery,
        ),
    }
}

fn get_thorn_emoji(thorn: Thorn) -> &'static str {
    [
        (8, emoji!(D88)),
        (7, emoji!(D87)),
        (6, emoji!(D86)),
        (5, emoji!(D85)),
        (4, emoji!(D84)),
        (3, emoji!(D83)),
        (2, emoji!(D82)),
        (1, emoji!(D81)),
    ]
    .into_iter()
    .find_map(|(roll_number, emoji)| (thorn.as_number() == roll_number).then_some(emoji))
    .expect("Thorns must only be in the 1-8 range")
}

pub fn rolls_str(rolls: &[Roll], mastery_dice: Dice) -> String {
    rolls
        .iter()
        .copied()
        .enumerate()
        .map(|(i, r)| {
            let mastery = i < mastery_dice.dice as usize;
            get_die_emoji(r, mastery)
        })
        .join(" ")
}
pub fn thorns_str(thorns: &[Thorn]) -> String {
    thorns.iter().copied().map(get_thorn_emoji).join(" ")
}

/// Parses out the roll emoji from the message back into the original rolls.
pub fn get_rolls_from_message(message: &str) -> (Vec<Roll>, Vec<Thorn>, Dice, Vec<Roll>) {
    let d6_re = lazy_regex::regex!(
        r"(?<assist>assisted!\s*)?<:d6(?<result>\d)(?<replace>_(?:grim|perfect))?(?<mastery>_mastery)?:\d+>"
    );
    let d8_re = lazy_regex::regex!(r"<:d8(?<result>\d):\d+>");
    let mut mastery = Dice::from(0);
    let get_result = |cap: &lazy_regex::Captures| {
        cap.name("result")
            .expect("must be present if regex matches")
            .as_str()
            .parse::<u8>()
            .expect("single digit won't fail parsing into u8")
    };
    let (rolls, assists) = d6_re
        .captures_iter(message)
        .map(|cap| {
            let result = get_result(&cap);
            if cap.name("mastery").is_some() {
                mastery += 1;
            }
            let roll = match cap.name("replace").map(|m| m.as_str()) {
                Some("_grim") => Roll::Grim(result),
                Some("_perfect") => Roll::Perfect(result),
                _ => Roll::try_from(result).expect("values from emoji won't be out of range"),
            };
            (roll, cap.name("assist").is_some())
        })
        .partition_map(|(roll, was_assist)| {
            if was_assist {
                itertools::Either::Right(roll)
            } else {
                itertools::Either::Left(roll)
            }
        });

    let thorns = d8_re
        .captures_iter(message)
        .map(|cap| {
            let result = get_result(&cap);
            Thorn::try_from(result).expect("values from emoji won't be out of range")
        })
        .collect();

    (rolls, thorns, mastery, assists)
}

#[cfg(test)]
mod tests {
    use crate::rolls::{Roll, Thorn};

    use super::get_rolls_from_message;
    use super::roll::{Dice, RollOutcomeMessageBuilder};

    #[test]
    fn emoji_back_and_forth() {
        let rolls = [
            Roll::Grim(1),
            Roll::Grim(2),
            Roll::Grim(3),
            Roll::Grim(4),
            Roll::Messy(4),
            Roll::Messy(5),
            Roll::Perfect(5),
            Roll::Perfect(6),
        ];
        let thorns = (1..8)
            .map(Thorn::try_from)
            .collect::<Result<Vec<_>, _>>()
            .unwrap();

        let message = RollOutcomeMessageBuilder::new(&rolls)
            .thorns(thorns.clone())
            .finish();
        let (parsed_rolls, parsed_thorns, parsed_mastery_dice, assists) =
            get_rolls_from_message(&message.content.unwrap());
        assert_eq!(&rolls, parsed_rolls.as_slice());
        assert_eq!(thorns, parsed_thorns);
        assert_eq!(Dice::from(0), parsed_mastery_dice);
        assert!(assists.is_empty());

        let mastery_dice = Dice::from(8);
        let message = RollOutcomeMessageBuilder::new(&rolls)
            .thorns(thorns.clone())
            .mastery(mastery_dice)
            .finish();
        let (parsed_rolls, parsed_thorns, parsed_mastery_dice, assists) =
            get_rolls_from_message(&message.content.unwrap());
        assert_eq!(&rolls, parsed_rolls.as_slice());
        assert_eq!(thorns, parsed_thorns);
        assert_eq!(mastery_dice, parsed_mastery_dice);
        assert!(assists.is_empty());
    }
}
