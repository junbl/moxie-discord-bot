//! This module contains each of the commands that the bot supports.
use std::{future::ready, str::FromStr};

use poise::CreateReply;
use roll::Dice;
use serenity::{
    all::{
        ArgumentConvert, CacheHttp, ChannelId, ComponentInteractionCollector,
        CreateInteractionResponseMessage, GuildId,
    },
    FutureExt,
};
use thiserror::Error;
use tracing::info;

use crate::{
    pools_in_database::{PoolId, PoolInDb},
    rolls::Pool,
    Context, Error,
};
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
struct ButtonInteraction<B> {
    action: B,
    pool_id: PoolId,
}
impl<B> ButtonInteraction<B> {
    fn new(action: B, pool: PoolId) -> Self {
        Self {
            action,
            pool_id: pool,
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
        let (action, pool_id) = s
            .split_once('/')
            .ok_or_else(|| format!("Failed to parse input (no `/`): {s}"))?;
        let action = action.parse()?;
        let pool_id = pool_id.parse()?;

        Ok(Self { action, pool_id })
    }
}
impl<B> std::fmt::Display for ButtonInteraction<B>
where
    for<'a> &'a B: Into<&'static str>,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let action: &str = (&self.action).into();
        let id = &self.pool_id;
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

pub fn needs_to_handle_buttons(reply: &poise::CreateReply) -> bool {
    reply
        .components
        .as_ref()
        .is_some_and(|components| !components.is_empty())
}

pub trait ButtonAction {
    async fn handle(self, ctx: &Context<'_>, pool: &PoolInDb) -> Result<CreateReply, Error>;
}
pub async fn handle_buttons<B>(ctx: &Context<'_>, pool: &PoolInDb) -> Result<(), Error>
where
    B: ButtonAction + FromStr,
    B::Err: std::error::Error + Send + Sync + 'static,
{
    ctx.defer().await?;
    let pool_id = pool.id;
    while let Some(mci) = ComponentInteractionCollector::new(ctx.serenity_context())
        .timeout(std::time::Duration::from_secs(120))
        .filter(move |mci| {
            mci.data
                .custom_id
                .parse::<ButtonInteraction<B>>()
                .is_ok_and(|pbi| pbi.pool_id == pool_id)
        })
        .await
    {
        tracing::debug!(?mci, ctx_id = ctx.id(), "Got interaction");

        let action = mci
            .data
            .custom_id
            .parse::<ButtonInteraction<B>>()
            .expect("Custom ID parsed in filter")
            .action;
        let message = action.handle(ctx, pool).await?;
        // mci.create_response(ctx, serenity::all::CreateInteractionResponse::Acknowledge)
        mci.create_response(
            ctx,
            serenity::all::CreateInteractionResponse::Message(
                CreateInteractionResponseMessage::new()
                    .content(message.content.unwrap_or_default())
                    .components(message.components.unwrap_or_default()),
            ),
        )
        .await?;
    }
    Ok(())
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
