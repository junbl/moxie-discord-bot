//! This module contains each of the commands that the bot supports.
use std::future::ready;

use roll::Dice;
use serenity::{
    all::{ArgumentConvert, CacheHttp, ChannelId, GuildId},
    FutureExt,
};
use thiserror::Error;
use tracing::info;

use crate::{rolls::Pool, Context, Error};
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
) -> Result<(), Error> {
    info!("Got command: quickpool");
    let mut pool = Pool::new(num_dice);
    let rolls = pool.roll(&ctx.data().roll_dist);

    let message = crate::commands::roll::RollOutcomeMessageBuilder::new(&rolls)
        .pool_remaining(pool.dice())
        .hide_outcome(true)
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
