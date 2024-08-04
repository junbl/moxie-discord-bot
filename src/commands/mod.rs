//! This module contains each of the commands that the bot supports.
use std::future::ready;

use serenity::{
    all::{ArgumentConvert, CacheHttp, ChannelId, GuildId},
    FutureExt,
};
use thiserror::Error;

use crate::{rolls::Pool, Context};
pub mod pool;

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
pub async fn hello(ctx: Context<'_>) -> Result<(), crate::Error> {
    tracing::info!("Received command: hello");
    ctx.say("<3").await?;
    Ok(())
}

/// Rolls a one-off pool without storing it.
#[poise::command(slash_command)]
pub async fn pooln(
    ctx: Context<'_>,
    #[description = "Number of dice in the pool"] num_dice: u8,
) -> Result<(), crate::Error> {
    let mut pool = Pool::new(num_dice);
    let rolls = pool.roll(&ctx.data().rolls);

    ctx.say(crate::commands::pool::print_pool_results(&rolls, pool))
        .await?;
    Ok(())
}

/// Testing subcommands
#[poise::command(slash_command, prefix_command, subcommands("there"))]
pub async fn hi(_: Context<'_>) -> Result<(), crate::Error> {
    Ok(())
}

/// Say hi!
#[poise::command(slash_command, prefix_command, subcommands("there"))]
pub async fn there(ctx: Context<'_>) -> Result<(), crate::Error> {
    ctx.say("Hi there!").await?;
    Ok(())
}
