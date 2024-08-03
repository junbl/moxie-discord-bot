use std::future::ready;

use itertools::Itertools;
use serenity::{
    all::{ArgumentConvert, CacheHttp, ChannelId, GuildId},
    FutureExt,
};
use thiserror::Error;

use crate::{
    rolls::{Pool, Roll},
    Context, Error,
};

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

pub fn print_pool_results(rolls: &[Roll], pool: Pool) -> String {
    format!(
        "rolls: {}\nremaining dice: {}",
        rolls.iter().map(ToString::to_string).join(" "),
        pool.dice()
    )
}

/// Entrypoint for interacting with pools.
#[poise::command(
    prefix_command,
    slash_command,
    subcommands("new", "roll", "reset", "delete", "adddice", "check")
)]
pub async fn pool(_: Context<'_>) -> Result<(), Error> {
    Ok(())
}
#[poise::command(prefix_command, slash_command)]
async fn new(
    ctx: Context<'_>,
    #[description = "Number of dice to put in the pool"] num_dice: u8,
    #[description = "Name of the pool"] name: String,
    #[description = "Where to store this pool (either channel or server, defaults to channel)"]
    scope: Option<Scope>,
) -> Result<(), Error> {
    let scope = scope.unwrap_or_else(|| Scope::Channel(ctx.channel_id()));
    let message = format!("Created new pool \"{name}\" with {num_dice} dice!");
    ctx.data().pools.new_pool(scope, name, num_dice).await?;
    ctx.say(message).await?;
    Ok(())
}

#[poise::command(prefix_command, slash_command)]
async fn roll(
    ctx: Context<'_>,
    #[description = "Name of the pool"] pool_name: String,
    #[description = "Where to look for the pool"] scope: Option<Scope>,
) -> Result<(), Error> {
    let scopes_to_check = if let Some(scope) = scope {
        vec![scope]
    } else {
        [Scope::Channel(ctx.channel_id())]
            .into_iter()
            .chain(ctx.guild_id().map(Scope::Server))
            .collect()
    };
    let pool_name = pool_name.as_str();
    let mut rolls = None;
    for scope in scopes_to_check {
        if let Some(p) = ctx
            .data()
            .pools
            .roll_pool(scope, pool_name, &ctx.data().rolls)
            .await
            .ok()
            .flatten()
        {
            let _ = rolls.insert(p);
            break;
        }
    }
    if let Some((pool, rolls)) = rolls {
        ctx.say(print_pool_results(&rolls, pool.pool)).await?;
        Ok(())
    } else {
        Err(anyhow::anyhow!("Pool {pool_name} not found!").into())
    }
}
#[poise::command(prefix_command, slash_command)]
async fn reset(
    ctx: Context<'_>,
    #[description = "Name of the pool"] pool: String,
) -> Result<(), Error> {
    ctx.say("world!").await?;
    Ok(())
}
#[poise::command(prefix_command, slash_command)]
async fn delete(
    ctx: Context<'_>,
    #[description = "Name of the pool"] pool: String,
) -> Result<(), Error> {
    ctx.say("world!").await?;
    Ok(())
}
#[poise::command(prefix_command, slash_command)]
async fn adddice(
    ctx: Context<'_>,
    #[description = "Name of the pool"] pool: String,
) -> Result<(), Error> {
    ctx.say("world!").await?;
    Ok(())
}
#[poise::command(prefix_command, slash_command)]
async fn check(
    ctx: Context<'_>,
    #[description = "Name of the pool"] pool: String,
) -> Result<(), Error> {
    ctx.say("world!").await?;
    Ok(())
}
