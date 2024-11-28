use tracing::{info, instrument};

use crate::commands::pool::SetValue;
use crate::{Context, Error};

/// Entrypoint for interacting with suspense.
#[poise::command(
    slash_command,
    prefix_command,
    aliases("s"),
    subcommand_required,
    subcommands("up", "down", "set", "check")
)]
pub async fn suspense(_: Context<'_>) -> Result<(), Error> {
    Ok(())
}

/// Manually change the amount of suspense.
#[poise::command(prefix_command, slash_command)]
#[instrument(skip(ctx), fields(channel=?ctx.channel_id(), user=ctx.author().name))]
pub async fn set(
    ctx: Context<'_>,
    challenge: Option<String>,
    #[description = "Add like `+1`, subtract like `-2`, or set like `6`"] suspense: SetValue,
) -> Result<(), Error> {
    info!("Received command: suspense set");
    ctx.say("").await?;
    Ok(())
}

/// Add to the current amount of suspense.
#[poise::command(prefix_command, slash_command)]
#[instrument(skip(ctx), fields(channel=?ctx.channel_id(), user=ctx.author().name))]
pub async fn up(
    ctx: Context<'_>,
    challenge: Option<String>,
    #[description = "Add like `+1`, subtract like `-2`, or set like `6`. Defaults to 1, or 2 if `challenge` is provided."] suspense: Option<SetValue>,
) -> Result<(), Error> {
    info!("Received command: suspense up");
    ctx.say("").await?;
    Ok(())
}

/// Subtract from the current amount of suspense.
#[poise::command(prefix_command, slash_command)]
#[instrument(skip(ctx), fields(channel=?ctx.channel_id(), user=ctx.author().name))]
pub async fn down(
    ctx: Context<'_>,
    challenge: Option<String>,
    #[description = "Add like `+1`, subtract like `-2`, or set like `6`. Defaults to 1."] suspense: Option<SetValue>,
) -> Result<(), Error> {
    info!("Received command: suspense down");
    ctx.say("").await?;
    Ok(())
}

/// Check the current amount of suspense.
#[poise::command(prefix_command, slash_command)]
#[instrument(skip(ctx), fields(channel=?ctx.channel_id(), user=ctx.author().name))]
pub async fn check(
    ctx: Context<'_>,
) -> Result<(), Error> {
    info!("Received command: suspense check");
    ctx.say("").await?;
    Ok(())
}