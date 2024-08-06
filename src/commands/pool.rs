//! The `pool` command, which offers operations to create, read, update, and delete diminishing
//! pools.
use std::future::ready;

use itertools::Itertools;
use serenity::futures::{future::Either, stream, Stream, StreamExt, TryStreamExt};
use tracing::{info, instrument};

use crate::{
    commands::{fmt_dice, Scope},
    rolls::{roll_result, Pool, Roll},
    Context, Error,
};

/// Testing subcommands
#[poise::command(slash_command, prefix_command, subcommands("subcommand"))]
pub async fn debug(_: Context<'_>) -> Result<(), crate::Error> {
    Ok(())
}

/// Say hi!
#[poise::command(slash_command, prefix_command)]
#[instrument(skip(ctx), fields(channel=?ctx.channel_id(), user=ctx.author().name))]
pub async fn subcommand(ctx: Context<'_>, arg: String) -> Result<(), crate::Error> {
    info!("Received command: debug subcommand");
    ctx.say(format!("Hi there! {arg}")).await?;
    Ok(())
}

fn get_die_emoji(roll: Roll) -> &'static str {
    [
        (6, "<:d66:1269902496600428554>"),
        (5, "<:d65:1269902489000345620>"),
        (4, "<:d64:1269902479907229767>"),
        (3, "<:d63:1269902465915158623>"),
        (2, "<:d62:1269902452988186735>"),
        (1, "<:d61:1269902426530512978>"),
    ]
    .into_iter()
    .find_map(|(roll_number, emoji)| (roll.as_number() == roll_number).then_some(emoji))
    .expect("Rolls should only be in the 1-6 range")
}

pub fn print_pool_results(rolls: &[Roll], pool: Pool, print_outcome: bool) -> String {
    let remaining = pool.dice();
    let mut msg = format!(
        "# {}\n## dropped: *{}* remaining: *{}*",
        rolls.iter().copied().map(get_die_emoji).join(" "),
        rolls.len() as u8 - remaining,
        remaining,
    );
    if print_outcome {
        use std::fmt::Write;
        write!(msg, "\n{}", roll_result(rolls.iter().copied()).as_number()).unwrap();
    }
    msg
}

/// If the scope argument is not provided, use this to default to
/// [`Scope::Channel`].
fn scope_or_default(opt_scope: Option<Scope>, ctx: &Context) -> Scope {
    opt_scope.unwrap_or_else(|| Scope::Channel(ctx.channel_id()))
}

async fn autocomplete_pool_name<'a>(
    ctx: Context<'a>,
    partial: &'a str,
) -> impl Stream<Item = String> + 'a {
    ctx.data()
        .pools
        .list(Scope::Channel(ctx.channel_id()), partial, Some(32), None)
        .await
        .map_or_else(
            |e| {
                tracing::error!("Error in autocomplete stream: {e}");
                Either::Left(stream::empty())
            },
            Either::Right,
        )
        .filter_map(|pool_res| ready(pool_res.ok().map(|p| p.name)))
}

/// Entrypoint for interacting with pools.
#[poise::command(
    slash_command,
    prefix_command,
    aliases("p"),
    subcommands("new", "roll", "reset", "delete", "set", "check", "list")
)]
pub async fn pool(_: Context<'_>) -> Result<(), Error> {
    Ok(())
}
/// Creates a new pool.
#[poise::command(prefix_command, slash_command)]
#[instrument(skip(ctx), fields(channel=?ctx.channel_id(), user=ctx.author().name))]
pub async fn new(
    ctx: Context<'_>,
    #[description = "Number of dice to put in the pool"] num_dice: u8,
    #[description = "Name of the pool"] name: String,
    #[description = "Storage location - channel or server, default channel"] scope: Option<Scope>,
) -> Result<(), Error> {
    info!("Received command: new");
    let message = format!("Created new pool \"{name}\" with {}!", fmt_dice(num_dice));
    ctx.data()
        .pools
        .create(scope_or_default(scope, &ctx), name, num_dice)
        .await?;
    ctx.say(message).await?;
    Ok(())
}

/// Rolls a pool.
#[poise::command(prefix_command, slash_command)]
#[instrument(skip(ctx), fields(channel=?ctx.channel_id(), user=ctx.author().name))]
pub async fn roll(
    ctx: Context<'_>,
    #[autocomplete = "autocomplete_pool_name"]
    #[description = "Name of the pool"]
    pool_name: String,
    #[description = "Storage location - channel or server, default channel"] scope: Option<Scope>,
) -> Result<(), Error> {
    info!("Received command: roll");
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
            .roll(scope, pool_name, &ctx.data().rolls)
            .await
            .ok()
            .flatten()
        {
            let _ = rolls.insert(p);
            break;
        }
    }
    if let Some((pool, rolls)) = rolls {
        ctx.say(format!(
            "Rolled pool \"{pool_name}\" with {}\n{}",
            fmt_dice(rolls.len() as u8),
            print_pool_results(&rolls, pool.pool, false)
        ))
        .await?;
        Ok(())
    } else {
        Err(anyhow::anyhow!("Pool {pool_name} not found!").into())
    }
}
/// Sets a pool's current dice back to the number of dice it started with.
#[poise::command(prefix_command, slash_command)]
#[instrument(skip(ctx), fields(channel=?ctx.channel_id(), user=ctx.author().name))]
pub async fn reset(
    ctx: Context<'_>,
    #[autocomplete = "autocomplete_pool_name"]
    #[description = "Name of the pool"]
    pool_name: String,
    #[description = "Storage location - channel or server, default channel"] scope: Option<Scope>,
) -> Result<(), Error> {
    info!("Received command: reset");
    let num_dice = ctx
        .data()
        .pools
        .reset(scope_or_default(scope, &ctx), &pool_name)
        .await?;
    ctx.say(format!(
        "Reset pool \"{pool_name}\" back to {}!",
        fmt_dice(num_dice)
    ))
    .await?;
    Ok(())
}
/// Deletes a pool.
#[poise::command(prefix_command, slash_command)]
#[instrument(skip(ctx), fields(channel=?ctx.channel_id(), user=ctx.author().name))]
pub async fn delete(
    ctx: Context<'_>,
    #[autocomplete = "autocomplete_pool_name"]
    #[description = "Name of the pool"]
    pool_name: String,
    #[description = "Storage location - channel or server, default channel"] scope: Option<Scope>,
) -> Result<(), Error> {
    info!("Received command: delete");
    let deleted_pool = ctx
        .data()
        .pools
        .delete(scope_or_default(scope, &ctx), &pool_name)
        .await?;

    ctx.say(format!(
        "Deleted pool \"{pool_name}\" (had {} left)",
        fmt_dice(deleted_pool.dice()),
    ))
    .await?;
    Ok(())
}

/// See the available pools for this channel or server.
#[poise::command(prefix_command, slash_command)]
#[instrument(skip(ctx), fields(channel=?ctx.channel_id(), user=ctx.author().name))]
pub async fn list(
    ctx: Context<'_>,
    #[description = "Name of the pool"] pool_search_string: Option<String>,
    #[description = "Page of results"] page: Option<u64>,
    #[description = "Storage location - channel or server, default channel"] scope: Option<Scope>,
) -> Result<(), Error> {
    info!("Received command: list");
    let page_size = 12;
    let pools = ctx
        .data()
        .pools
        .list(
            scope_or_default(scope, &ctx),
            &pool_search_string.unwrap_or_default(),
            Some(page_size),
            page.map(|p| p * page_size),
        )
        .await?
        .try_collect::<Vec<_>>()
        .await?;
    let max_name_length = pools
        .iter()
        .map(|p| p.name().len())
        .max()
        .unwrap_or_default();
    let width = (max_name_length + 3).max(8);
    let table = pools
        .into_iter()
        .map(|pool| {
            format!(
                "{:width$}{}/{}",
                pool.name(),
                pool.pool.dice(),
                fmt_dice(pool.original_size())
            )
        })
        .join("\n");
    ctx.say(format!("```\n{table}\n```")).await?;
    Ok(())
}
/// Manually change the amount of dice in a pool.
///
/// To set to `n` dice, use `set {pool} n`.
/// To add `n` dice to the pool, use `set {pool} +n`.
/// To remove `n` dice from the pool, use `set {pool} -n`.
#[poise::command(prefix_command, slash_command)]
#[instrument(skip(ctx), fields(channel=?ctx.channel_id(), user=ctx.author().name))]
pub async fn set(
    ctx: Context<'_>,
    #[autocomplete = "autocomplete_pool_name"]
    #[description = "Name of the pool"]
    pool_name: String,
    #[description = "Add like \"+1\", subtract like \"-2\", or set like \"6\""] num_dice: SetValue,
    #[description = "Storage location - channel or server, default channel"] scope: Option<Scope>,
) -> Result<(), Error> {
    info!("Received command: set");
    let new_size = ctx
        .data()
        .pools
        .set(scope_or_default(scope, &ctx), &pool_name, num_dice)
        .await?;
    ctx.say(format!(
        "Pool \"{pool_name}\" now has {}!",
        fmt_dice(new_size),
    ))
    .await?;
    Ok(())
}
/// Checks the current number of dice in a pool without rolling it.
#[poise::command(prefix_command, slash_command)]
#[instrument(skip(ctx), fields(channel=?ctx.channel_id(), user=ctx.author().name))]
pub async fn check(
    ctx: Context<'_>,
    #[autocomplete = "autocomplete_pool_name"]
    #[description = "Name of the pool"]
    pool_name: String,
    #[description = "Storage location - channel or server, default channel"] scope: Option<Scope>,
) -> Result<(), Error> {
    info!("Received command: check");
    let pool = ctx
        .data()
        .pools
        .get(scope_or_default(scope, &ctx), &pool_name)
        .await?;
    ctx.say(format!(
        "Pool \"{}\" currently has {}/{} remaining!",
        pool_name,
        pool.pool.dice(),
        fmt_dice(pool.original_size()),
    ))
    .await?;
    Ok(())
}

/// The type for the `num_dice` argument of the [`set`] command.
#[derive(Debug, PartialEq)]
pub enum SetValue {
    Add(u8),
    Subtract(u8),
    Set(u8),
}
impl std::str::FromStr for SetValue {
    type Err = std::num::ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (variant, value): (fn(_) -> _, _) = if let Some(add) = s.strip_prefix('+') {
            (Self::Add, add)
        } else if let Some(sub) = s.strip_prefix('-') {
            (Self::Subtract, sub)
        } else {
            (Self::Set, s)
        };
        let value = value.parse::<u8>()?;
        Ok(variant(value))
    }
}

#[cfg(test)]
mod tests {
    use super::SetValue;

    #[test]
    fn set_value() {
        let good = [
            ("1", SetValue::Set(1)),
            ("+2", SetValue::Add(2)),
            ("-3", SetValue::Subtract(3)),
            ("+0", SetValue::Add(0)),
            ("-004", SetValue::Subtract(4)),
            ("++1", SetValue::Add(1)),
            ("-+1", SetValue::Subtract(1)),
        ];
        for (val, expected) in good {
            assert_eq!(val.parse::<SetValue>().unwrap(), expected);
        }
        let bad = [
            "+-1", "--12", "+++1", "---1", "-1+", "-1-", "256", "+256", "+1.0",
        ];
        for val in bad {
            let res = val.parse::<SetValue>();
            assert!(res.is_err(), "val: {val}, result: {res:?}");
        }
    }
}
