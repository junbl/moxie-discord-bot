//! The `pool` command, which offers operations to create, read, update, and delete diminishing
//! pools.
use itertools::Itertools;

use crate::{
    commands::Scope,
    rolls::{Pool, Roll},
    Context, Error,
};

pub fn print_pool_results(rolls: &[Roll], pool: Pool) -> String {
    format!(
        "rolls: {}\nremaining dice: {}",
        rolls.iter().map(ToString::to_string).join(" "),
        pool.dice()
    )
}

fn scope_or_default(opt_scope: Option<Scope>, ctx: &Context) -> Scope {
    opt_scope.unwrap_or_else(|| Scope::Channel(ctx.channel_id()))
}

/// Entrypoint for interacting with pools.
#[poise::command(
    prefix_command,
    slash_command,
    subcommands("new", "roll", "reset", "delete", "set", "check")
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
    let message = format!("Created new pool \"{name}\" with {num_dice} dice!");
    ctx.data()
        .pools
        .create(scope_or_default(scope, &ctx), name, num_dice)
        .await?;
    ctx.say(message).await?;
    Ok(())
}

#[poise::command(prefix_command, slash_command)]
async fn roll(
    ctx: Context<'_>,
    #[description = "Name of the pool"] pool_name: String,
    #[description = "Where to look for this pool (either channel or server, defaults to channel)"]
    scope: Option<Scope>,
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
            "Rolled pool \"{pool_name}\" with {} dice\n{}",
            rolls.len(),
            print_pool_results(&rolls, pool.pool)
        ))
        .await?;
        Ok(())
    } else {
        Err(anyhow::anyhow!("Pool {pool_name} not found!").into())
    }
}
#[poise::command(prefix_command, slash_command)]
async fn reset(
    ctx: Context<'_>,
    #[description = "Name of the pool"] pool_name: String,
    #[description = "Where to look for this pool (either channel or server, defaults to channel)"]
    scope: Option<Scope>,
) -> Result<(), Error> {
    ctx.data()
        .pools
        .reset(scope_or_default(scope, &ctx), &pool_name)
        .await?;
    ctx.say("Reset pool \"{pool_name}\" back to {num_dice} dice!")
        .await?;
    Ok(())
}
#[poise::command(prefix_command, slash_command)]
async fn delete(
    ctx: Context<'_>,
    #[description = "Name of the pool"] pool_name: String,
    #[description = "Where to look for this pool (either channel or server, defaults to channel)"]
    scope: Option<Scope>,
) -> Result<(), Error> {
    let deleted_pool = ctx
        .data()
        .pools
        .delete(scope_or_default(scope, &ctx), &pool_name)
        .await?;

    ctx.say(format!(
        "Deleted pool \"{pool_name}\" (had {} dice left)",
        deleted_pool.dice(),
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
#[poise::command(prefix_command, slash_command)]
async fn set(
    ctx: Context<'_>,
    #[description = "Name of the pool"] pool_name: String,
    #[description = "Where to look for this pool (either channel or server, defaults to channel)"]
    scope: Option<Scope>,
    #[description = "Number of dice to set this pool to - can be a number like \"6\", or \
        you can add or subtract with an expression like \"+1\" or \"-2\""]
    num_dice: SetValue,
) -> Result<(), Error> {
    let new_size = ctx
        .data()
        .pools
        .set(scope_or_default(scope, &ctx), &pool_name, num_dice)
        .await?;
    ctx.say(format!("Set pool \"{pool_name}\" to {new_size} dice!"))
        .await?;
    Ok(())
}
#[poise::command(prefix_command, slash_command)]
async fn check(
    ctx: Context<'_>,
    #[description = "Name of the pool"] pool_name: String,
    #[description = "Where to look for this pool (either channel or server, defaults to channel)"]
    scope: Option<Scope>,
) -> Result<(), Error> {
    let pool = ctx
        .data()
        .pools
        .get(scope_or_default(scope, &ctx), &pool_name)
        .await?;
    ctx.say(format!(
        "Pool \"{}\" currently has {}/{} dice remaining!",
        pool_name,
        pool.pool.dice(),
        pool.original_size(),
    ))
    .await?;
    Ok(())
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
        ];
        for (val, expected) in good {
            assert_eq!(val.parse::<SetValue>().unwrap(), expected);
        }
        let bad = [
            "+-1", "--12", "++1", "-+1", "-1+", "-1-", "256", "+256", "+1.0",
        ];
        for val in bad {
            let res = val.parse::<SetValue>();
            assert!(res.is_err(), "{res:?}");
        }
    }
}
