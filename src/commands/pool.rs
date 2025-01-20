//! The `pool` command, which offers operations to create, read, update, and delete diminishing
//! pools.
use std::{borrow::Cow, future::ready};

use itertools::Itertools;
use poise::CreateReply;
use serenity::{
    all::{ButtonStyle, CreateActionRow, CreateButton},
    futures::{future::Either, stream, Stream, StreamExt, TryStreamExt},
};
use tracing::{info, instrument};

use crate::{
    commands::{
        handle_buttons,
        roll::{Dice, PoolButtonAction, RollOutcomeMessageBuilder, Thorns},
        ButtonInteraction, Scope,
    },
    error::MoxieError,
    pools_in_database::PoolInDb,
    rolls::{Pool, Roll, Thorn},
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

// prod
const D61: &str = "<:d61:1270888983051636777>";
const D62: &str = "<:d62:1270889005373984899>";
const D63: &str = "<:d63:1270889037514670142>";
const D64: &str = "<:d64:1270889188190978199>";
const D65: &str = "<:d65:1270889051871776809>";
const D65_PERFECT: &str = "<:d65_perfect:1270928629286567976>";
const D66: &str = "<:d66:1270889063628537910>";

const D88: &str = "<:d88:1270888678700355584>";
const D87: &str = "<:d87:1270888640054038599>";
const D86: &str = "<:d86:1270815442394677389>";
const D85: &str = "<:d85:1270815474309140612>";
const D84: &str = "<:d84:1270815484412956724>";
const D83: &str = "<:d83:1270815503912271983>";
const D82: &str = "<:d82:1270815516671344660>";
const D81: &str = "<:d81:1270888657426845766>";

// staging
// const D61: &str = "<:d61:1330587892707233945>";
// const D62: &str = "<:d62:1330588022965534760>";
// const D63: &str = "<:d63:1330588035968143381>";
// const D64: &str = "<:d64:1330588042800664719>";
// const D65: &str = "<:d65:1330588051973345280>";
// const D65_PERFECT: &str = "<:d65:1330588051973345280>";
// const D66: &str = "<:d66:1330588061762846822>";

// const D88: &str = "<:d88:1330588239282704457>";
// const D87: &str = "<:d87:1330588226238550119>";
// const D86: &str = "<:d86:1330588215148675102>";
// const D85: &str = "<:d85:1330588201726906501>";
// const D84: &str = "<:d84:1330588187353153627>";
// const D83: &str = "<:d83:1330588115697537095>";
// const D82: &str = "<:d82:1330588084244578334>";
// const D81: &str = "<:d81:1330588072793866322>";

fn get_die_emoji(roll: Roll) -> &'static str {
    match roll {
        Roll::Grim(1) => D61,
        Roll::Grim(2) => D62,
        Roll::Grim(3) => D63,
        Roll::Grim(4) => D64,
        Roll::Messy(1) => D61,
        Roll::Messy(4) => D64,
        Roll::Messy(5) => D65,
        Roll::Perfect(5) => D65_PERFECT,
        Roll::Perfect(6) => D66,
        other => get_die_emoji(
            other
                .as_number()
                .try_into()
                .expect("Rolls must only be in the 1-6 range"),
        ),
    }
}

fn get_thorn_emoji(thorn: Thorn) -> &'static str {
    [
        (8, D88),
        (7, D87),
        (6, D86),
        (5, D85),
        (4, D84),
        (3, D83),
        (2, D82),
        (1, D81),
    ]
    .into_iter()
    .find_map(|(roll_number, emoji)| (thorn.as_number() == roll_number).then_some(emoji))
    .expect("Thorns must only be in the 1-8 range")
}

pub fn rolls_str(rolls: &[Roll], mastery_dice: Dice) -> String {
    let mut emoji_iter = rolls.iter().copied().map(get_die_emoji);
    let mastery_marker = "܀";

    if !mastery_dice.is_empty() {
        emoji_iter
            .enumerate()
            .map(|(i, emoji)| {
                let mut emoji = if i == 0 {
                    Cow::Owned(format!("{mastery_marker}{emoji}"))
                } else {
                    Cow::Borrowed(emoji)
                };
                if i + 1 == usize::from(mastery_dice.dice) {
                    emoji += mastery_marker;
                    emoji += " ";
                }
                emoji
            })
            .join(" ")
    } else {
        emoji_iter.join(" ")
    }
}
pub fn thorns_str(thorns: &[Thorn]) -> String {
    thorns.iter().copied().map(get_thorn_emoji).join(" ")
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
    subcommand_required,
    subcommands(
        "new", "roll", "reset", "delete", "set", "setmax", "check", "list", "droproll"
    )
)]
pub async fn pool(_: Context<'_>) -> Result<(), Error> {
    Ok(())
}
/// Creates a new pool.
#[poise::command(prefix_command, slash_command)]
#[instrument(skip(ctx), fields(channel=?ctx.channel_id(), user=ctx.author().name))]
pub async fn new(
    ctx: Context<'_>,
    #[description = "Number of dice to put in the pool"] num_dice: Dice,
    #[description = "Name of the pool"] name: String,
    #[description = "Storage location - channel or server, default channel"] scope: Option<Scope>,
) -> Result<(), Error> {
    info!("Received command: pool new");
    let message = format!("Created new pool `{name}` with {num_dice}!");
    let pool = ctx
        .data()
        .pools
        .create(scope_or_default(scope, &ctx), name, num_dice)
        .await?;

    let components = vec![CreateActionRow::Buttons(vec![CreateButton::new(
        ButtonInteraction::new(PoolButtonAction::Roll, pool.id),
    )
    .label(PoolButtonAction::Roll)
    .style(ButtonStyle::Primary)])];

    let reply = CreateReply::default()
        .content(message)
        .components(components);
    ctx.send(reply).await?;
    handle_buttons::<PoolButtonAction>(&ctx, &pool).await?;
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
    #[description = "Show the outcome of the roll of the dice in the pool, like for a power pool"]
    show_outcome: Option<bool>,
    #[description = "Number of thorns to add to potentially cut the outcome of a pool. Enables show_outcome."]
    thorns: Option<Thorns>,
    #[description = "Roll this pool with potency. Enables show_outcome."] potency: Option<bool>,
    #[description = "Only roll some dice from the pool. Enables show_outcome."]
    only_roll_some: Option<Dice>,
) -> Result<(), Error> {
    info!("Received command: roll");
    let mut pool = get_pool_try_all_scopes(&ctx, &pool_name, scope).await?;
    let message = roll_inner(
        &ctx,
        &mut pool,
        show_outcome,
        thorns,
        potency,
        only_roll_some,
    )
    .await?;
    send_pool_roll_message(&ctx, message, &pool).await?;
    Ok(())
}
pub async fn roll_inner(
    ctx: &Context<'_>,
    pool: &mut PoolInDb,
    show_outcome: Option<bool>,
    thorns: Option<Thorns>,
    potency: Option<bool>,
    only_roll_some: Option<Dice>,
) -> Result<CreateReply, Error> {
    let pre_roll_size = pool.pool.dice();
    let rolls = pool
        .roll(
            ctx.data().pools.conn(),
            &ctx.data().roll_dist,
            only_roll_some,
        )
        .await?;
    let show_outcome = thorns.is_some()
        || potency.is_some_and(std::convert::identity)
        || only_roll_some.is_some()
        || show_outcome.unwrap_or_default();

    let thorns = thorns.map(|thorns| {
        let mut rng = rand::thread_rng();
        ctx.data().thorn_dist.roll_n(&mut rng, thorns).collect()
    });
    let pool_result_msg = RollOutcomeMessageBuilder::new(&rolls)
        .username(ctx)
        .pool(pool)
        .pool_size_pre_roll(pre_roll_size)
        .hide_outcome(!show_outcome)
        .thorns(thorns)
        .potency(potency.unwrap_or_default());
    Ok(pool_result_msg.finish())
}

/// Sends the given message, handling any interactions if necessary.
pub async fn send_pool_roll_message(
    ctx: &Context<'_>,
    message: CreateReply,
    pool: &PoolInDb,
) -> Result<(), Error> {
    let needs_to_handle_buttons = super::needs_to_handle_buttons(&message);
    ctx.send(message).await?;
    if needs_to_handle_buttons {
        handle_buttons::<PoolButtonAction>(ctx, pool).await
    } else {
        Ok(())
    }
}

async fn get_pool_try_all_scopes(
    ctx: &Context<'_>,
    pool_name: &str,
    scope: Option<Scope>,
) -> Result<PoolInDb, MoxieError> {
    let scopes_to_check = if let Some(scope) = scope {
        vec![scope]
    } else {
        [Scope::Channel(ctx.channel_id())]
            .into_iter()
            .chain(ctx.guild_id().map(Scope::Server))
            .collect()
    };
    let mut pool = Err(MoxieError::PoolNotFound(pool_name.to_string()));
    for scope in scopes_to_check {
        if let Ok(p) = ctx.data().pools.get(scope, pool_name).await {
            pool = Ok(p);
            break;
        }
    }
    pool
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
    ctx.say(reset_message(&pool_name, num_dice)).await?;
    Ok(())
}
pub fn reset_message(pool_name: &str, num_dice: Dice) -> String {
    format!("Reset pool `{pool_name}` back to `{num_dice}`!",)
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
    ctx.say(delete_message(&pool_name, deleted_pool)).await?;
    Ok(())
}
pub fn delete_message(pool_name: &str, deleted_pool: Pool) -> String {
    format!(
        "Deleted pool `{pool_name}` (had `{}` left)",
        deleted_pool.dice(),
    )
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
                pool.original_size()
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
    #[description = "Add like `+1`, subtract like `-2`, or set like `6`"] num_dice: SetValue,
    #[description = "Storage location - channel or server, default channel"] scope: Option<Scope>,
) -> Result<(), Error> {
    info!("Received command: set");
    let (_, message) = set_inner(ctx, &pool_name, num_dice, scope).await?;
    ctx.say(message).await?;
    Ok(())
}
pub async fn set_inner(
    ctx: Context<'_>,
    pool_name: &str,
    num_dice: SetValue,
    scope: Option<Scope>,
) -> Result<(PoolInDb, String), Error> {
    let mut pool = get_pool_try_all_scopes(&ctx, pool_name, scope).await?;
    let starting_size = pool.pool.dice();
    let new_size = ctx.data().pools.set(&mut pool, num_dice).await?;
    let message = format!("Set pool `{pool_name}` `{starting_size}` → `{new_size}`!",);
    Ok((pool, message))
}

/// Manually change the amount of maximum dice in a pool.
///
/// To set to `n` dice, use `set {pool} n`.
/// To add `n` dice to the pool, use `set {pool} +n`.
/// To remove `n` dice from the pool, use `set {pool} -n`.
#[poise::command(prefix_command, slash_command)]
#[instrument(skip(ctx), fields(channel=?ctx.channel_id(), user=ctx.author().name))]
pub async fn setmax(
    ctx: Context<'_>,
    #[autocomplete = "autocomplete_pool_name"]
    #[description = "Name of the pool"]
    pool_name: String,
    #[description = "Add like `+1`, subtract like `-2`, or set like `6`"] num_dice: SetValue,
    #[description = "Set the pool's current size to the new max size"] reset: Option<bool>,
    #[description = "Storage location - channel or server, default channel"] scope: Option<Scope>,
) -> Result<(), Error> {
    info!("Received command: set max");
    let mut pool = get_pool_try_all_scopes(&ctx, &pool_name, scope).await?;
    let starting_size = pool.original_size();
    let new_size = ctx
        .data()
        .pools
        .set_max(&mut pool, num_dice, reset.unwrap_or_default())
        .await?;
    let message =
        format!("Updated max size of pool `{pool_name}` from `{starting_size}` → `{new_size}`!",);
    ctx.say(message).await?;
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
    check_inner(ctx, pool_name, scope).await
}

async fn check_inner(
    ctx: Context<'_>,
    pool_name: String,
    scope: Option<Scope>,
) -> Result<(), Error> {
    info!("Received command: check");
    let pool = ctx
        .data()
        .pools
        .get(scope_or_default(scope, &ctx), &pool_name)
        .await?;
    ctx.say(format!(
        "Pool `{pool_name}` currently has `{}`/`{}` remaining!",
        pool.pool.dice(),
        pool.original_size(),
    ))
    .await?;
    Ok(())
}

/// Drops one or more dice from the pool, then rolls it.
#[poise::command(prefix_command, slash_command)]
#[instrument(skip(ctx), fields(channel=?ctx.channel_id(), user=ctx.author().name))]
#[expect(
    clippy::too_many_arguments,
    reason = "sorry for adding too many features ig"
)]
pub async fn droproll(
    ctx: Context<'_>,
    #[autocomplete = "autocomplete_pool_name"]
    #[description = "Name of the pool"]
    pool_name: String,
    #[description = "Number of dice to drop before rolling - default 1"] num_dice_to_drop: Option<
        Dice,
    >,
    #[description = "Storage location - channel or server, default channel"] scope: Option<Scope>,
    #[description = "Show the outcome of the roll of the dice in the pool"] show_outcome: Option<
        bool,
    >,
    #[description = "Number of thorns to add to potentially cut the outcome of a pool. Enables show_outcome."]
    thorns: Option<Thorns>,
    #[description = "Roll this pool with potency. Enables show_outcome."] potency: Option<bool>,
    #[description = "Only roll some dice from the pool"] only_roll_some: Option<Dice>,
) -> Result<(), Error> {
    let (mut pool, message) = set_inner(
        ctx,
        &pool_name,
        SetValue::Subtract(num_dice_to_drop.unwrap_or(Dice::from(1))),
        scope,
    )
    .await?;
    let roll_message = roll_inner(
        &ctx,
        &mut pool,
        show_outcome,
        thorns,
        potency,
        only_roll_some,
    )
    .await?;
    let message = format!(
        "{message}\n{}",
        roll_message.content.as_deref().unwrap_or_default()
    );
    let message = roll_message.content(message);
    send_pool_roll_message(&ctx, message, &pool).await?;
    Ok(())
}
/// The type for the `num_dice` argument of the [`set`] command.
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum SetValue {
    Add(Dice),
    Subtract(Dice),
    Set(Dice),
}
impl SetValue {
    pub fn apply(self, dice: Dice) -> Dice {
        match self {
            SetValue::Add(add) => dice + add,
            SetValue::Subtract(sub) => dice - sub,
            SetValue::Set(set) => set,
        }
    }
}
impl std::str::FromStr for SetValue {
    type Err = <Dice as std::str::FromStr>::Err;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (variant, value): (fn(_) -> _, _) = if let Some(add) = s.strip_prefix('+') {
            (Self::Add, add)
        } else if let Some(sub) = s.strip_prefix('-') {
            (Self::Subtract, sub)
        } else {
            (Self::Set, s)
        };
        let value = value.parse::<Dice>()?;
        Ok(variant(value))
    }
}

#[cfg(test)]
mod tests {
    use crate::commands::roll::Dice;

    use super::SetValue;

    #[test]
    fn set_value() {
        let good = [
            ("1", SetValue::Set(Dice::from(1))),
            ("1d", SetValue::Set(Dice::from(1))),
            ("+2", SetValue::Add(Dice::from(2))),
            ("+2d", SetValue::Add(Dice::from(2))),
            ("-3", SetValue::Subtract(Dice::from(3))),
            ("-3  d", SetValue::Subtract(Dice::from(3))),
            ("+0", SetValue::Add(Dice::from(0))),
            ("-004", SetValue::Subtract(Dice::from(4))),
            ("++1", SetValue::Add(Dice::from(1))),
            ("++1d", SetValue::Add(Dice::from(1))),
            ("-+1", SetValue::Subtract(Dice::from(1))),
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
