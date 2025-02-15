use std::borrow::Cow;

use entity::suspense;
use itertools::Itertools;
use sea_orm::ActiveValue::{NotSet, Set};
use sea_orm::{
    ActiveModelTrait, ColumnTrait, Condition, EntityTrait, IntoActiveModel, QueryFilter,
    QuerySelect, Select,
};
use serenity::futures::stream::iter;
use serenity::futures::Stream;
use tracing::{debug, info, instrument};

use crate::commands::pool::SetValue;
use crate::commands::roll::Dice;
use crate::{write_s, Context, Error};

/// Entrypoint for interacting with suspense.
#[poise::command(
    slash_command,
    prefix_command,
    aliases("s"),
    subcommand_required,
    subcommands("up", "down", "set", "check", "delete")
)]
pub async fn suspense(_: Context<'_>) -> Result<(), Error> {
    Ok(())
}

async fn query(
    select: Select<suspense::Entity>,
    ctx: &Context<'_>,
) -> Result<Vec<suspense::Model>, sea_orm::DbErr> {
    select
        .filter(suspense::Column::ChannelId.eq(ctx.channel_id().get()))
        .all(ctx.data().db.conn())
        .await
}

async fn query_with_opt_challenge(
    select: Select<suspense::Entity>,
    challenge: Option<&str>,
    ctx: &Context<'_>,
) -> Result<Option<suspense::Model>, sea_orm::DbErr> {
    let condition = Condition::all();
    let condition = condition.add(if let Some(challenge) = challenge {
        debug!(challenge, "pulling suspense for challenge");
        suspense::Column::Challenge.eq(challenge)
    } else {
        suspense::Column::Challenge.is_null()
    });
    let suspenses = query(select.limit(1).filter(condition), ctx).await?;
    Ok(suspenses.into_iter().next())
}

/// Manually change the amount of suspense.
#[poise::command(prefix_command, slash_command)]
#[instrument(skip(ctx), fields(channel=?ctx.channel_id(), user=ctx.author().name))]
pub async fn set(
    ctx: Context<'_>,
    #[description = "Optionally, the name of a challenge that this suspense is associated with"]
    #[autocomplete = "autocomplete_challenge_name"]
    challenge: Option<String>,
    #[description = "Add like `+1`, subtract like `-2`, or set like `6`"] suspense: SetValue,
) -> Result<(), Error> {
    info!(challenge, ?suspense, "Received command: suspense set");
    let message = set_inner(ctx, challenge, suspense).await?;
    ctx.say(message).await?;
    Ok(())
}

async fn set_inner(
    ctx: Context<'_>,
    challenge: Option<String>,
    suspense: SetValue,
) -> Result<String, Error> {
    let (current_suspense, suspense_value) =
        query_with_opt_challenge(suspense::Entity::find(), challenge.as_deref(), &ctx)
            .await?
            .map_or_else(
                || {
                    let suspense = 0;
                    (
                        suspense::ActiveModel {
                            id: NotSet,
                            channel_id: Set(ctx.channel_id().get() as i64),
                            suspense: Set(suspense as i16),
                            challenge: Set(challenge.clone()),
                            created: NotSet,
                            updated: NotSet,
                        },
                        suspense,
                    )
                },
                |m| {
                    let suspense = m.suspense as u8;
                    (m.into_active_model(), suspense)
                },
            );

    info!(
        ?suspense,
        current = suspense_value,
        "setting new suspense value"
    );
    let new_suspense = suspense.apply(Dice::from(suspense_value));
    let new_suspense = new_suspense.dice as i16;
    let active_model = suspense::ActiveModel {
        suspense: Set(new_suspense),
        ..current_suspense
    };
    let _ = active_model.save(ctx.data().db.conn()).await?;
    let add_amt_to_msg = |dice: Dice, msg| {
        let mut msg = String::from(msg);
        let dice = dice.dice;
        if dice != 1 {
            write_s!(msg, " by `{dice}`");
        }
        Cow::Owned(msg)
    };
    let verb = match suspense {
        SetValue::Add(dice) => add_amt_to_msg(dice, "Upped suspense"),
        SetValue::Subtract(dice) => add_amt_to_msg(dice, "Dropped suspense"),
        SetValue::Set(_) => Cow::Borrowed("Set suspense"),
    };
    let mut message = format!("# {verb} to `{new_suspense}`");
    if let Some(challenge) = challenge {
        write_s!(message, " for challenge `{challenge}`");
    }

    Ok(message)
}

/// Add to the current amount of suspense.
#[poise::command(prefix_command, slash_command)]
#[instrument(skip(ctx), fields(channel=?ctx.channel_id(), user=ctx.author().name))]
pub async fn up(
    ctx: Context<'_>,
    #[autocomplete = "autocomplete_challenge_name"]
    #[description = "Optionally, the name of a challenge that this suspense is associated with"]
    challenge: Option<String>,
    #[description = "Add like `+1`, subtract like `-2`, or set like `6`. Defaults to +1, or +2 for a `challenge`."]
    suspense: Option<SetValue>,
) -> Result<(), Error> {
    info!(challenge, ?suspense, "Received command: suspense up");
    let suspense = suspense.unwrap_or(SetValue::Add(
        if challenge.is_some() { 2 } else { 1 }.into(),
    ));
    let suspense = match suspense {
        SetValue::Add(dice) | SetValue::Set(dice) => SetValue::Add(dice),
        SetValue::Subtract(dice) => SetValue::Subtract(dice),
    };
    let message = set_inner(ctx, challenge, suspense).await?;
    ctx.say(message).await?;
    Ok(())
}

/// Subtract from the current amount of suspense.
#[poise::command(prefix_command, slash_command)]
#[instrument(skip(ctx), fields(channel=?ctx.channel_id(), user=ctx.author().name))]
pub async fn down(
    ctx: Context<'_>,
    #[description = "Optionally, the name of a challenge that this suspense is associated with"]
    #[autocomplete = "autocomplete_challenge_name"]
    challenge: Option<String>,
    #[description = "Add like `+1`, subtract like `-2`, or set like `6`. Defaults to -1."] suspense: Option<SetValue>,
) -> Result<(), Error> {
    info!(challenge, ?suspense, "Received command: suspense down");
    let suspense = match suspense.unwrap_or(SetValue::Add(1.into())) {
        SetValue::Add(dice) | SetValue::Set(dice) => SetValue::Subtract(dice),
        SetValue::Subtract(dice) => SetValue::Add(dice),
    };
    let message = set_inner(ctx, challenge, suspense).await?;
    ctx.say(message).await?;
    Ok(())
}

/// Check the current amount of suspense.
#[poise::command(prefix_command, slash_command)]
#[instrument(skip(ctx), fields(channel=?ctx.channel_id(), user=ctx.author().name))]
pub async fn check(ctx: Context<'_>) -> Result<(), Error> {
    info!("Received command: suspense check");
    let suspenses = query(suspense::Entity::find(), &ctx).await?;
    let single_suspense_message = |suspense| format!("# Suspense: `{suspense}`");
    let global_message = "(Global)";
    let multi_suspense_message = |suspenses: Vec<suspense::Model>| {
        let max_name_length = suspenses
            .iter()
            .filter_map(|s| s.challenge.as_ref().map(|c| c.len()))
            .max()
            .unwrap_or_default();
        let width = (max_name_length).max(global_message.len()) + 3;
        let mut total_suspense = 0;
        let message = suspenses
            .into_iter()
            .filter(|s| s.challenge.is_none() || s.suspense > 0)
            .map(|suspense| {
                total_suspense += suspense.suspense;
                format!(
                    "{:width$}{}",
                    suspense.challenge.map_or(global_message.into(), Cow::Owned),
                    suspense.suspense,
                )
            })
            .join("\n");
        format!("# Total Suspense: `{total_suspense}`\n```{message}```")
    };
    let message = if suspenses.is_empty() {
        single_suspense_message(0)
    } else {
        match <[suspense::Model; 1]>::try_from(suspenses) {
            Ok([suspense]) if suspense.challenge.is_none() => {
                single_suspense_message(suspense.suspense)
            }
            Ok([suspense]) => {
                let fake_global_suspense = suspense::Model {
                    suspense: 0,
                    challenge: None,
                    ..suspense.clone()
                };
                let suspenses = vec![suspense, fake_global_suspense];
                multi_suspense_message(suspenses)
            }
            Err(suspenses) => multi_suspense_message(suspenses),
        }
    };
    ctx.say(message).await?;
    Ok(())
}

/// Delete a suspense entry for a challenge.
#[poise::command(prefix_command, slash_command)]
#[instrument(skip(ctx), fields(channel=?ctx.channel_id(), user=ctx.author().name))]
pub async fn delete(
    ctx: Context<'_>,
    #[description = "The name of a challenge to delete"]
    #[autocomplete = "autocomplete_challenge_name"]
    challenge: String,
) -> Result<(), Error> {
    info!(challenge, "Received command: suspense delete");
    let suspense = query_with_opt_challenge(suspense::Entity::find(), Some(&challenge), &ctx)
        .await?
        .ok_or_else(|| format!("Challenge not found: {challenge}"))?;
    suspense::Entity::delete_by_id(suspense.id)
        .exec(ctx.data().db.conn())
        .await?;
    let message = format!("Deleted suspense entry for challenge `{challenge}`");
    ctx.say(message).await?;
    Ok(())
}

async fn autocomplete_challenge_name<'a>(
    ctx: Context<'a>,
    partial: &'a str,
) -> impl Stream<Item = String> + 'a {
    let challenges = query(
        suspense::Entity::find()
            .filter(suspense::Column::Challenge.is_not_null())
            .filter(suspense::Column::Challenge.contains(partial)),
        &ctx,
    )
    .await
    .unwrap_or_default();
    iter(challenges.into_iter().filter_map(|c| c.challenge))
}
