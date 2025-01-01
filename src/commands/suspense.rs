use entity::suspense;
use itertools::Itertools;
use sea_orm::ActiveValue::{NotSet, Set};
use sea_orm::{
    ActiveModelTrait, ColumnTrait, Condition, EntityTrait, IntoActiveModel, QueryFilter,
    QuerySelect, Select,
};
use tracing::{info, instrument};

use crate::commands::pool::SetValue;
use crate::commands::roll::Dice;
use crate::{write_s, Context, Error};

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

async fn query(
    select: Select<suspense::Entity>,
    ctx: &Context<'_>,
) -> Result<Vec<suspense::Model>, sea_orm::DbErr> {
    select
        .filter(suspense::Column::ChannelId.eq(ctx.channel_id().get()))
        .all(ctx.data().pools.conn())
        .await
}

async fn query_with_opt_challenge(
    select: Select<suspense::Entity>,
    challenge: &Option<String>,
    ctx: &Context<'_>,
) -> Result<Option<suspense::Model>, sea_orm::DbErr> {
    let condition = Condition::all();
    let condition = condition.add(if let Some(challenge) = challenge {
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
    challenge: Option<String>,
    #[description = "Add like `+1`, subtract like `-2`, or set like `6`"] suspense: SetValue,
) -> Result<(), Error> {
    info!("Received command: suspense set");
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
        query_with_opt_challenge(suspense::Entity::find(), &challenge, &ctx)
            .await?
            .map_or_else(
                || {
                    let suspense = 0;
                    (
                        suspense::ActiveModel {
                            id: NotSet,
                            channel_id: Set(ctx.channel_id().get() as i64),
                            suspense: Set(suspense as i16),
                            challenge: Set(None),
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

    let new_suspense = suspense.apply(Dice::from(suspense_value));
    let active_model = suspense::ActiveModel {
        suspense: Set(new_suspense.dice as i16),
        ..current_suspense
    };
    let _ = active_model.save(ctx.data().pools.conn()).await?;
    let mut message = format!("Set suspense to {new_suspense}");
    if let Some(challenge) = challenge {
        write_s!(message, " for challenge {challenge}");
    }

    Ok(message)
}

/// Add to the current amount of suspense.
#[poise::command(prefix_command, slash_command)]
#[instrument(skip(ctx), fields(channel=?ctx.channel_id(), user=ctx.author().name))]
pub async fn up(
    ctx: Context<'_>,
    challenge: Option<String>,
    #[description = "Add like `+1`, subtract like `-2`, or set like `6`. Defaults to 1, or 2 if `challenge` is provided."]
    suspense: Option<SetValue>,
) -> Result<(), Error> {
    info!("Received command: suspense up");
    let suspense = match suspense.unwrap_or(SetValue::Add(1.into())) {
        SetValue::Add(dice) => SetValue::Add(dice),
        SetValue::Subtract(dice) => SetValue::Subtract(dice),
        SetValue::Set(dice) => SetValue::Add(dice),
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
    challenge: Option<String>,
    #[description = "Add like `+1`, subtract like `-2`, or set like `6`. Defaults to 1."] suspense: Option<SetValue>,
) -> Result<(), Error> {
    info!("Received command: suspense down");
    let suspense = match suspense.unwrap_or(SetValue::Subtract(1.into())) {
        SetValue::Add(dice) => SetValue::Subtract(dice),
        SetValue::Subtract(dice) => SetValue::Add(dice),
        SetValue::Set(dice) => SetValue::Subtract(dice),
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
    let single_suspense_message = |suspense| format!("# Suspense: {suspense}");
    let message = if suspenses.is_empty() {
        single_suspense_message(0)
    } else {
        match <[suspense::Model; 1]>::try_from(suspenses) {
            Ok([suspense]) => single_suspense_message(suspense.suspense),
            Err(suspenses) => {
                let max_name_length = suspenses
                    .iter()
                    .filter_map(|s| s.challenge.as_ref().map(|c| c.len()))
                    .max()
                    .unwrap_or_default();
                let width = (max_name_length + 3).max(8);
                let mut total_suspense = 0;
                let message = suspenses
                    .into_iter()
                    .filter(|s| s.challenge.is_none() || s.suspense > 0)
                    .map(|suspense| {
                        total_suspense += suspense.suspense;
                        format!(
                            "{:width$}{}",
                            suspense.challenge.unwrap_or_default(),
                            suspense.suspense,
                        )
                    })
                    .join("\n");
                format!("```\n{message}\n```# Total Suspense: `{total_suspense}`")
            }
        }
    };
    ctx.say(message).await?;
    Ok(())
}