use entity::{active_character, character};
use poise::{execute_modal_on_component_interaction, CreateReply, Modal};
use sea_orm::{
    ActiveModelTrait, ActiveValue, ColumnTrait, Condition, DatabaseConnection, EntityTrait,
    QueryFilter, QueryOrder,
};
use serenity::all::{
    ButtonStyle, ChannelId, ComponentInteraction, CreateActionRow, CreateButton, CreateEmbed,
};
use serenity::futures::future::Either;
use serenity::futures::stream::{empty, iter};
use serenity::futures::{Stream, StreamExt, TryStreamExt};
use strum::{EnumString, IntoStaticStr};
use tracing::instrument;

use crate::commands::{handle_buttons, ButtonInteraction};
use crate::{Context, Error};

use super::{ButtonHandler, ButtonHandlerFuture, InteractionTarget};

/// Entrypoint for interacting with characters.
#[poise::command(
    slash_command,
    prefix_command,
    subcommand_required,
    subcommands("new", "edit", "delete", "show", "set", "list")
)]
pub async fn character(_: Context<'_>) -> Result<(), Error> {
    Ok(())
}

/// Creates a new character.
#[poise::command(prefix_command, slash_command)]
#[instrument(skip(ctx), fields(channel=?ctx.channel_id(), user=ctx.author().name))]
pub async fn new(
    ctx: Context<'_>,
    #[description = "Name of the character"] name: String,
    #[description = "Associate this character with the current channel. If false, you can see it anywhere."]
    lock_to_channel: Option<bool>,
) -> Result<(), Error> {
    let conn = ctx.data().db.conn();
    let message = format!("Created new character `{name}`!");
    let inserted_character = character::Entity::insert(character::ActiveModel {
        user_id: ActiveValue::Set(ctx.author().id.get() as i64),
        channel_id: ActiveValue::Set(
            lock_to_channel
                .unwrap_or_default()
                .then(|| ctx.channel_id().get() as i64),
        ),
        name: ActiveValue::Set(name),
        ..Default::default()
    })
    .exec(conn)
    .await?;
    let character_id = inserted_character.last_insert_id;
    let reply = CreateReply::default()
        .content(message)
        .components(character_buttons(character_id));
    ctx.send(reply).await?;

    handle_buttons::<CharacterButtonAction>(ctx, character_id.into()).await?;

    Ok(())
}

fn character_buttons(character_id: impl Into<CharacterId>) -> Vec<CreateActionRow> {
    vec![CreateActionRow::Buttons(vec![CreateButton::new(
        ButtonInteraction::new(
            CharacterButtonAction::Edit,
            CharacterId::from(character_id.into().0),
        ),
    )
    .label(CharacterButtonAction::Edit)
    .style(ButtonStyle::Primary)])]
}

/// Edits an existing character.
#[poise::command(prefix_command, slash_command)]
#[instrument(skip(ctx), fields(channel=?ctx.channel_id(), user=ctx.author().name))]
pub async fn edit(
    ctx: Context<'_>,
    #[autocomplete = "autocomplete_character_name"]
    #[description = "Name of the character"]
    name: Option<String>,
) -> Result<(), Error> {
    let (requested_char, conn) = find_character(&ctx, name.as_deref()).await?;
    let application_ctx = match ctx {
        poise::Context::Application(application_context) => application_context,
        poise::Context::Prefix(_) => return Err("Can only call this inside a slash command".into()),
    };

    let character_id = requested_char.id;
    let existing_character = requested_char.into();
    let char = Character::execute_with_defaults(application_ctx, existing_character)
        .await?
        .ok_or("No character received from modal")?;
    char.update(character_id, conn).await?;

    Ok(())
}
/// Deletes an existing character.
#[poise::command(prefix_command, slash_command)]
#[instrument(skip(ctx), fields(channel=?ctx.channel_id(), user=ctx.author().name))]
pub async fn delete(
    ctx: Context<'_>,
    #[autocomplete = "autocomplete_character_name"]
    #[description = "Name of the character"]
    name: String,
) -> Result<(), Error> {
    let (char, conn) = find_character(&ctx, Some(&name)).await?;
    character::ActiveModel::from(char).delete(conn).await?;
    ctx.say(format!("Deleted character `{name}`!")).await?;
    Ok(())
}

/// Shows information about a character.
#[poise::command(prefix_command, slash_command)]
#[instrument(skip(ctx), fields(channel=?ctx.channel_id(), user=ctx.author().name))]
pub async fn show(
    ctx: Context<'_>,
    #[autocomplete = "autocomplete_character_name"]
    #[description = "Name of the character"]
    name: Option<String>,
) -> Result<(), Error> {
    let (requested_char, _conn) = find_character(&ctx, name.as_deref()).await?;
    let mut embed = CreateEmbed::new()
        .title(requested_char.name)
        .description(requested_char.notes);
    if let Some(link) = requested_char.link {
        embed = embed.url(link);
    }
    let reply = CreateReply::default()
        .embed(embed)
        .components(character_buttons(requested_char.id));
    ctx.send(reply).await?;

    handle_buttons::<CharacterButtonAction>(ctx, requested_char.id.into()).await?;

    Ok(())
}

/// List all characters for this user.
#[poise::command(prefix_command, slash_command)]
#[instrument(skip(ctx), fields(channel=?ctx.channel_id(), user=ctx.author().name))]
pub async fn list(
    ctx: Context<'_>,
    #[description = "Character name search string"] name: Option<String>,
) -> Result<(), Error> {
    let mut query = character::Entity::find()
        .find_also_related(active_character::Entity)
        .order_by_desc(active_character::Column::Updated)
        .order_by_desc(character::Column::Updated);
    if let Some(name) = name {
        query = query.filter(character::Column::Name.contains(name));
    }
    let characters = query.all(ctx.data().db.conn()).await?;
    let max_name_length = characters
        .iter()
        .map(|(c, _)| c.name.len())
        .max()
        .unwrap_or_default();
    let width = (max_name_length + 3).max(8);
    let table = iter(characters)
        .map(Ok)
        .and_then(|(character, active)| async move {
            let channel_name = if let Some(channel_id) = character.channel_id {
                ChannelId::new(channel_id as u64).name(ctx).await?
            } else {
                String::new()
            };
            Ok::<_, Error>(format!(
                "{} {:width$}{:53}   {}",
                if active.is_some() { "(*)" } else { "   " },
                character.name,
                character
                    .notes
                    .split_at_checked(50)
                    .map(|(first, _rest)| first.replace("\n", " ") + "...")
                    .unwrap_or(character.notes),
                channel_name,
            ))
        })
        .try_collect::<Vec<_>>()
        .await?
        .join("\n");
    let message = format!("```\n{table}\n```");
    ctx.reply(message).await?;

    Ok(())
}

/// Sets the active character, which will be the default for other character commands.
#[poise::command(prefix_command, slash_command)]
#[instrument(skip(ctx), fields(channel=?ctx.channel_id(), user=ctx.author().name))]
pub async fn set(
    ctx: Context<'_>,
    #[autocomplete = "autocomplete_character_name"]
    #[description = "Name of the character"]
    name: String,
) -> Result<(), Error> {
    let (char, conn) = find_character(&ctx, Some(&name)).await?;
    let user_id = ctx.author().id.get() as i64;
    let active_character_model = active_character::ActiveModel {
        user_id: ActiveValue::Set(user_id),
        character_id: ActiveValue::Set(char.id as i64),
        ..Default::default()
    };
    if active_character::Entity::find_by_id(user_id)
        .one(conn)
        .await?
        .is_some()
    {
        active_character_model.update(conn).await?;
    } else {
        active_character_model.insert(conn).await?;
    }
    ctx.say(format!("Set active character to `{name}`!"))
        .await?;
    Ok(())
}

async fn find_character<'a>(
    ctx: &Context<'a>,
    name: Option<&str>,
) -> Result<(character::Model, &'a DatabaseConnection), Error> {
    let conn = ctx.data().db.conn();
    let char = if let Some(name) = name {
        query(ctx)
            .filter(character::Column::Name.eq(name))
            .one(conn)
            .await?
            .ok_or_else(|| format!("Character `{name}` not found!"))?
    } else {
        active_character::Entity::find_by_id(ctx.author().id.get() as i64)
            .find_also_related(character::Entity)
            .one(conn)
            .await?
            .and_then(|(_active, character)| character)
            .ok_or("No active character set and no name provided!")?
    };
    Ok((char, conn))
}
fn query(ctx: &Context<'_>) -> sea_orm::Select<character::Entity> {
    character::Entity::find()
        .filter(character::Column::UserId.eq(ctx.author().id.get()))
        .filter(
            Condition::any()
                .add(character::Column::ChannelId.is_null())
                .add(character::Column::ChannelId.eq(ctx.channel_id().get())),
        )
}

async fn autocomplete_character_name<'a>(
    ctx: Context<'a>,
    partial: &'a str,
) -> impl Stream<Item = String> + 'a {
    let conn = ctx.data().db.conn();

    query(&ctx)
        .filter(character::Column::Name.contains(partial))
        .stream(conn)
        .await
        .map_or(Either::Left(empty()), Either::Right)
        .filter_map(|res| std::future::ready(res.ok().map(|char| char.name)))
}

/// Defines the structure of the form shown to edit character information.
#[derive(poise::Modal)]
struct Character {
    name: String,
    link: Option<String>,
    #[paragraph]
    notes: String,
}
impl Character {
    async fn update(
        self,
        character_id: impl Into<CharacterId>,
        conn: &DatabaseConnection,
    ) -> Result<(), Error> {
        character::ActiveModel {
            id: ActiveValue::Unchanged(character_id.into().0),
            name: ActiveValue::Set(self.name),
            link: ActiveValue::Set(self.link),
            notes: ActiveValue::Set(self.notes),
            ..Default::default()
        }
        .update(conn)
        .await?;
        Ok(())
    }
}
impl From<character::Model> for Character {
    fn from(character: character::Model) -> Self {
        let character::Model {
            id: _,
            user_id: _,
            channel_id: _,
            name,
            notes,
            link,
            created: _,
            updated: _,
        } = character;
        Character { name, link, notes }
    }
}

/// The different options for what a button on a message can do.
///
/// Note that the serialize options need be unique between this and any other implementors of
/// [`ButtonHandler`] because all the button interactions go into the same pipe and have to get
/// sorted out using this.
#[derive(Debug, EnumString, IntoStaticStr)]
pub enum CharacterButtonAction {
    #[strum(serialize = "e")]
    Edit,
}
impl From<CharacterButtonAction> for String {
    fn from(value: CharacterButtonAction) -> Self {
        format!("{value:?}")
    }
}
impl ButtonHandler for CharacterButtonAction {
    type Target<'a> = CharacterId;
    fn handle<'a: 'b, 'b>(
        self,
        ctx: Context<'a>,
        mci: &'b ComponentInteraction,
        target: Self::Target<'a>,
    ) -> ButtonHandlerFuture<'b> {
        Box::pin(async move {
            match self {
                CharacterButtonAction::Edit => {
                    tracing::info!(?target, "Opening edit modal for character");
                    let conn = ctx.data().db.conn();
                    let existing_character = character::Entity::find_by_id(target.0)
                        .one(conn)
                        .await?
                        .ok_or_else(|| format!("Requested character `{target:?}` not found!"))?;
                    let character_id = existing_character.id;
                    let new_character = execute_modal_on_component_interaction(
                        ctx,
                        mci.clone(),
                        Some(Character::from(existing_character)),
                        None,
                    )
                    .await?
                    .ok_or("No character received from modal")?;
                    new_character.update(character_id, conn).await?;
                    Ok(None)
                }
            }
        })
    }
}

#[derive(PartialEq, Clone, Copy, derive_more::From, Debug, derive_more::Display)]
pub struct CharacterId(i32);

impl InteractionTarget for CharacterId {
    fn id(&self) -> super::InteractionId {
        (*self).into()
    }
}
impl std::str::FromStr for CharacterId {
    type Err = std::num::ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let id = s.parse()?;
        Ok(CharacterId(id))
    }
}
