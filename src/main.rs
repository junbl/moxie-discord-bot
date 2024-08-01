use std::any::Any;
use std::sync::{Arc, RwLock};

use anyhow::{anyhow, Context as _};
use itertools::Itertools;
use poise::serenity_prelude::{ClientBuilder, GatewayIntents};
use rand::{thread_rng, Rng};
use rolls::{Pool, Roll, Rolls};
use sea_orm::DatabaseConnection;
use serenity::all::{ChannelId, ChannelType, GuildId};
use shuttle_runtime::SecretStore;
use shuttle_serenity::ShuttleSerenity;
use std::collections::HashMap;

mod rolls;

// #[derive(Default)]
struct Pools {
    inner: DatabaseConnection,
}

impl Pools {
    pub fn new(inner: DatabaseConnection) -> Self {
        Self { inner }
    }
    pub fn roll_pool<R: Rng + ?Sized>(
        &mut self,
        guild_id: GuildId,
        pool_name: &str,
        rng: &mut R,
        rolls: &Rolls,
    ) -> Result<Option<Vec<Roll>>, anyhow::Error> {
        let guild_id = guild_id.to_string();
        let mut server_pools: HashMap<String, Pool> = todo!();
        let Some(pool) = server_pools.get_mut(pool_name) else {
            return Ok(None);
        };
        let rolls = pool.roll(rng, rolls);
        // self.inner.save(&guild_id, server_pools)?;
        Ok(Some(rolls))
    }
    pub async fn get_channel_pool(
        &self,
        channel_id: ChannelId,
        pool_name: &str,
    ) -> anyhow::Result<Pool> {
        todo!();
    }
}

// impl Pools {
//     fn new() -> anyhow::Result<Self> {
//         let filename = std::path::Path::new("pools.json");
//         if !filename.exists() {
//             Ok(Self::default())
//         } else {
//             let pools = serde_json::from_str(&std::fs::read_to_string(filename)?)?;
//             Ok(Self {
//                 inner: Arc::new(RwLock::new(pools)),
//             })
//         }
//     }
// }

struct Data {
    pools: Pools,
    rolls: Rolls,
} // User data, which is stored and accessible in all command invocations
type Error = Box<dyn std::error::Error + Send + Sync>;
type Context<'a> = poise::Context<'a, Data, Error>;

/// Responds with "world!"
#[poise::command(slash_command)]
async fn hello(ctx: Context<'_>) -> Result<(), Error> {
    ctx.say("world!").await?;
    Ok(())
}

/// Creates or rolls a pool.
#[poise::command(slash_command)]
async fn pooln(
    ctx: Context<'_>,
    #[description = "Number of dice in the pool"] num_dice: usize,
) -> Result<(), Error> {
    let mut pool = Pool::new(num_dice);
    let message = {
        let mut rng = thread_rng();
        let rolls = pool.roll(&mut rng, &ctx.data().rolls);
        format!(
            "rolls: {}\nremaining dice: {}",
            rolls.iter().map(ToString::to_string).join(" "),
            pool.dice()
        )
    };

    ctx.say(message).await?;
    Ok(())
}
/// Entrypoint for interacting with pools.
#[poise::command(
    prefix_command,
    slash_command,
    subcommands("new", "roll", "reset", "delete", "adddice")
)]
async fn pool(_: Context<'_>) -> Result<(), Error> {
    Ok(())
}
#[poise::command(prefix_command, slash_command)]
async fn new(
    ctx: Context<'_>,
    #[description = "Name of the pool"] pool: String,
) -> Result<(), Error> {
    let guild_id = ctx.guild_id().map(|gid| gid.get());
    let channel_id = ctx.channel_id().get();
    ctx.say("world!").await?;
    Ok(())
}
#[poise::command(prefix_command, slash_command)]
async fn roll(
    ctx: Context<'_>,
    #[description = "Name of the pool"] pool: String,
) -> Result<(), Error> {
    ctx.guild_id();
    ctx.say("world!").await?;
    Ok(())
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

#[shuttle_runtime::main]
async fn main(
    #[shuttle_runtime::Secrets] secret_store: SecretStore,
    #[shuttle_shared_db::Postgres] conn_str: String,
) -> ShuttleSerenity {
    // Get the discord token set in `Secrets.toml`
    let discord_token = secret_store
        .get("DISCORD_TOKEN")
        .context("'DISCORD_TOKEN' was not found")?;
    let conn = sea_orm::Database::connect(conn_str)
        .await
        .map_err(|e| anyhow!(e))?;

    let framework = poise::Framework::builder()
        .options(poise::FrameworkOptions {
            commands: vec![hello(), pool(), pooln()],
            ..Default::default()
        })
        .setup(|ctx, _ready, framework| {
            Box::pin(async move {
                poise::builtins::register_globally(ctx, &framework.options().commands).await?;
                Ok(Data {
                    rolls: Rolls::new(),
                    pools: Pools::new(conn),
                })
            })
        })
        .build();

    let client = ClientBuilder::new(discord_token, GatewayIntents::non_privileged())
        .framework(framework)
        .await
        .map_err(shuttle_runtime::CustomError::new)?;

    Ok(client.into())
}
