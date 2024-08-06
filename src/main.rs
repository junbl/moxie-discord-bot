use anyhow::Context as _;
use poise::serenity_prelude::{ClientBuilder, GatewayIntents};
use shuttle_runtime::SecretStore;
use shuttle_serenity::ShuttleSerenity;

mod commands;
mod error;
mod pools_in_database;
mod rolls;

use commands::{hello, help, pool::pool, quickpool};

// use commands::pool::{check, delete, new, reset, roll, set};
use pools_in_database::Pools;
use rolls::Rolls;

/// User data, which is stored and accessible in all command invocations
pub struct Data {
    pools: Pools,
    rolls: Rolls,
}
pub type Error = Box<dyn std::error::Error + Send + Sync>;
pub type Context<'a> = poise::Context<'a, Data, Error>;

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
        .map_err(shuttle_runtime::CustomError::new)?;

    let framework = poise::Framework::builder()
        .options(poise::FrameworkOptions {
            commands: vec![hello(), pool(), quickpool(), help()],
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

    // let intents = GatewayIntents::GUILD_MESSAGES | GatewayIntents::MESSAGE_CONTENT;

    // let client = ClientBuilder::new(discord_token, intents)
    let client = ClientBuilder::new(discord_token, GatewayIntents::non_privileged())
        .framework(framework)
        .await
        .map_err(shuttle_runtime::CustomError::new)?;

    Ok(client.into())
}
