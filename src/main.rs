use anyhow::Context as _;
use poise::serenity_prelude::{ClientBuilder, GatewayIntents};
use shuttle_runtime::SecretStore;
use shuttle_serenity::ShuttleSerenity;

mod commands;
mod error;
mod pools_in_database;
mod rolls;

use commands::{help, pool::pool, quickpool, roll::roll, scenebreak};

// use commands::pool::{check, delete, new, reset, roll, set};
use pools_in_database::Pools;
use rolls::{RollDistribution, ThornDistribution};

/// User data, which is stored and accessible in all command invocations
pub struct Data {
    pools: Pools,
    roll_dist: RollDistribution,
    thorn_dist: ThornDistribution,
}
pub type Error = Box<dyn std::error::Error + Send + Sync>;
pub type Context<'a> = poise::Context<'a, Data, Error>;

#[shuttle_runtime::main]
async fn main(
    #[shuttle_runtime::Secrets] secret_store: SecretStore,
    #[shuttle_shared_db::Postgres] conn_str: String,
) -> ShuttleSerenity {
    let discord_token = secret_store
        .get("DISCORD_TOKEN")
        .context("'DISCORD_TOKEN' was not found")?;
    let conn = sea_orm::Database::connect(conn_str)
        .await
        .map_err(shuttle_runtime::CustomError::new)?;

    let framework = poise::Framework::builder()
        .options(poise::FrameworkOptions {
            commands: vec![pool(), quickpool(), help(), roll(), scenebreak()],
            ..Default::default()
        })
        .setup(|ctx, _ready, framework| {
            Box::pin(async move {
                poise::builtins::register_globally(ctx, &framework.options().commands).await?;
                Ok(Data {
                    pools: Pools::new(conn),
                    roll_dist: RollDistribution::new(),
                    thorn_dist: ThornDistribution::new(),
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


macro_rules! write_s {
    ($s:expr, $($fmt:tt)*) => {
        {
            use std::fmt::Write;
            let s: &mut String = &mut $s;
            write!(s, $($fmt)*).unwrap();
        }
    };
}
pub(crate) use write_s;