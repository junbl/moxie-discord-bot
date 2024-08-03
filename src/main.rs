use anyhow::{anyhow, Context as _};
use entity::{channel_pool, server_pool};
use poise::serenity_prelude::{ClientBuilder, GatewayIntents};
use pool::Scope;
use rolls::{Pool, Roll, Rolls};
use sea_orm::ActiveValue::{Set, Unchanged};
use sea_orm::DatabaseConnection;
use sea_orm::{ActiveModelTrait, ColumnTrait, EntityTrait, QueryFilter};
use shuttle_runtime::SecretStore;
use shuttle_serenity::ShuttleSerenity;

mod pool;
mod rolls;

/// User data, which is stored and accessible in all command invocations
pub struct Data {
    pools: Pools,
    rolls: Rolls,
}
pub type Error = Box<dyn std::error::Error + Send + Sync>;
pub type Context<'a> = poise::Context<'a, Data, Error>;

enum PoolId {
    Server(i32),
    Channel(i32),
}

struct PoolInDb {
    pool: Pool,
    id: PoolId,
}
impl PoolInDb {
    pub fn new(dice: u8, id: PoolId) -> Self {
        Self {
            pool: Pool::new(dice),
            id,
        }
    }
    pub async fn roll(
        &mut self,
        conn: &DatabaseConnection,
        rolls: &Rolls,
    ) -> Result<Vec<Roll>, anyhow::Error> {
        let rolls = self.pool.roll(rolls);
        match self.id {
            PoolId::Server(id) => {
                server_pool::ActiveModel {
                    id: Unchanged(id),
                    current_size: Set(self.pool.dice() as i16),
                    updated: Set(chrono::Utc::now()),
                    ..Default::default()
                }
                .update(conn)
                .await?;
            }
            PoolId::Channel(id) => {
                channel_pool::ActiveModel {
                    id: Unchanged(id),
                    current_size: Set(self.pool.dice() as i16),
                    updated: Set(chrono::Utc::now()),
                    ..Default::default()
                }
                .update(conn)
                .await?;
            }
        }
        Ok(rolls)
    }
}

struct Pools {
    conn: DatabaseConnection,
}

impl Pools {
    pub fn new(conn: DatabaseConnection) -> Self {
        Self { conn }
    }
    pub async fn roll_pool(
        &self,
        scope: Scope,
        pool_name: &str,
        rolls: &Rolls,
    ) -> Result<Option<Vec<Roll>>, anyhow::Error> {
        let Some(mut pool) = self.get_pool(scope, pool_name).await? else {
            return Ok(None);
        };
        let rolls = pool.roll(&self.conn, rolls).await?;
        Ok(Some(rolls))
    }
    pub async fn get_pool(
        &self,
        scope: Scope,
        pool_name: &str,
    ) -> anyhow::Result<Option<PoolInDb>> {
        let conn = &self.conn;
        Ok(match scope {
            Scope::Server(server_id) => server_pool::Entity::find()
                .filter(server_pool::Column::ServerId.eq(server_id.get()))
                .filter(server_pool::Column::Name.eq(pool_name))
                .one(conn)
                .await?
                .map(|pool| PoolInDb::new(pool.current_size as u8, PoolId::Server(pool.id))),
            Scope::Channel(channel_id) => channel_pool::Entity::find()
                .filter(channel_pool::Column::ChannelId.eq(channel_id.get()))
                .filter(channel_pool::Column::Name.eq(pool_name))
                .one(conn)
                .await?
                .map(|pool| PoolInDb::new(pool.current_size as u8, PoolId::Channel(pool.id))),
        })
    }
    pub async fn new_pool(
        &self,
        scope: Scope,
        pool_name: String,
        pool_size: u8,
    ) -> Result<(), Error> {
        let pool_size = pool_size as i16;
        match scope {
            Scope::Server(server_id) => {
                let new_pool = server_pool::ActiveModel {
                    server_id: Set(server_id.get() as i64),
                    name: Set(pool_name),
                    original_size: Set(pool_size),
                    current_size: Set(pool_size),
                    ..Default::default()
                };
                new_pool.save(&self.conn).await?;
            }
            Scope::Channel(channel_id) => {
                let new_pool = channel_pool::ActiveModel {
                    channel_id: Set(channel_id.get() as i64),
                    name: Set(pool_name),
                    original_size: Set(pool_size),
                    current_size: Set(pool_size),
                    ..Default::default()
                };
                new_pool.save(&self.conn).await?;
            }
        };
        Ok(())
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
    #[description = "Number of dice in the pool"] num_dice: u8,
) -> Result<(), Error> {
    let mut pool = Pool::new(num_dice);
    let rolls = pool.roll(&ctx.data().rolls);

    ctx.say(crate::pool::print_pool_results(&rolls, pool))
        .await?;
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
            commands: vec![hello(), pool::pool(), pooln()],
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
