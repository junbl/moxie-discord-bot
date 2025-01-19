//! This module contains the code for performing operations on pools in the database.

use anyhow::anyhow;
use entity::{channel_pool, server_pool};
use sea_orm::ActiveValue::{NotSet, Set, Unchanged};
use sea_orm::{ActiveModelTrait, ColumnTrait, EntityTrait, QueryFilter, QueryOrder};
use sea_orm::{DatabaseConnection, QuerySelect};
use serenity::futures::{Stream, TryFutureExt, TryStreamExt};

use crate::commands::pool::SetValue;
use crate::commands::roll::Dice;
use crate::commands::Scope;
use crate::error::MoxieError;
use crate::rolls::{Pool, Roll, RollDistribution};
use crate::Error;

macro_rules! match_pool_id {
    ($pool_id:expr, $body_fn:expr) => {
        match $pool_id {
            PoolId::Server(id) => {
                use entity::server_pool::*;
                ($body_fn)(id).await
            }
            PoolId::Channel(id) => {
                use entity::channel_pool::*;
                ($body_fn)(id).await
            }
        }
    };
}

macro_rules! match_scope {
    ($scope:expr, $body_fn:expr) => {
        match $scope {
            Scope::Server(id) => {
                #[allow(unused_imports)]
                {
                    use entity::server_pool::Column::ServerId as Id;
                    use entity::server_pool::*;
                    use serenity::futures::future::Either::Left as Either;
                    use PoolId::Server as PoolIdVariant;
                    ($body_fn)(id.get()).await
                }
            }
            Scope::Channel(id) => {
                #[allow(unused_imports)]
                {
                    use entity::channel_pool::Column::ChannelId as Id;
                    use entity::channel_pool::*;
                    use serenity::futures::future::Either::Right as Either;
                    use PoolId::Channel as PoolIdVariant;
                    ($body_fn)(id.get()).await
                }
            }
        }
    };
}
#[derive(Copy, Clone, PartialEq, Eq)]
pub enum PoolId {
    Server(i32),
    Channel(i32),
}
impl std::fmt::Display for PoolId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PoolId::Server(id) => write!(f, "s{id}"),
            PoolId::Channel(id) => write!(f, "c{id}"),
        }
    }
}
#[derive(Debug, thiserror::Error)]
pub enum ParsePoolIdError {
    #[error("failed to parse input as pool id: {0}")]
    BadVariant(String),
    #[error("{0}")]
    BadId(#[from] std::num::ParseIntError),
}
impl std::str::FromStr for PoolId {
    type Err = ParsePoolIdError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (scope, id) = s.split_at(1);
        let id = id.parse()?;
        match scope {
            "s" => Ok(PoolId::Server(id)),
            "c" => Ok(PoolId::Channel(id)),
            other => Err(ParsePoolIdError::BadVariant(other.to_string())),
        }
    }
}

#[derive(Clone)]
pub struct PoolInDb {
    pub pool: Pool,
    pub name: String,
    pub id: PoolId,
    original_size: Dice,
}
impl PoolInDb {
    pub fn new(dice: Dice, name: String, id: PoolId, original_size: Dice) -> Self {
        Self {
            pool: Pool::new(dice),
            name,
            id,
            original_size,
        }
    }
    // pub async fn lookup(pools: &Pools, pool_id: PoolId) -> Result<Self, Error> {
    //     match_pool_id!(pool_id, |id| async move {
    //         let model = Entity::find_by_id(id)
    //             .one(pools.conn())
    //             .await?
    //             .ok_or_else(|| MoxieError::PoolNotFound(pool_id.to_string()))?;
    //         Ok(model.into())
    //     })
    // }
    pub fn name(&self) -> &str {
        self.name.as_str()
    }
    pub fn original_size(&self) -> Dice {
        self.original_size
    }
    pub async fn roll(
        &mut self,
        conn: &DatabaseConnection,
        rolls: &RollDistribution,
        only_roll_some: Option<Dice>,
    ) -> Result<Vec<Roll>, anyhow::Error> {
        let rolls = self.pool.roll(rolls, only_roll_some);
        match_pool_id!(self.id, |id| async move {
            ActiveModel {
                id: Unchanged(id),
                current_size: Set(self.pool.dice().dice as i16),
                updated: Set(chrono::Utc::now()),
                ..Default::default()
            }
            .update(conn)
            .await
            .map(|_| ())
        })?;
        Ok(rolls)
    }
    pub async fn delete(&self, pools: &Pools) -> Result<Pool, Error> {
        let delete = match_pool_id!(self.id, |id| {
            Entity::delete_by_id(id).exec(pools.conn())
        })?;
        if delete.rows_affected == 1 {
            Ok(self.pool)
        } else {
            Err(anyhow!("Didn't delete exactly one row: {}", delete.rows_affected).into())
        }
    }
    pub async fn reset(&self, pools: &Pools) -> Result<Dice, Error> {
        // should transaction but &self not 'static, but op is idempotent so nbd
        let original_size = match_pool_id!(self.id, |id| ActiveModel {
            id: Unchanged(id),
            current_size: Set(self.original_size.dice as i16),
            updated: Set(chrono::Utc::now()),
            ..Default::default()
        }
        .update(pools.conn())
        .map_ok(|_| self.original_size))?;
        Ok(original_size)
    }
}

impl From<server_pool::Model> for PoolInDb {
    fn from(pool: server_pool::Model) -> Self {
        PoolInDb::new(
            Dice::from(pool.current_size as u8),
            pool.name,
            PoolId::Server(pool.id),
            Dice::from(pool.original_size as u8),
        )
    }
}
impl From<channel_pool::Model> for PoolInDb {
    fn from(pool: channel_pool::Model) -> Self {
        PoolInDb::new(
            Dice::from(pool.current_size as u8),
            pool.name,
            PoolId::Channel(pool.id),
            Dice::from(pool.original_size as u8),
        )
    }
}

pub struct Pools {
    conn: DatabaseConnection,
}

impl Pools {
    pub fn new(conn: DatabaseConnection) -> Self {
        Self { conn }
    }
    pub fn conn(&self) -> &DatabaseConnection {
        &self.conn
    }
    pub async fn get(&self, scope: Scope, pool_name: &str) -> Result<PoolInDb, MoxieError> {
        let conn = &self.conn;
        match_scope!(scope, |id| async move {
            let pool = Entity::find()
                .filter(Id.eq(id))
                .filter(Column::Name.like(pool_name))
                .one(conn)
                .await?
                .ok_or_else(|| MoxieError::PoolNotFound(pool_name.to_string()))?;
            Ok(PoolInDb::from(pool))
        })
    }
    pub async fn create(
        &self,
        scope: Scope,
        pool_name: String,
        pool_size: Dice,
    ) -> Result<(), Error> {
        if self.get(scope, &pool_name).await.is_ok() {
            Err(anyhow!("Pool `{pool_name}` already exists!").into())
        } else {
            let pool_size = pool_size.dice as i16;
            match scope {
                Scope::Server(server_id) => {
                    let new_pool = server_pool::ActiveModel {
                        server_id: Set(server_id.get() as i64),
                        name: Set(pool_name),
                        original_size: Set(pool_size),
                        current_size: Set(pool_size),
                        ..Default::default()
                    };
                    new_pool.insert(&self.conn).await?;
                }
                Scope::Channel(channel_id) => {
                    let new_pool = channel_pool::ActiveModel {
                        channel_id: Set(channel_id.get() as i64),
                        name: Set(pool_name),
                        original_size: Set(pool_size),
                        current_size: Set(pool_size),
                        ..Default::default()
                    };
                    new_pool.insert(&self.conn).await?;
                }
            };
            Ok(())
        }
    }
    pub async fn delete(&self, scope: Scope, pool_name: &str) -> Result<Pool, Error> {
        let pool = self.get(scope, pool_name).await?;
        pool.delete(self).await
    }

    #[expect(dead_code, reason = "will be used when auto clean is implemented")]
    pub async fn clear_old_pools(&self) -> Result<(), sea_orm::DbErr> {
        let server_res = server_pool::Entity::delete_many()
            .filter(
                server_pool::Column::Updated.lt(chrono::Utc::now() - chrono::Duration::days(30)),
            )
            .exec(&self.conn)
            .await;
        let channel_res = channel_pool::Entity::delete_many()
            .filter(
                channel_pool::Column::Updated.lt(chrono::Utc::now() - chrono::Duration::days(30)),
            )
            .exec(&self.conn)
            .await;
        server_res.and(channel_res).map(|_| ())
    }

    pub async fn set(&self, pool: &mut PoolInDb, num_dice: SetValue) -> Result<Dice, Error> {
        let new_size = num_dice.apply(pool.pool.dice());
        pool.pool.set_dice(new_size);
        match_pool_id!(pool.id, |id| ActiveModel {
            id: Unchanged(id),
            current_size: Set(new_size.dice as i16),
            updated: Set(chrono::Utc::now()),
            ..Default::default()
        }
        .update(&self.conn)
        .map_ok(|_| ()))?;
        Ok(new_size)
    }
    pub async fn set_max(
        &self,
        pool: &mut PoolInDb,
        num_dice: SetValue,
        reset: bool,
    ) -> Result<Dice, Error> {
        let new_size = num_dice.apply(pool.pool.dice());
        let mut current_size = NotSet;

        if reset {
            pool.pool.set_dice(new_size);
            current_size = Set(new_size.dice as i16);
        }

        match_pool_id!(pool.id, |id| ActiveModel {
            id: Unchanged(id),
            current_size,
            original_size: Set(new_size.dice as i16),
            updated: Set(chrono::Utc::now()),
            ..Default::default()
        }
        .update(&self.conn)
        .map_ok(|_| ()))?;

        Ok(new_size)
    }
    pub async fn reset(&self, scope: Scope, pool_name: &str) -> Result<Dice, Error> {
        // should transaction but &self not 'static, but op is idempotent so nbd
        let pool = self.get(scope, pool_name).await?;
        pool.reset(self).await
    }
    pub async fn list<'a>(
        &'a self,
        scope: Scope,
        pool_search_string: &str,
        limit: Option<u64>,
        offset: Option<u64>,
    ) -> Result<impl Stream<Item = Result<PoolInDb, sea_orm::DbErr>> + 'a, Error> {
        match_scope!(scope, |id| async move {
            let pools = Entity::find()
                .filter(Id.eq(id))
                .filter(Column::Name.contains(pool_search_string))
                .order_by_desc(Column::Updated)
                .limit(limit)
                .offset(offset)
                .stream(&self.conn)
                .await?
                .map_ok(PoolInDb::from);
            Ok(Either(pools))
        })
    }
}
