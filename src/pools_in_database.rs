//! This module contains the code for performing operations on pools in the database.

use anyhow::anyhow;
use entity::{channel_pool, server_pool};
use sea_orm::ActiveValue::{Set, Unchanged};
use sea_orm::DatabaseConnection;
use sea_orm::{ActiveModelTrait, ColumnTrait, EntityTrait, QueryFilter};
use serenity::futures::TryFutureExt;

use crate::commands::pool::SetValue;
use crate::commands::Scope;
use crate::error::MoxieError;
use crate::rolls::{Pool, Roll, Rolls};
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
                use entity::server_pool::Column::ServerId as Id;
                use entity::server_pool::*;
                use PoolId::Server as PoolIdVariant;
                ($body_fn)(id.get()).await
            }
            Scope::Channel(id) => {
                use entity::channel_pool::Column::ChannelId as Id;
                use entity::channel_pool::*;
                use PoolId::Channel as PoolIdVariant;
                ($body_fn)(id.get()).await
            }
        }
    };
}
pub enum PoolId {
    Server(i32),
    Channel(i32),
}

pub struct PoolInDb {
    pub pool: Pool,
    id: PoolId,
    original_size: u8,
}
impl PoolInDb {
    pub fn new(dice: u8, id: PoolId, original_size: u8) -> Self {
        Self {
            pool: Pool::new(dice),
            id,
            original_size,
        }
    }
    pub fn original_size(&self) -> u8 {
        self.original_size
    }
    pub async fn roll(
        &mut self,
        conn: &DatabaseConnection,
        rolls: &Rolls,
    ) -> Result<Vec<Roll>, anyhow::Error> {
        let rolls = self.pool.roll(rolls);
        match_pool_id!(self.id, |id| async move {
            ActiveModel {
                id: Unchanged(id),
                current_size: Set(self.pool.dice() as i16),
                updated: Set(chrono::Utc::now()),
                ..Default::default()
            }
            .update(conn)
            .await
            .map(|_| ())
        })?;
        Ok(rolls)
    }
    pub async fn delete(self, conn: &DatabaseConnection) -> Result<Pool, Error> {
        let delete = match_pool_id!(self.id, |id| { Entity::delete_by_id(id).exec(conn) })?;
        if delete.rows_affected == 1 {
            Ok(self.pool)
        } else {
            Err(anyhow!("Didn't delete exactly one row: {}", delete.rows_affected).into())
        }
    }
}

pub struct Pools {
    conn: DatabaseConnection,
}

impl Pools {
    pub fn new(conn: DatabaseConnection) -> Self {
        Self { conn }
    }
    pub async fn roll(
        &self,
        scope: Scope,
        pool_name: &str,
        rolls: &Rolls,
    ) -> Result<Option<(PoolInDb, Vec<Roll>)>, anyhow::Error> {
        let mut pool = match self.get(scope, pool_name).await {
            Ok(p) => Ok(p),
            Err(MoxieError::PoolNotFound) => {
                return Ok(None);
            }
            Err(e) => Err(e),
        }?;
        let rolls = pool.roll(&self.conn, rolls).await?;
        Ok(Some((pool, rolls)))
    }
    pub async fn get(&self, scope: Scope, pool_name: &str) -> Result<PoolInDb, MoxieError> {
        let conn = &self.conn;
        match_scope!(scope, |id| async move {
            let pool = Entity::find()
                .filter(Id.eq(id))
                .filter(Column::Name.eq(pool_name))
                .one(conn)
                .await?
                .ok_or(MoxieError::PoolNotFound)?;
            Ok(PoolInDb::new(
                pool.current_size as u8,
                PoolIdVariant(pool.id),
                pool.original_size as u8,
            ))
        })
    }
    pub async fn create(
        &self,
        scope: Scope,
        pool_name: String,
        pool_size: u8,
    ) -> Result<(), Error> {
        if self.get(scope, &pool_name).await.is_ok() {
            Err(anyhow!("Pool \"{pool_name}\" already exists!").into())
        } else {
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
        pool.delete(&self.conn).await
    }

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

    pub async fn set(
        &self,
        scope: Scope,
        pool_name: &str,
        num_dice: SetValue,
    ) -> Result<u8, Error> {
        let pool = self.get(scope, pool_name).await?;
        let new_size = match num_dice {
            SetValue::Add(add) => pool.pool.dice() + add,
            SetValue::Subtract(sub) => pool.pool.dice() - sub,
            SetValue::Set(set) => set,
        };
        match_pool_id!(pool.id, |id| ActiveModel {
            id: Unchanged(id),
            current_size: Set(new_size as i16),
            updated: Set(chrono::Utc::now()),
            ..Default::default()
        }
        .update(&self.conn)
        .map_ok(|_| ()))?;
        Ok(new_size)
    }
    pub async fn reset(&self, scope: Scope, pool_name: &str) -> Result<(), Error> {
        // should transaction but &self not 'static, but op is idempotent so nbd
        let pool = self.get(scope, pool_name).await?;
        match_pool_id!(pool.id, |id| ActiveModel {
            id: Unchanged(id),
            current_size: Set(pool.original_size as i16),
            updated: Set(chrono::Utc::now()),
            ..Default::default()
        }
        .update(&self.conn)
        .map_ok(|_| ()))?;
        Ok(())
    }
}
