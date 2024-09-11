use thiserror::Error;
#[derive(Debug, Error)]
pub enum MoxieError {
    #[error("Pool `{0}` not found!")]
    PoolNotFound(String),
    #[error("Database error: {0}")]
    Database(#[from] sea_orm::DbErr),
}
