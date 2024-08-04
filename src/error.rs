use thiserror::Error;
#[derive(Debug, Error)]
pub enum MoxieError {
    #[error("Pool not found!")]
    PoolNotFound,
    #[error("Database error: {0}")]
    Database(#[from] sea_orm::DbErr),
}
