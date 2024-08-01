use sea_orm_migration::prelude::*;

#[derive(DeriveMigrationName)]
pub struct Migration;

#[async_trait::async_trait]
impl MigrationTrait for Migration {
    async fn up(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .create_table(
                Table::create()
                    .table(ServerPool::Table)
                    .if_not_exists()
                    .col(
                        ColumnDef::new(ServerPool::Id)
                            .integer()
                            .not_null()
                            .auto_increment()
                            .primary_key(),
                    )
                    .col(
                        ColumnDef::new(ServerPool::ServerId)
                            .big_unsigned()
                            .not_null(),
                    )
                    .col(ColumnDef::new(ServerPool::Name).string().not_null())
                    .col(
                        ColumnDef::new(ServerPool::OriginalSize)
                            .small_unsigned()
                            .not_null(),
                    )
                    .col(
                        ColumnDef::new(ServerPool::CurrentSize)
                            .small_unsigned()
                            .not_null(),
                    )
                    .take(),
            )
            .await?;
        manager
            .create_table(
                Table::create()
                    .table(ChannelPool::Table)
                    .if_not_exists()
                    .col(
                        ColumnDef::new(ChannelPool::Id)
                            .integer()
                            .not_null()
                            .auto_increment()
                            .primary_key(),
                    )
                    .col(
                        ColumnDef::new(ChannelPool::ChannelId)
                            .big_unsigned()
                            .not_null(),
                    )
                    .col(ColumnDef::new(ChannelPool::Name).string().not_null())
                    .col(
                        ColumnDef::new(ChannelPool::OriginalSize)
                            .small_unsigned()
                            .not_null(),
                    )
                    .col(
                        ColumnDef::new(ChannelPool::CurrentSize)
                            .small_unsigned()
                            .not_null(),
                    )
                    .take(),
            )
            .await?;
        Ok(())
    }

    async fn down(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .drop_table(Table::drop().table(ServerPool::Table).take())
            .await?;
        manager
            .drop_table(Table::drop().table(ChannelPool::Table).take())
            .await?;
        Ok(())
    }
}

#[derive(DeriveIden)]
enum ServerPool {
    Table,
    Id,
    ServerId,
    Name,
    OriginalSize,
    CurrentSize,
}

#[derive(DeriveIden)]
enum ChannelPool {
    Table,
    Id,
    ChannelId,
    Name,
    OriginalSize,
    CurrentSize,
}
