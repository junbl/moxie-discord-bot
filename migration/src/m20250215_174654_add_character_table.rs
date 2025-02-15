use sea_orm_migration::prelude::*;

#[derive(DeriveMigrationName)]
pub struct Migration;

#[async_trait::async_trait]
impl MigrationTrait for Migration {
    async fn up(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .create_table(
                Table::create()
                    .table(Character::Table)
                    .if_not_exists()
                    .col(
                        ColumnDef::new(Character::Id)
                            .integer()
                            .not_null()
                            .auto_increment()
                            .primary_key(),
                    )
                    .col(ColumnDef::new(Character::UserId).big_unsigned().not_null())
                    .col(ColumnDef::new(Character::ChannelId).big_unsigned())
                    .col(ColumnDef::new(Character::Name).text().not_null())
                    .col(
                        ColumnDef::new(Character::Notes)
                            .text()
                            .not_null()
                            .default(""),
                    )
                    .col(ColumnDef::new(Character::Link).text())
                    .col(
                        ColumnDef::new(Character::Created)
                            .timestamp_with_time_zone()
                            .not_null()
                            .default(Expr::current_timestamp()),
                    )
                    .col(
                        ColumnDef::new(Character::Updated)
                            .timestamp_with_time_zone()
                            .not_null()
                            .default(Expr::current_timestamp()),
                    )
                    .take(),
            )
            .await?;
        manager
            .create_index(
                Index::create()
                    .table(Character::Table)
                    .col(Character::Name)
                    .take(),
            )
            .await?;
        Ok(())
    }

    async fn down(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .drop_table(Table::drop().table(Character::Table).to_owned())
            .await?;
        Ok(())
    }
}

#[derive(DeriveIden)]
pub enum Character {
    Table,
    Id,
    UserId,
    ChannelId,
    Name,
    Notes,
    Link,
    Created,
    Updated,
}
