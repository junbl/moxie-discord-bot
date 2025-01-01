use sea_orm_migration::prelude::*;

#[derive(DeriveMigrationName)]
pub struct Migration;

#[async_trait::async_trait]
impl MigrationTrait for Migration {
    async fn up(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .create_table(
                Table::create()
                    .table(Suspense::Table)
                    .if_not_exists()
                    .col(
                        ColumnDef::new(Suspense::Id)
                            .integer()
                            .not_null()
                            .auto_increment()
                            .primary_key(),
                    )
                    .col(
                        ColumnDef::new(Suspense::ChannelId)
                            .big_unsigned()
                            .not_null(),
                    )
                    .col(ColumnDef::new(Suspense::Challenge).string())
                    .col(
                        ColumnDef::new(Suspense::Suspense)
                            .small_unsigned()
                            .not_null(),
                    )
                    .col(
                        ColumnDef::new(Suspense::Created)
                            .timestamp_with_time_zone()
                            .not_null()
                            .default(Expr::current_timestamp()),
                    )
                    .col(
                        ColumnDef::new(Suspense::Updated)
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
                    .table(Suspense::Table)
                    .unique()
                    .nulls_not_distinct()
                    .col(Suspense::Challenge)
                    .col(Suspense::ChannelId)
                    .take(),
            )
            .await?;
        Ok(())
    }

    async fn down(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .drop_table(Table::drop().table(Suspense::Table).to_owned())
            .await?;
        Ok(())
    }
}

#[derive(DeriveIden)]
#[expect(clippy::enum_variant_names, reason = "so true fr fr")]
enum Suspense {
    Table,
    Id,
    ChannelId,
    Suspense,
    Challenge,
    Created,
    Updated,
}
