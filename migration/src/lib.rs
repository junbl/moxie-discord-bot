pub use sea_orm_migration::prelude::*;

mod m20240801_042205_create_tables;
mod m20241128_194627_add_suspense_table;
mod m20250215_174654_add_character_table;
mod m20250215_205851_add_active_character_table;

pub struct Migrator;

#[async_trait::async_trait]
impl MigratorTrait for Migrator {
    fn migrations() -> Vec<Box<dyn MigrationTrait>> {
        vec![
            Box::new(m20240801_042205_create_tables::Migration),
            Box::new(m20241128_194627_add_suspense_table::Migration),
            Box::new(m20250215_174654_add_character_table::Migration),
            Box::new(m20250215_205851_add_active_character_table::Migration),
        ]
    }
}
