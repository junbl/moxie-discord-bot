use crate::{Context, Error};
use itertools::Itertools;
use poise::CreateReply;
use rand::Rng;
use rand::seq::IndexedRandom;
use strum::{EnumIter, EnumString, IntoEnumIterator, IntoStaticStr};

/// A d66 table to roll on to get a particular result.
#[derive(serde::Deserialize)]
struct Crucible(Vec<Vec<String>>);

#[derive(Debug, Clone, Copy, EnumString, EnumIter, IntoStaticStr)]
#[strum(serialize_all = "snake_case")]
enum CrucibleName {
    Age,
    Barrier,
    Body,
    Building,
    CityName,
    Clothing,
    Curiosity,
    Danger,
    DruidTell,
    Eyes,
    Faction,
    Gm,
    Herbalism,
    Heritage,
    Instrument,
    MartialArts,
    AmericanNames,
    Patron,
    Settlement,
    Site,
    SpellTheorem,
    Voice,
    WeaponOrigin,
    WildSurge,
}

async fn get_crucible(crucible_name: CrucibleName) -> anyhow::Result<Vec<Crucible>> {
    let paths = match crucible_name {
        CrucibleName::Age => vec!["age"],
        CrucibleName::Barrier => vec!["barrier1", "barrier2"],
        CrucibleName::Body => vec!["body"],
        CrucibleName::Building => vec!["building1", "building2"],
        CrucibleName::CityName => vec!["city_names1", "city_names2"],
        CrucibleName::Clothing => vec!["clothing"],
        CrucibleName::Curiosity => vec!["curiosities1", "curiosities2"],
        CrucibleName::Danger => vec!["dangers1", "dangers2"],
        CrucibleName::DruidTell => vec!["druid_tells"],
        CrucibleName::Eyes => vec!["eyes"],
        CrucibleName::Faction => vec!["factions1", "factions2"],
        CrucibleName::Gm => vec!["gm1", "gm2"],
        CrucibleName::Herbalism => vec!["herbalism_name", "herbalism_form"],
        CrucibleName::Heritage => vec!["heritage_people", "heritage_mood", "heritage_land"],
        CrucibleName::Instrument => vec!["bard_instrument"],
        CrucibleName::MartialArts => vec![
            "martial_arts_concept",
            "martial_arts_stance",
            "martial_arts_weapon",
        ],
        CrucibleName::AmericanNames => vec!["names_family", "names_given"],
        CrucibleName::Patron => vec!["patron_desires", "patron_entity"],
        CrucibleName::Settlement => vec!["settlement1", "settlement2"],
        CrucibleName::Site => vec!["site1", "site2"],
        CrucibleName::SpellTheorem => vec!["spell_style", "spell_essence", "spell_form"],
        CrucibleName::Voice => vec!["voice"],
        CrucibleName::WeaponOrigin => vec!["weapon_origin"],
        CrucibleName::WildSurge => vec!["wild_surge1", "wild_surge2"],
    };

    let mut crucibles = vec![];
    for path in paths {
        let path = format!("src/commands/crucibles/{path}.json");
        tracing::info!(path, "reading crucible from file");
        let s = tokio::fs::read_to_string(path).await?;
        let crucible: Crucible = serde_json::from_str(&s)?;
        crucibles.push(crucible);
    }
    Ok(crucibles)
}

/// Rolls on a crucible.
#[poise::command(slash_command)]
#[tracing::instrument(skip(ctx))]
pub async fn crucible(
    ctx: Context<'_>,
    #[autocomplete = crucible_autocomplete]
    #[description = "Which crucible to roll"]
    crucible_name: CrucibleName,
) -> Result<(), Error> {
    let mut crucibles = get_crucible(crucible_name).await?;

    let separator = match crucible_name {
        CrucibleName::Herbalism => "",
        _ => " ",
    };
    let crucible_result_message = {
        let mut rng = rand::rng();
        if matches!(crucible_name, CrucibleName::SpellTheorem) {
            let removed = rng.random_range(..crucibles.len());
            crucibles.remove(removed);
        }
        crucibles
            .into_iter()
            .map(|crucible| {
                let row = crucible
                    .0
                    .choose(&mut rng)
                    .expect("crucible shouldn't be empty");
                let cell = row.choose(&mut rng).expect("crucible shouldn't be empty");
                cell.to_string()
            })
            .join(separator)
    };
    let message = CreateReply::default().content(format!(
        "Rolled on crucible `{}`:\n## {crucible_result_message}",
        <&'static str>::from(crucible_name).replace("_", " "),
    ));
    ctx.send(message).await?;
    Ok(())
}

fn crucible_autocomplete<'a>(
    _ctx: Context<'a>,
    partial: &'a str,
) -> std::future::Ready<Vec<String>> {
    std::future::ready(
        CrucibleName::iter()
            .map(<&'static str>::from)
            .filter(|c| c.contains(partial))
            .map(ToString::to_string)
            .collect(),
    )
}
