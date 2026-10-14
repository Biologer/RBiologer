# The list of abailble servers
BIOLOGER_URLS <- c(
  "rs" = "https://biologer.rs/api",
  "hr" = "https://biologer.hr/api",
  "ba" = "https://biologer.ba/api",
  "me" = "https://biologer.me/api",
  "dev" = "https://dev.biologer.rs/api"
)

FIELD_OBS_COLUMNS <- c(
  "id", "day", "month", "year", "location", "latitude", "longitude", "mgrs10k",
  "accuracy", "elevation", "photos", "observer", "identifier", "license", "sex",
  "stage_id", "number", "note", "project", "habitat", "found_on", "found_dead",
  "found_dead_note", "time", "status", "types", "dataset", "atlas_code",
  "timed_count_id", "activity",
  "taxon.id", "taxon.name",
  "observed_by.id", "identified_by.id"
)

LITERATURE_OBS_COLUMNS <- c(
  "id", "year", "month", "day", "elevation", "minimum_elevation",
  "maximum_elevation", "latitude", "longitude", "mgrs10k", "location",
  "accuracy", "georeferenced_by", "georeferenced_date", "observer", "identifier",
  "note", "sex", "number", "project", "found_on", "habitat", "stage_id",
  "time", "dataset", "is_original_data", "cited_publication_id",
  "place_where_referenced_in_publication", "original_date", "original_locality",
  "original_elevation", "original_coordinates", "original_identification",
  "original_identification_validity", "other_original_data", "collecting_start_year",
  "collecting_start_month", "collecting_end_year", "collecting_end_month",
  "taxon.id", "taxon.name",
  "publication.id", "publication.year", "publication.authors", "publication.citation"
)

TAXA_COLUMNS <- c(
  "id", "parent_id", "name", "rank", "rank_level", "author", "restricted",
  "allochthonous", "invasive", "uses_atlas_codes", "ancestors_names",
  "translations", "stages", "synonyms"
)

DWC_COLUMN_ORDER <- c(
  "occurrenceID", "taxonID", "kingdom", "subkingdom", "infrakingdom", "phylum",
  "subphylum", "class", "subclass", "order", "suborder", "infraorder",
  "superfamily", "family", "subfamily", "tribe", "subtribe", "genus",
  "specificEpithet", "scientificNameAuthorship", "infraspecificEpithet",
  "scientificName", "acceptedNameUsage", "previousIdentifications", "taxonRank",
  "vernacularName", "verbatimScientificName", "taxonomicStatus", "identifiedBy", "dateIdentified",
  "basisOfRecord", "dcterms:type", "typeOfRecord", "dcterms:rightsHolder",
  "dcterms:accessRights", "dcterms:license", "recordedBy",
  "decimalLatitude", "decimalLongitude", "coordinateUncertaintyInMeters",
  "locality", "georeferencedBy", "georeferencedDate",
  "minimumElevationInMeters", "maximumElevationInMeters",
  "eventTime", "eventDate", "day", "month", "year", "modified",
  "lifeStage", "sex", "individualCount",
  "associatedMedia", "habitat", "substrate", "occurrenceRemarks", "dynamicProperties",
  "occurrenceDetails", "verbatimEventDate", "verbatimLocality", "verbatimElevation",
  "verbatimCoordinates", "verbatimIdentification", "identificationVerificationStatus",
  "verbatimLabel", "bibliographicCitation"
)

atlas_code_map <- c(
  "0"  = "Observed but suspected to be still on migration or to be summering non-breeder (Possible Breeding)",
  "1"  = "Observed in breeding season in possible nesting habitat (Possible Breeding)",
  "2"  = "Singing male(s) present (or breeding calls heard) in breeding season (Possible Breeding)",
  "3"  = "Pair observed in suitable nesting habitat in breeding season (Probable Breeding)",
  "4"  = "Permanent territory presumed (Probable Breeding)",
  "5"  = "Courtship and display (Probable Breeding)",
  "6"  = "Visiting probable nest site (Probable Breeding)",
  "7"  = "Agitated behaviour or anxiety calls from adults (Probable Breeding)",
  "8"  = "Brood patch on adult examined in the hand (Probable Breeding)",
  "9"  = "Nest building or excavating nest-hole (Confirmed Breeding)",
  "10" = "Distraction-display or injury-feigning (Confirmed Breeding)",
  "11" = "Used nest or eggshells found (Confirmed Breeding)",
  "12" = "Recently fledged young or downy young (Confirmed Breeding)",
  "13" = "Adults entering or leaving nest-site indicating occupied nest or adult seen incubating (Confirmed Breeding)",
  "14" = "Adult carrying faecal sac or food for young (Confirmed Breeding)",
  "15" = "Nest containing eggs (Confirmed Breeding)",
  "16" = "Nest with young seen or heard (Confirmed Breeding)"
)

map_gbif_to_biologer <- function(gbif_raw) {
  data.frame(
    occurrenceID = if ("occurrenceID" %in% names(gbif_raw)) {
      as.character(gbif_raw$occurrenceID)
    } else {
      as.character(gbif_raw$key)
    },
    taxonID = NA_character_,
    kingdom = if ("kingdom" %in% names(gbif_raw)) gbif_raw$kingdom else NA_character_,
    subkingdom = NA_character_,
    infrakingdom = NA_character_,
    phylum = if ("phylum" %in% names(gbif_raw)) gbif_raw$phylum else NA_character_,
    subphylum = NA_character_,
    class = if ("class" %in% names(gbif_raw)) gbif_raw$class else NA_character_,
    subclass = NA_character_,
    order = if ("order" %in% names(gbif_raw)) gbif_raw$order else NA_character_,
    suborder = NA_character_,
    infraorder = NA_character_,
    superfamily = NA_character_,
    family = if ("family" %in% names(gbif_raw)) gbif_raw$family else NA_character_,
    subfamily = NA_character_,
    tribe = NA_character_,
    subtribe = NA_character_,
    genus = if ("genus" %in% names(gbif_raw)) gbif_raw$genus else NA_character_,
    specificEpithet = if ("specificEpithet" %in% names(gbif_raw)) gbif_raw$specificEpithet else NA_character_,
    scientificNameAuthorship = if ("scientificNameAuthorship" %in% names(gbif_raw)) {
      as.character(gbif_raw$scientificNameAuthorship)
    } else {
      NA_character_
    },
    infraspecificEpithet = if ("infraspecificEpithet" %in% names(gbif_raw)) {
      as.character(gbif_raw$infraspecificEpithet)
    } else {
      NA_character_
    },
    scientificName = if ("species" %in% names(gbif_raw)) {
      gbif_raw$species
    } else if ("scientificName" %in% names(gbif_raw)) {
      gbif_raw$scientificName
    } else {
      rep(NA_character_, nrow(gbif_raw))
    },
    acceptedNameUsage = if ("acceptedScientificName" %in% names(gbif_raw)) {
      gbif_raw$acceptedScientificName
    } else {
      NA_character_
    },
    previousIdentifications = NA_character_,
    taxonRank = if ("taxonRank" %in% names(gbif_raw)) tolower(gbif_raw$taxonRank) else NA_character_,
    vernacularName = NA_character_,
    verbatimScientificName = NA_character_,
    taxonomicStatus = if ("taxonomicStatus" %in% names(gbif_raw)) gbif_raw$taxonomicStatus else NA_character_,
    identifiedBy = {
      id_val <- if ("identifiedBy" %in% names(gbif_raw)) {
        as.character(gbif_raw$identifiedBy)
      } else {
        rep(NA_character_, nrow(gbif_raw))
      }
      id_ids_val <- if ("identifiedByIDs" %in% names(gbif_raw)) {
        as.character(gbif_raw$identifiedByIDs)
      } else {
        rep(NA_character_, nrow(gbif_raw))
      }
      ifelse(!is.na(id_val) & id_val != "", id_val,
        ifelse(!is.na(id_ids_val) & id_ids_val != "", id_ids_val, NA_character_)
      )
    },
    dateIdentified = NA_character_,
    basisOfRecord = if ("basisOfRecord" %in% names(gbif_raw)) gbif_raw$basisOfRecord else NA_character_,
    `dcterms:type` = NA_character_,
    typeOfRecord = {
      bor <- if ("basisOfRecord" %in% names(gbif_raw)) {
        as.character(gbif_raw$basisOfRecord)
      } else {
        rep(NA_character_, nrow(gbif_raw))
      }
      ifelse(!is.na(bor) & bor != "", bor, "HumanObservation")
    },
    `dcterms:rightsHolder` = {
      pub <- if ("publishingOrgKey" %in% names(gbif_raw)) {
        as.character(gbif_raw$publishingOrgKey)
      } else {
        rep(NA_character_, nrow(gbif_raw))
      }
      pub_country <- if ("publishingCountry" %in% names(gbif_raw)) {
        as.character(gbif_raw$publishingCountry)
      } else {
        rep(NA_character_, nrow(gbif_raw))
      }
      ifelse(!is.na(pub) & pub != "", pub,
        ifelse(!is.na(pub_country) & pub_country != "", pub_country, NA_character_)
      )
    },
    `dcterms:accessRights` = {
      lic <- if ("license" %in% names(gbif_raw)) as.character(gbif_raw$license) else rep(NA_character_, nrow(gbif_raw))
      ifelse(!is.na(lic) & lic != "", lic, NA_character_)
    },
    `dcterms:license` = if ("license" %in% names(gbif_raw)) gbif_raw$license else NA_character_,
    recordedBy = {
      rec_val <- if ("recordedBy" %in% names(gbif_raw)) {
        as.character(gbif_raw$recordedBy)
      } else {
        rep(NA_character_, nrow(gbif_raw))
      }
      rec_ids_val <- if ("recordedByIDs" %in% names(gbif_raw)) {
        as.character(gbif_raw$recordedByIDs)
      } else {
        rep(NA_character_, nrow(gbif_raw))
      }
      ifelse(!is.na(rec_val) & rec_val != "", rec_val,
        ifelse(!is.na(rec_ids_val) & rec_ids_val != "", rec_ids_val, NA_character_)
      )
    },
    decimalLatitude = gbif_raw$decimalLatitude,
    decimalLongitude = gbif_raw$decimalLongitude,
    coordinateUncertaintyInMeters = if ("coordinateUncertaintyInMeters" %in% names(gbif_raw)) {
      gbif_raw$coordinateUncertaintyInMeters
    } else {
      NA_real_
    },
    locality = NA_character_,
    georeferencedBy = NA_character_,
    georeferencedDate = NA_character_,
    minimumElevationInMeters = NA_real_,
    maximumElevationInMeters = NA_real_,
    eventTime = NA_character_,
    eventDate = if ("eventDate" %in% names(gbif_raw)) gbif_raw$eventDate else NA_character_,
    day = if ("day" %in% names(gbif_raw)) gbif_raw$day else NA_integer_,
    month = if ("month" %in% names(gbif_raw)) gbif_raw$month else NA_integer_,
    year = if ("year" %in% names(gbif_raw)) gbif_raw$year else NA_integer_,
    modified = NA_character_,
    lifeStage = {
      ls_val <- as.character(gbif_raw$lifeStage)
      if ("lifeStage" %in% names(gbif_raw)) {
        ifelse(!is.na(ls_val) & ls_val != "", ls_val, NA_character_)
      } else {
        NA_character_
      }
    },
    sex = {
      sex_val <- as.character(gbif_raw$sex)
      if ("sex" %in% names(gbif_raw)) {
        ifelse(!is.na(sex_val) & sex_val != "", sex_val, NA_character_)
      } else {
        NA_character_
      }
    },
    individualCount = if ("individualCount" %in% names(gbif_raw)) gbif_raw$individualCount else NA_integer_,
    associatedMedia = NA_character_,
    habitat = NA_character_,
    substrate = NA_character_,
    occurrenceRemarks = NA_character_,
    dynamicProperties = NA_character_,
    occurrenceDetails = NA_character_,
    verbatimEventDate = NA_character_,
    verbatimLocality = NA_character_,
    verbatimElevation = NA_character_,
    verbatimCoordinates = NA_character_,
    verbatimIdentification = NA_character_,
    identificationVerificationStatus = NA_character_,
    verbatimLabel = NA_character_,
    bibliographicCitation = NA_character_,
    mgrs10k = NA_character_,
    status = "approved",
    types = NA_character_,
    dataset = if ("datasetName" %in% names(gbif_raw)) {
      gbif_raw$datasetName
    } else {
      "GBIF"
    },
    atlas_code = NA_character_,
    timed_count_id = NA_character_,
    observed_by.id = NA_character_,
    identified_by.id = NA_character_,
    restricted = FALSE,
    allochthonous = FALSE,
    invasive = FALSE,
    translations = NA,
    synonyms = NA,
    is_original_data = FALSE,
    cited_publication_id = NA_integer_,
    publication.id = NA_integer_,
    publication.year = NA_integer_,
    publication.authors = NA_character_,
    stringsAsFactors = FALSE
  )
}

gbif_to_biologer_species_names <- data.frame(
  gbif_name = c(
    "Ochlodes venata",
    "Spialia sertorius",
    "Elkalyce alcetas",
    "Elkalyce argiades",
    "Elkalyce decolorata",
    "Fixsenia pruni",
    "Loweia tityrus",
    "Lycaeides idas",
    "Nordmannia ilicis",
    "Plebejus amanda",
    "Quercusia quercus",
    "Strymon acaciae",
    "Thersamonia thersamon",
    "Tuttiola spini",
    "Clossiana euphrosyne",
    "Coenonympha iphis",
    "Damora pandora",
    "Mellicta aurelia",
    "Colias croceus",
    "Allancastria cerisyi",
    "Yponomeuta cagnagellus",
    "Pleurota huebneri",
    "Neognopharmia stevenaria",
    "Tephrina murinaria",
    "Nebula salicata",
    "Idaea antiquaria",
    "Yigoga forcipula",
    "Pyrgus fritillarius",
    "Herculia rubidalis",
    "Maculinea arion",
    "Plebejus pylaon",
    "Manulea palliatella",
    "Manulea pseudocomplana",
    "Manulea complana",
    "Glossotrophia confinaria",
    "Satyrus briseis",
    "Epicallia villica",
    "Oecophora staintoniella",
    "Chrysocrambus craterellus",
    "Elkalyce amyntula",
    "Coloeus monedula",
    "Ardea alba",
    "Dorcadion fulvum",
    "Adela viridella"
  ),
  biologer_name = c(
    "Ochlodes sylvanus",
    "Spialia orbifer",
    "Cupido alcetas",
    "Cupido argiades",
    "Cupido decoloratus",
    "Satyrium pruni",
    "Lycaena tityrus",
    "Plebejus idas",
    "Satyrium ilicis",
    "Polyommatus amandus",
    "Favonius quercus",
    "Satyrium acaciae",
    "Lycaena thersamon",
    "Satyrium spini",
    "Boloria euphrosyne",
    "Coenonympha glycerion",
    "Argynnis pandora",
    "Melitaea aurelia",
    "Colias crocea",
    "Zerynthia cerisy",
    "Yponomeuta cagnagella",
    "Holoscolia huebneri",
    "Gnopharmia stevenaria",
    "Isturgia murinaria",
    "Coenotephria salicata",
    "Idaea macilentaria",
    "Dichagyris forcipula",
    "Pyrgus carthami",
    "Hypsopygia rubidalis",
    "Phengaris arion",
    "Kretania sephirus",
    "Eilema palliatella",
    "Eilema pseudocomplana",
    "Eilema complana",
    "Scopula confinaria",
    "Chazara briseis",
    "Arctia villica",
    "Alabonia staintoniella",
    "Chrysocrambus craterella",
    "Cupido argiades",
    "Corvus monedula",
    "Casmerodius albus",
    "Carinatodorcadion fulvum",
    "Adela reaumurella"
  )
)
