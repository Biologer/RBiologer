# The list of abailble servers
BIOLOGER_URLS <- c(
  "rs" = "https://biologer.rs/api",
  "hr" = "https://biologer.hr/api",
  "ba" = "https://biologer.ba/api",
  "me" = "https://biologer.me/api",
  "dev" = "https://dev.biologer.org/api"
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

DWC_COLUMN_ORDER <- c("occurrenceID", "taxonID", "kingdom", "subkingdom", "infrakingdom", "phylum",
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
                      "verbatimLabel", "bibliographicCitation")

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