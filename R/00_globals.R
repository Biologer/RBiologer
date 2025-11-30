# The list of abailble servers
BIOLOGER_URLS <- c(
  "rs" = "https://biologer.rs/api",
  "hr" = "https://biologer.hr/api",
  "ba" = "https://biologer.ba/api",
  "me" = "https://biologer.me/api"
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
  "publication.id", "publication.year", "publication.authors"
)

TAXA_COLUMNS <- c(
  "id", "parent_id", "name", "rank", "rank_level", "author", "restricted",
  "allochthonous", "invasive", "uses_atlas_codes", "ancestors_names",
  "translations", "stages", "synonyms"
)