#' Summary of Data within a Polygon
#'
#' @param polygon An sf or terra spatial object defining the area of interest.
#' @param kingdom Return only the data from given kingdom.
#' @param phylum Return only the data from given phylum
#' @param class Return only the data from given class
#' @param order Return only the data from given order
#' @param area_buffer Numeric. Buffer distance in meters (default 50).
#' @param region_buffer Numeric. Buffer distance in meters (default 10,000).
#' @param auto.download Logical. Automatically download new data?
#' @param verbose Logical. Print progress messages?
#'
#' @return A summary data frame/table.
#' @export
#'
#' @import sf
#' @import terra
#' @import giscoR
#' @import rgbif
species_within_polygon <- function(
  polygon = NULL,
  kingdom = NULL,
  phylum = NULL,
  class = NULL,
  order = NULL,
  country = "all",
  area_buffer = 0,
  region_buffer = 10000,
  auto.download = TRUE,
  verbose = TRUE,
  get_gbif = FALSE
) {
  if (is.null(polygon)) {
    stop("You must provide study area polygon (i.e. path to KML or SHP polygon file).", call. = FALSE)
  }

  # Get the Biologer data
  biologer_data <- open_data(auto.download, verbose)
  biologer_data <- biologer_data[is.na(biologer_data$status) | biologer_data$status == "approved", ]
  biologer_data <- biologer_data[!is.na(biologer_data$taxonID), ]
  biologer_data <- as.data.frame(biologer_data)

  # Load study area and create buffers
  study_area <- terra::vect(polygon)
  study_area_laea <- terra::project(study_area, "EPSG:3035")
  area_buf_laea <- terra::buffer(study_area_laea, width = area_buffer)
  region_buf_laea <- terra::buffer(study_area_laea, width = region_buffer)
  study_area_buffer <- terra::aggregate(terra::project(area_buf_laea, "EPSG:4326"))
  study_region <- terra::aggregate(terra::project(region_buf_laea, "EPSG:4326"))
  rm(study_area_laea, area_buf_laea, region_buf_laea)

  # Load taxonomic data from local cache used for queries and naming GBIF dataset
  taxa_file <- file.path(get_storage_path(), paste0("biologer_taxa_", country, ".csv"))
  taxa_df <- NULL
  if (file.exists(taxa_file)) {
    taxa_df <- data.table::fread(taxa_file, stringsAsFactors = FALSE, fill = TRUE)
    if ("id" %in% names(taxa_df)) taxa_df$id <- as.character(taxa_df$id)
  } else {
    if (verbose) message(sprintf("⚠️ Local taxa cache not found at: %s", taxa_file))
  }

  # Get the GBIF dataset and merge it to the Biologer dataset
  if (get_gbif) {
    if (verbose) message("Preparing spatial tiles for GBIF batch download...")

    region_laea <- terra::project(study_region, "EPSG:3035")
    tile_raster <- terra::rast(region_laea, res = 5000)
    tiles_poly <- terra::as.polygons(tile_raster, values = FALSE)
    tiles_intersect <- terra::intersect(tiles_poly, region_laea)
    tiles_wgs84 <- terra::project(tiles_intersect, "EPSG:4326")
    total_tiles <- terra::nrow(tiles_wgs84)

    if (verbose) message(sprintf("Study region split into %d tiles for GBIF querying.", total_tiles))

    gbif_list <- list()
    max_attempts <- 3
    for (t_idx in seq_len(total_tiles)) {
      single_tile <- tiles_wgs84[t_idx, ]

      # Convert tile SpatVector to sf for WKT formatting
      tile_sf <- sf::st_as_sf(single_tile)
      wkt_tile <- sf::st_as_text(sf::st_geometry(tile_sf))

      if (verbose) message(sprintf("Fetching GBIF tile %d of %d...", t_idx, total_tiles))

      tile_res <- NULL
      for (attempt in 1:max_attempts) {
        tile_res <- tryCatch(
          {
            rgbif::occ_search(
              geometry = wkt_tile,
              hasCoordinate = TRUE,
              limit = 100000,
              geom_big = "bbox"
            )
          },
          error = function(e) {
            NULL
          }
        )

        if (!is.null(tile_res)) break
        Sys.sleep(5)
      }

      # Check if data exists and is a non-empty data frame
      if (!is.null(tile_res$data) && is.data.frame(tile_res$data) && nrow(tile_res$data) > 0) {
        tile_df <- as.data.frame(tile_res$data)

        # Clean the data. List to vector for later merging...
        list_cols <- sapply(tile_df, is.list)
        if (any(list_cols)) {
          tile_df[list_cols] <- lapply(tile_df[list_cols], function(col) {
            sapply(
              col,
              function(x) if (is.null(x) || length(x) == 0) NA_character_ else paste(as.character(x), collapse = "; ")
            )
          })
        }

        gbif_list[[length(gbif_list) + 1]] <- tile_df
      }
      Sys.sleep(1)
    }

    if (length(gbif_list) > 0) {
      gbif_raw <- as.data.frame(data.table::rbindlist(gbif_list, fill = TRUE, use.names = TRUE))

      # Remove exact duplicates if any overlap occurred
      if ("key" %in% names(gbif_raw)) {
        gbif_raw <- gbif_raw[!duplicated(gbif_raw$key), ]
      } else {
        gbif_raw <- unique(gbif_raw)
      }

      if (verbose) message(sprintf("Successfully fetched %d total records from GBIF across all tiles.", nrow(gbif_raw)))

      # Map GBIF columns to match Biologer structure
      gbif_df <- map_gbif_to_biologer(gbif_raw)

      # Harmonize GBIF taxonomy with Biologer's master taxa list
      # 1. Fill the verbatimScientificName with current GBIF species name
      if ("acceptedNameUsage" %in% names(gbif_df)) {
        gbif_df$verbatimScientificName <- ifelse(
          is.na(gbif_df$verbatimScientificName) | gbif_df$verbatimScientificName == "",
          gbif_df$acceptedNameUsage,
          gbif_df$verbatimScientificName
        )
      }
      # 2. Replace GBIF scientificName with Biologer scientificName
      manual_match <- match(gbif_df$scientificName, gbif_to_biologer_species_names$gbif_name)
      manual_rows <- !is.na(manual_match)
      if (any(manual_rows)) {
        gbif_df$scientificName[manual_rows] <- gbif_to_biologer_species_names$biologer_name[manual_match[manual_rows]]
        if (verbose) message(sprintf("Biologer taxonomy applied to %d GBIF records.", sum(manual_rows)))
      }
      # 3. Get the author name from the Biologer taxonomic table
      match_idx <- match(gbif_df$scientificName, taxa_df$name)
      matched_rows <- !is.na(match_idx)
      if (any(matched_rows)) {
        if ("author" %in% names(taxa_df) && "scientificNameAuthorship" %in% names(gbif_df)) {
          gbif_df$scientificNameAuthorship[matched_rows] <- taxa_df$author[match_idx[matched_rows]]
        }
        if ("taxonID" %in% names(gbif_df) && "id" %in% names(taxa_df)) {
          gbif_df$taxonID[matched_rows] <- as.character(taxa_df$id[match_idx[matched_rows]])
        }
      }

      # 4. Fill acceptedNameUsage as "scientificName + scientificNameAuthorship"
      if ("acceptedNameUsage" %in% names(gbif_df) &&
        "scientificName" %in% names(gbif_df) &&
        "scientificNameAuthorship" %in% names(gbif_df)) {
        gbif_df$acceptedNameUsage <- ifelse(
          !is.na(gbif_df$scientificName) &
            !is.na(gbif_df$scientificNameAuthorship) &
            gbif_df$scientificNameAuthorship != "",
          paste(gbif_df$scientificName, " ", gbif_df$scientificNameAuthorship),
          gbif_df$scientificName
        )
      }

      # 5. Split genus and specificEpithet for species-rank records
      if ("taxonRank" %in% names(gbif_df) && "scientificName" %in% names(gbif_df)) {
        species_mask <- !is.na(gbif_df$taxonRank) &
          tolower(gbif_df$taxonRank) == "species" &
          !is.na(gbif_df$scientificName)
        if (any(species_mask)) {
          name_parts <- strsplit(gbif_df$scientificName[species_mask], "\\s+")
          gbif_df$genus[species_mask] <-
            sapply(name_parts, function(x) if (length(x) >= 1) x[1] else NA_character_)
          gbif_df$specificEpithet[species_mask] <-
            sapply(name_parts, function(x) if (length(x) >= 2) x[2] else NA_character_)
        }
      }

      # Drop rows without valid species or coordinates
      gbif_df <- gbif_df[
        !is.na(gbif_df$scientificName) &
          !is.na(gbif_df$decimalLatitude) &
          !is.na(gbif_df$decimalLongitude),
      ]

      common_cols <- intersect(names(biologer_data), names(gbif_df))
      biologer_data <- rbind(
        biologer_data[, common_cols, drop = FALSE],
        gbif_df[, common_cols, drop = TRUE]
      )
    }
  }

  # Subset the dataset to the given higher taxa
  if (!is.null(order)) {
    biologer_data <- biologer_data[!is.na(biologer_data$order) & biologer_data$order == order, ]
  } else if (!is.null(class)) {
    biologer_data <- biologer_data[!is.na(biologer_data$class) & biologer_data$class == class, ]
  } else if (!is.null(phylum)) {
    biologer_data <- biologer_data[!is.na(biologer_data$phylum) & biologer_data$phylum == phylum, ]
  } else if (!is.null(kingdom)) {
    biologer_data <- biologer_data[!is.na(biologer_data$kingdom) & biologer_data$kingdom == kingdom, ]
  }

  # Convert data.frame to spatial data
  biologer_vect <- terra::vect(
    biologer_data,
    geom = c("decimalLongitude", "decimalLatitude"),
    crs = "EPSG:4326"
  )

  # Get the Country and create 1×1 km grid
  country_sf <- get_country_polygon(country = country)
  country_sf_laea <- terra::project(country_sf, "EPSG:3035")
  grid_raster <- terra::rast(country_sf_laea, res = 1000)
  grid_all <- terra::as.polygons(grid_raster, values = FALSE)
  grid_clipped <- terra::intersect(grid_all, country_sf_laea)
  grid_final <- terra::project(grid_clipped, "EPSG:4326")
  grid_final$grid_cell_id <- seq_along(grid_final)
  biologer_vect$grid_cell_id <- terra::extract(
    grid_final,
    biologer_vect
  )$grid_cell_id

  # Get the data from the polygons
  data_area <- biologer_vect[study_area_buffer, ]
  data_region <- biologer_vect[study_region, ]

  # Compile the species list for the study location and the regional buffer
  area_species <- unique(data_area$scientificName)
  area_species <- area_species[area_species != ""]
  region_species <- unique(data_region$scientificName)
  region_species <- region_species[!(region_species %in% area_species)]
  region_species <- region_species[region_species != ""]

  biologer_dt <- as.data.table(biologer_vect)
  national_species_occupancy <- unique(
    biologer_dt[
      !is.na(grid_cell_id),
      .(scientificName, grid_cell_id)
    ]
  )[
    ,
    .(occupied_cells = .N),
    by = scientificName
  ]
  area_species_occupancy <- unique(
    as.data.table(data_area)[
      !is.na(grid_cell_id),
      .(scientificName, grid_cell_id)
    ]
  )[
    ,
    .(occupied_cells = .N),
    by = scientificName
  ]

  output.list <- list()
  # Rund the query for get the taxon data by taxon ID
  for (i in seq_along(area_species)) {
    species <- area_species[i]
    if (verbose) print(paste0("Adding species ", species, "; ", i, " of ", length(area_species)))

    # Get the national coverage of the 1×1 km grids
    national_grids <- as.numeric(as.character(
      national_species_occupancy[
        national_species_occupancy$scientificName == species, 2
      ]
    ))
    area_grids <- as.numeric(as.character(
      area_species_occupancy[
        area_species_occupancy$scientificName == species, 2
      ]
    ))
    national_coverage <- round((area_grids / national_grids) * 100, digits = 2)

    taxon <- biologer_dt[biologer_dt$scientificName == species][1]
    taxon_online <- NULL
    raw_id <- taxon$taxonID
    if (is.list(raw_id)) raw_id <- unlist(raw_id)
    if (length(raw_id) == 0) raw_id <- NA_character_
    clean_id <- as.character(raw_id)

    # Check if the incoming taxonID is valid
    has_valid_id <- !is.na(clean_id) &&
      clean_id != "" &&
      tolower(clean_id) != "na" &&
      tolower(clean_id) != "null"

    if (has_valid_id) {
      # 1. The species has valid Biologer ID
      res_online <- tryCatch(
        get_taxon_by_id(clean_id, server = country),
        error = function(e) {
          message(sprintf("⚠️ API error for taxonID %s: %s", clean_id, e$message))
          NULL
        }
      )
      if (!is.null(res_online)) taxon_online <- res_online$data
    } else if (!is.null(taxa_df)) {
      # 2. Fall back to the pre-loaded local taxa table
      taxon_match <- taxa_df[!is.na(taxa_df$name) & taxa_df$name == species &
        !is.na(taxa_df$id) & taxa_df$id != "" &
        tolower(taxa_df$id) != "na" &
        tolower(taxa_df$id) != "null", ]

      if (nrow(taxon_match) > 0) {
        fallback_id <- as.character(taxon_match$id[1])
        res_online <- tryCatch(
          get_taxon_by_id(fallback_id, server = country),
          error = function(e) {
            message(sprintf("⚠️ API error for fallback ID %s (%s): %s", fallback_id, species, e$message))
            NULL
          }
        )
        if (!is.null(res_online)) taxon_online <- res_online$data
      } else {
        if (verbose) message(sprintf("⚠️ Species '%s' could not be matched to any valid ID in the local taxa CSV.", species))
      }
    }

    output.list[[i]] <- data.frame(
      Class = taxon$class,
      Order = taxon$order,
      Family = taxon$family,
      Taxon = taxon$scientificName,
      EnglishName = get_local_name(taxon$translations, "en"),
      LocalName = get_local_name(taxon$translations, country),
      BernConvention = get_bern_directive(taxon_online$conservation_legislations),
      HabitatDirective = get_habitats_directive(taxon_online$conservation_legislations),
      BirdsDirective = get_birds_directive(taxon_online$conservation_legislations),
      CMSConvention = get_cms_convention(taxon_online$conservation_legislations),
      CITESConvention = get_cites_convention(taxon_online$conservation_legislations),
      NationalLeg = get_protected_species(taxon_online$conservation_legislations),
      RedList = get_red_list(level = "Global", taxon_online$red_lists),
      RedListEu = get_red_list(level = "Europe", taxon_online$red_lists),
      RedListRS = get_red_list(level = "Serbia", taxon_online$red_lists),
      References = get_references(data_region[data_region$scientificName == species]),
      Years = paste(sort(unique(data_region[data_region$scientificName == species]$year)), collapse = ", "),
      Location = "within",
      Type = get_type_of_record(data_region[data_region$scientificName == species]$bibliographicCitation),
      NationalCoverage = national_coverage
    )
  }

  for (i in seq_along(region_species)) {
    species <- region_species[i]
    if (verbose) print(paste0("Adding species ", species, "; ", i, " of ", length(region_species)))

    taxon <- biologer_dt[biologer_dt$scientificName == species][1]
    taxon_online <- NULL

    raw_id <- taxon$taxonID
    if (is.list(raw_id)) raw_id <- unlist(raw_id)
    if (length(raw_id) == 0) raw_id <- NA_character_
    clean_id <- as.character(raw_id)

    has_valid_id <- !is.na(clean_id) &&
      clean_id != "" &&
      tolower(clean_id) != "na" &&
      tolower(clean_id) != "null"

    if (has_valid_id) {
      # 1. Valid Biologer ID
      res_online <- tryCatch(
        get_taxon_by_id(clean_id, server = country),
        error = function(e) {
          message(sprintf("⚠️ API error for taxonID %s: %s", clean_id, e$message))
          NULL
        }
      )
      if (!is.null(res_online)) taxon_online <- res_online$data
    } else if (!is.null(taxa_df)) {
      # 2. Fall back to local master taxa table
      taxon_match <- taxa_df[!is.na(taxa_df$name) & taxa_df$name == species &
        !is.na(taxa_df$id) & taxa_df$id != "" &
        tolower(taxa_df$id) != "na" &
        tolower(taxa_df$id) != "null", ]

      if (nrow(taxon_match) > 0) {
        fallback_id <- as.character(taxon_match$id[1])
        res_online <- tryCatch(
          get_taxon_by_id(fallback_id, server = country),
          error = function(e) {
            message(sprintf("⚠️ API error for fallback ID %s (%s): %s", fallback_id, species, e$message))
            NULL
          }
        )
        if (!is.null(res_online)) taxon_online <- res_online$data
      } else {
        if (verbose) message(sprintf("⚠️ Species '%s' could not be matched to any valid ID in the local taxa CSV.", species))
      }
    }

    output.list[[length(output.list) + 1]] <- data.frame(
      Class = taxon$class,
      Order = taxon$order,
      Family = taxon$family,
      Taxon = taxon$scientificName,
      EnglishName = get_local_name(taxon$translations, "en"),
      LocalName = get_local_name(taxon$translations, country),
      BernConvention = get_bern_directive(taxon_online$conservation_legislations),
      HabitatDirective = get_habitats_directive(taxon_online$conservation_legislations),
      BirdsDirective = get_birds_directive(taxon_online$conservation_legislations),
      CMSConvention = get_cms_convention(taxon_online$conservation_legislations),
      CITESConvention = get_cites_convention(taxon_online$conservation_legislations),
      NationalLeg = get_protected_species(taxon_online$conservation_legislations),
      RedList = get_red_list(level = "Global", taxon_online$red_lists),
      RedListEu = get_red_list(level = "Europe", taxon_online$red_lists),
      RedListRS = get_red_list(level = "Serbia", taxon_online$red_lists),
      References = get_references(data_region[data_region$scientificName == species]),
      Years = paste(sort(unique(data_region[data_region$scientificName == species]$year)), collapse = ", "),
      Location = "outside",
      Type = get_type_of_record(data_region[data_region$scientificName == species]$bibliographicCitation),
      NationalCoverage = NA_character_
    )
  }

  output_table <- do.call(rbind, output.list)
  output_table <- as.data.frame(output_table)
  output_table <- output_table[order(
    output_table[, 1],
    output_table[, 2],
    output_table[, 3],
    output_table[, 4]
  ), ]

  df_area <- as.data.frame(data_area, geom = "XY")
  names(df_area)[names(df_area) == "x"] <- "decimalLongitude"
  names(df_area)[names(df_area) == "y"] <- "decimalLatitude"
  df_region <- as.data.frame(data_region, geom = "XY")
  names(df_region)[names(df_region) == "x"] <- "decimalLongitude"
  names(df_region)[names(df_region) == "y"] <- "decimalLatitude"

  list(
    summary = output_table,
    data_area = df_area,
    data_region = df_region
  )
}

#' Get High-Precision Country Borders
#'
#' @param country Character vector. Options include "rs" (Serbia), "me" (Montenegro),
#'        "ba" (Bosnia and Herzegovina), "hr" (Croatia), or "all" for all four countries.
#'        Can also accept a vector of codes like c("rs", "hr") to merge specific countries.
#' @return A terra SpatVector object with the projected country boundaries.
#' @export
#'
#' @importFrom giscoR gisco_get_countries
#' @importFrom terra vect project aggregate
get_country_polygon <- function(country = "all") {
  # 1. Package Guard
  if (!requireNamespace("giscoR", quietly = TRUE)) {
    stop("Package 'giscoR' is required. Please install it.", call. = FALSE)
  }

  # 2. Map Biologer server codes to GISCO-recognized country names
  country_map <- c(
    "rs" = "Serbia",
    "me" = "Montenegro",
    "ba" = "Bosnia and Herzegovina",
    "hr" = "Croatia"
  )

  # 3. Handle the "all" keyword shortcut
  if (length(country) == 1 && country == "all") {
    target_countries <- unname(country_map)
  } else {
    # Normalize input strings to lowercase to prevent typos
    country <- tolower(country)

    # Check if input matches our allowed codes
    if (!all(country %in% names(country_map))) {
      stop(
        "Invalid country selection. Please use 'all' or combinations of: ",
        paste(names(country_map), collapse = ", "),
        call. = FALSE
      )
    }
    target_countries <- country_map[country]
  }

  # 4. Fetch the boundaries using sf (giscoR returns sf objects)
  sf_borders <- giscoR::gisco_get_countries(
    country = target_countries,
    resolution = "01"
  )

  # 5. Convert to terra SpatVector
  terra_borders <- terra::vect(sf_borders)

  # 6. Project to WGS84 explicitly to guarantee match with Biologer coordinates
  terra_borders <- terra::project(terra_borders, "EPSG:4326")

  # 7. Dissolve internal borders if multiple countries were chosen/merged
  if (nrow(terra_borders) > 1) {
    terra_borders <- terra::aggregate(terra_borders)
  }

  terra_borders
}

#' Extract Localized Species Name from a Multilingual String
#'
#' @param name_string Character. The raw multilingual string (e.g., "en=common wall lizard|hr=...")
#' @param locale Character. The language code to extract (e.g., "en", "sr", "sr-Latn", "hr")
#'
#' @return Character string containing the localized name, or NA if the locale is not found.
#' @export
get_local_name <- function(name_string, locale = "en") {
  if (is.na(name_string) || name_string == "") {
    return(NA_character_)
  }

  if (locale == "rs") {
    use_locale <- "sr"
  } else if (locale == "ba") {
    use_locale <- "bs"
  } else {
    use_locale <- locale
  }

  pattern <- paste0("(?<=", use_locale, "=)[^|]+")
  match <- regmatches(name_string, regexpr(pattern, name_string, perl = TRUE))
  if (length(match) > 0) {
    match
  } else {
    NA_character_
  }
}

#' @export
get_red_list <- function(data = NULL, level = "Europe") {
  if (is.null(data) || length(data) == 0) {
    return(NA_character_)
  }

  for (n in seq_along(data)) {
    if (data[[n]]$name == level) {
      return(data[[n]]$pivot$category)
    }
  }

  return(NA_character_)
}

#' @export
get_bern_directive <- function(data = NULL) {
  if (is.null(data) || length(data) == 0) {
    return(NA_character_)
  }

  matched_annexes <- character(0)

  for (n in seq_along(data)) {
    if (data[[n]]$name == "Bern, Annex 1") {
      matched_annexes <- c(matched_annexes, "Annex 1")
    } else if (data[[n]]$name == "Bern, Annex 2") {
      matched_annexes <- c(matched_annexes, "Annex 2")
    } else if (data[[n]]$name == "Bern, Annex 3") {
      matched_annexes <- c(matched_annexes, "Annex 3")
    } else if (data[[n]]$name == "Bern Resolution 6") {
      matched_annexes <- c(matched_annexes, "Res. 6")
    }
  }

  if (length(matched_annexes) == 0) {
    return(NA_character_)
  } else {
    return(paste(matched_annexes, collapse = ", "))
  }
}

#' @export
get_habitats_directive <- function(data = NULL) {
  if (is.null(data) || length(data) == 0) {
    return(NA_character_)
  }

  matched_annexes <- character(0)

  for (n in seq_along(data)) {
    if (data[[n]]$name == "Habitat, Annex 2") {
      matched_annexes <- c(matched_annexes, "Annex 2")
    } else if (data[[n]]$name == "Habitat, Annex 4") {
      matched_annexes <- c(matched_annexes, "Annex 4")
    } else if (data[[n]]$name == "Habitat, Annex 5") {
      matched_annexes <- c(matched_annexes, "Annex 5")
    }
  }

  if (length(matched_annexes) == 0) {
    return(NA_character_)
  } else {
    return(paste(matched_annexes, collapse = ", "))
  }
}

#' @export
get_cites_convention <- function(data = NULL) {
  if (is.null(data) || length(data) == 0) {
    return(NA_character_)
  }

  matched_annexes <- character(0)

  for (n in seq_along(data)) {
    leg_name <- data[[n]]$name
    if (!is.null(leg_name) && grepl("^CITES", leg_name, ignore.case = TRUE)) {
      annex_part <- sub("^.*?,\\s*", "", leg_name)
      matched_annexes <- c(matched_annexes, annex_part)
    }
  }

  if (length(matched_annexes) == 0) {
    return(NA_character_)
  } else {
    return(paste(matched_annexes, collapse = ", "))
  }
}

#' @export
get_cms_convention <- function(data = NULL) {
  if (is.null(data) || length(data) == 0) {
    return(NA_character_)
  }

  matched_annexes <- character(0)

  for (n in seq_along(data)) {
    leg_name <- data[[n]]$name
    if (!is.null(leg_name) && grepl("^CMS", leg_name, ignore.case = TRUE)) {
      annex_part <- sub("^.*?,\\s*", "", leg_name)
      matched_annexes <- c(matched_annexes, annex_part)
    }
  }

  if (length(matched_annexes) == 0) {
    return(NA_character_)
  } else {
    return(paste(matched_annexes, collapse = ", "))
  }
}

#' @export
get_birds_directive <- function(data = NULL) {
  if (is.null(data) || length(data) == 0) {
    return(NA_character_)
  }

  matched_annexes <- character(0)

  for (n in seq_along(data)) {
    leg_name <- data[[n]]$name
    if (!is.null(leg_name) && grepl("^Birds Directive", leg_name, ignore.case = TRUE)) {
      # Izvlači sve posle zareza ili uzima ceo naziv ako nema podela
      annex_part <- sub("^.*?,\\s*", "", leg_name)
      matched_annexes <- c(matched_annexes, annex_part)
    }
  }

  if (length(matched_annexes) == 0) {
    return(NA_character_)
  } else {
    return(paste(matched_annexes, collapse = ", "))
  }
}

#' @export
get_protected_species <- function(data = NULL) {
  if (is.null(data) || length(data) == 0) {
    return(NA_character_)
  }

  matched_annexes <- character(0)

  for (n in seq_along(data)) {
    if (data[[n]]$name == "Serbia 1") {
      matched_annexes <- c(matched_annexes, "Strictly protected RS")
    } else if (data[[n]]$name == "Serbia 2") {
      matched_annexes <- c(matched_annexes, "Protected RS")
    }
  }

  if (length(matched_annexes) == 0) {
    return(NA_character_)
  } else {
    return(paste(matched_annexes, collapse = ", "))
  }
}

#' Extract Unique Literature References from Data Table
#'
#' @param dt A data.table containing 'publication.authors' and 'publication.year' columns.
#'
#' @return A character vector of unique formatted references (alphabetically sorted).
#' @export
#'
#' @importFrom data.table as.data.table
get_references <- function(dt) {
  refs_dt <- data.table::as.data.table(dt)[, .(publication.authors, publication.year)]

  refs_dt <- refs_dt[!is.na(publication.authors) & !is.na(publication.year)]
  refs_dt <- unique(refs_dt)
  if (nrow(refs_dt) == 0) {
    return(NA_character_)
  }

  #' @export
  # Format authors (et al. role)
  format_authors <- function(author_string) {
    authors_vector <- unlist(strsplit(author_string, split = ";|\\band\\b"))
    authors_vector <- trimws(authors_vector)
    authors_vector <- authors_vector[authors_vector != ""]

    num_authors <- length(authors_vector)

    if (num_authors == 0) {
      return(NA_character_)
    } else if (num_authors > 3) {
      first_author_surname <- unlist(strsplit(authors_vector[1], ","))[1]
      return(paste0(trimws(first_author_surname), " et al."))
    } else if (num_authors == 2) {
      auth1 <- trimws(unlist(strsplit(authors_vector[1], ","))[1])
      auth2 <- trimws(unlist(strsplit(authors_vector[2], ","))[1])
      return(paste(auth1, "&", auth2))
    } else {
      if (num_authors == 1) {
        return(trimws(unlist(strsplit(authors_vector[1], ","))[1]))
      }
      return(author_string)
    }
  }

  formatted_authors <- vapply(refs_dt$publication.authors, format_authors, character(1))
  refs_dt[, formatted_ref := paste(formatted_authors, publication.year)]

  return(paste(sort(unique(refs_dt$formatted_ref)), collapse = ", "))
}

#' @export
get_type_of_record <- function(citation_vector = NULL) {
  if (is.null(citation_vector) || length(citation_vector) == 0) {
    return(NA_character_)
  }

  has_literature <- sum(!is.na(citation_vector)) > 0
  has_field_data <- sum(is.na(citation_vector)) > 0

  if (has_literature && has_field_data) {
    return("field and literature")
  } else if (has_literature) {
    return("literature")
  } else if (has_field_data) {
    return("field")
  } else {
    return(NA_character_)
  }
}
