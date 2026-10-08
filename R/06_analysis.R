#' Summary of Data within a Polygon
#'
#' @param polygon An sf or terra spatial object defining the area of interest.
#' @param kingdom Return only the data from given kingdom.
#' @param phylum Return only the data from given phylum
#' @param order Return only the data from given order
#' @param area_buffer Numeric. Buffer distance in meters (default 50).
#' @param region_buffer Numeric. Buffer distance in meters (default 50,000).
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
  region_buffer = 50000,
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

  # Load study area and create buffers
  study_area <- terra::vect(polygon)
  study_area_laea <- terra::project(study_area, "EPSG:3035")
  area_buf_laea <- terra::buffer(study_area_laea, width = area_buffer)
  region_buf_laea <- terra::buffer(study_area_laea, width = region_buffer)
  study_area_buffer <- terra::aggregate(terra::project(area_buf_laea, "EPSG:4326"))
  study_region <- terra::aggregate(terra::project(region_buf_laea, "EPSG:4326"))
  rm(study_area_laea, area_buf_laea, region_buf_laea)

  # Get the GBIF dataset
  if (get_gbif) {
    if (verbose) message("Fetching GBIF occurrence data for the study region...")

    # 1. Convert study_region SpatVector to sf for WKT formatting
    region_sf <- sf::st_as_sf(study_region)
    region_sf_clean <- sf::st_make_valid(region_sf)
    region_sf_simp <- sf::st_simplify(region_sf_clean, dTolerance = 50)
    wkt_region <- sf::st_as_text(sf::st_geometry(region_sf_simp))

    # 2. Fetch data from GBIF using rgbif
    gbif_res <- NULL
    max_attempts <- 3
    for (attempt in 1:max_attempts) {
      gbif_res <- tryCatch(
        {
          rgbif::occ_search(
            geometry = wkt_region,
            hasCoordinate = TRUE,
            limit = 100000,
            geom_big = "bbox"
          )
        },
        error = function(e) {
          message(sprintf("⚠️ GBIF fetch failed on attempt %d (%s). Retrying...", attempt, e$message))
          NULL
        }
      )

      if (!is.null(gbif_res)) break
      Sys.sleep(5)
    }

    if (!is.null(gbif_res$data) && nrow(gbif_res$data) > 0) {
      gbif_raw <- gbif_res$data

      # Explicitly map GBIF columns to match Biologer structure
      gbif_df <- data.frame(
        occurrenceID = if ("occurrenceID" %in% names(gbif_raw)) as.character(gbif_raw$occurrenceID) else as.character(gbif_raw$key),
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
        scientificNameAuthorship = if ("scientificNameAuthorship" %in% names(gbif_raw)) as.character(gbif_raw$scientificNameAuthorship) else NA_character_,
        infraspecificEpithet = if ("infraspecificEpithet" %in% names(gbif_raw)) as.character(gbif_raw$infraspecificEpithet) else NA_character_,
        scientificName = if ("species" %in% names(gbif_raw)) gbif_raw$species else gbif_raw$scientificName,
        acceptedNameUsage = if ("acceptedScientificName" %in% names(gbif_raw)) gbif_raw$acceptedScientificName else NA_character_,
        previousIdentifications = NA_character_,
        taxonRank = if ("taxonRank" %in% names(gbif_raw)) gbif_raw$taxonRank else NA_character_,
        vernacularName = NA_character_,
        verbatimScientificName = NA_character_,
        taxonomicStatus = if ("taxonomicStatus" %in% names(gbif_raw)) gbif_raw$taxonomicStatus else NA_character_,
        identifiedBy = {
          id_val <- if ("identifiedBy" %in% names(gbif_raw)) as.character(gbif_raw$identifiedBy) else rep(NA_character_, nrow(gbif_raw))
          id_ids_val <- if ("identifiedByIDs" %in% names(gbif_raw)) as.character(gbif_raw$identifiedByIDs) else rep(NA_character_, nrow(gbif_raw))
          ifelse(!is.na(id_val) & id_val != "", id_val,
            ifelse(!is.na(id_ids_val) & id_ids_val != "", id_ids_val, NA_character_)
          )
        },
        dateIdentified = NA_character_,
        basisOfRecord = if ("basisOfRecord" %in% names(gbif_raw)) gbif_raw$basisOfRecord else NA_character_,
        `dcterms:type` = NA_character_,
        typeOfRecord = {
          bor <- if ("basisOfRecord" %in% names(gbif_raw)) as.character(gbif_raw$basisOfRecord) else rep(NA_character_, nrow(gbif_raw))
          ifelse(!is.na(bor) & bor != "", bor, "HumanObservation")
        },
        `dcterms:rightsHolder` = {
          pub <- if ("publishingOrgKey" %in% names(gbif_raw)) as.character(gbif_raw$publishingOrgKey) else rep(NA_character_, nrow(gbif_raw))
          pub_country <- if ("publishingCountry" %in% names(gbif_raw)) as.character(gbif_raw$publishingCountry) else rep(NA_character_, nrow(gbif_raw))
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
          rec_val <- if ("recordedBy" %in% names(gbif_raw)) as.character(gbif_raw$recordedBy) else rep(NA_character_, nrow(gbif_raw))
          rec_ids_val <- if ("recordedByIDs" %in% names(gbif_raw)) as.character(gbif_raw$recordedByIDs) else rep(NA_character_, nrow(gbif_raw))
          ifelse(!is.na(rec_val) & rec_val != "", rec_val,
            ifelse(!is.na(rec_ids_val) & rec_ids_val != "", rec_ids_val, NA_character_)
          )
        },
        decimalLatitude = gbif_raw$decimalLatitude,
        decimalLongitude = gbif_raw$decimalLongitude,
        coordinateUncertaintyInMeters = if ("coordinateUncertaintyInMeters" %in% names(gbif_raw)) gbif_raw$coordinateUncertaintyInMeters else NA_real_,
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
          ls_val <- if ("lifeStage" %in% names(gbif_raw)) as.character(gbif_raw$lifeStage) else rep(NA_character_, nrow(gbif_raw))
          ifelse(!is.na(ls_val) & ls_val != "", ls_val, NA_character_)
        },
        sex = {
          sex_val <- if ("sex" %in% names(gbif_raw)) as.character(gbif_raw$sex) else rep(NA_character_, nrow(gbif_raw))
          ifelse(!is.na(sex_val) & sex_val != "", sex_val, NA_character_)
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
        dataset = if ("datasetName" %in% names(gbif_raw)) gbif_raw$datasetName else "GBIF",
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

      # Drop rows without valid species or coordinates
      gbif_df <- gbif_df[!is.na(gbif_df$scientificName) &
        !is.na(gbif_df$decimalLatitude) &
        !is.na(gbif_df$decimalLongitude), ]

      # Ensure both are data.frames to avoid data.table scoping errors on rbind
      biologer_data <- as.data.frame(biologer_data)

      # Keep only columns present in biologer_data to ensure a clean bind
      common_cols <- intersect(names(biologer_data), names(gbif_df))

      biologer_data <- rbind(
        biologer_data[, common_cols, drop = FALSE],
        gbif_df[, common_cols, drop = FALSE]
      )
    }
  }

  if (!is.null(order)) {
    biologer_data <- biologer_data[!is.na(biologer_data$order) & biologer_data$order == order, ]
  } else if (!is.null(class)) {
    biologer_data <- biologer_data[!is.na(biologer_data$class) & biologer_data$phylum == class, ]
  } else if (!is.null(phylum)) {
    biologer_data <- biologer_data[!is.na(biologer_data$phylum) & biologer_data$phylum == phylum, ]
  } else if (!is.null(kingdom)) {
    biologer_data <- biologer_data[!is.na(biologer_data$kingdom) & biologer_data$kingdom == kingdom, ]
  }

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
  for (i in seq_along(area_species)) {
    species <- area_species[i]
    print(paste0(
      "Adding species ", species, "; ", i, " of ", length(area_species)
    ))

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

    if (!is.na(taxon$taxonID) && taxon$taxonID != "") {
      # 1. We have Biologer ID, query the server
      res_online <- get_taxon_by_id(taxon$taxonID, server = country)
      if (!is.null(res_online)) taxon_online <- res_online$data
    } else {
      # 2. GBIF data lacks a Biologer ID, look in the loaded Biologer dataset first.
      valid_match <- biologer_data[!is.na(biologer_data$taxonID) & biologer_data$scientificName == species, ]

      if (nrow(valid_match) > 0) {
        fallback_id <- valid_match$taxonID[1]
        res_online <- get_taxon_by_id(fallback_id, server = country)
        if (!is.null(res_online)) taxon_online <- res_online$data
      } else {
        # 3. Not found in dataset, fall back to local taxa CSV cache file.
        taxa_file <- file.path(get_storage_path(), paste0("biologer_taxa_", country, ".csv"))

        if (file.exists(taxa_file)) {
          taxa_df <- data.table::fread(taxa_file, stringsAsFactors = FALSE, fill = TRUE)
          taxon_match <- taxa_df[taxa_df$name == species, ]

          if (nrow(taxon_match) > 0) {
            fallback_id <- taxon_match$id[1] # Fixed from taxa_df$id[1]
            res_online <- get_taxon_by_id(fallback_id, server = country)
            if (!is.null(res_online)) taxon_online <- res_online$data
          } else {
            message(sprintf("⚠️ Species '%s' could not be matched to any Biologer ID.", species))
            taxon_online <- NULL
          }
        } else {
          message(sprintf("⚠️ Local taxa cache not found at: %s", taxa_file))
          taxon_online <- NULL
        }
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
    print(paste0(
      "Adding species ", species, "; ", i, " of ", length(region_species)
    ))

    taxon <- biologer_dt[biologer_dt$scientificName == species][1]
    taxon_online <- get_taxon_by_id(taxon$taxonID, server = country)$data
    data_region_dt <- as.data.table(data_region)

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
