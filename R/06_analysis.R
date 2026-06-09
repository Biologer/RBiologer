#' Summary of Data within a Polygon
#'
#' @param polygon An sf or terra spatial object defining the area of interest.
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
species_within_polygon <- function(
  polygon = NULL,
  country = "all",
  area_buffer = 0,
  region_buffer = 50000,
  auto.download = TRUE,
  verbose = TRUE
) {
  if (is.null(polygon)) {
    stop("You must provide study area polygon (i.e. path to KML or SHP polygon file).", call. = FALSE)
  }

  # Get the Biologer data
  biologer_data <- open_data(auto.download, verbose)
  biologer_data <- biologer_data[is.na(biologer_data$status) | biologer_data$status == "approved", ]
  biologer_data <- biologer_data[!is.na(biologer_data$taxonID), ]
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

  # Load study area and create buffers
  study_area <- terra::vect(polygon)
  study_area_laea <- terra::project(study_area, "EPSG:3035")
  area_buf_laea <- terra::buffer(study_area_laea, width = area_buffer)
  region_buf_laea <- terra::buffer(study_area_laea, width = region_buffer)
  study_area_buffer <- terra::aggregate(terra::project(area_buf_laea, "EPSG:4326"))
  study_region <- terra::aggregate(terra::project(region_buf_laea, "EPSG:4326"))
  rm(study_area_laea, area_buf_laea, region_buf_laea)

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
    taxon_online <- get_taxon_by_id(taxon$taxonID, server = country)$data

    output.list[[i]] <- data.frame(
      Class = taxon$class,
      Order = taxon$order,
      Family = taxon$family,
      Taxon = taxon$scientificName,
      EnglishName = get_local_name(taxon$translations, "en"),
      LocalName = get_local_name(taxon$translations, country),
      BernConvention = get_bern_directive(taxon_online$conservation_legislations),
      HabitatDirective = get_habitats_directive(taxon_online$conservation_legislations),
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

  list(
    summary = output_table,
    data_area = as.data.frame(data_area),
    data_region = as.data.frame(data_region)
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
