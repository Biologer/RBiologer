#' @import jsonlite
#' @import data.table
#' @import curl
#' @import sf
#' @importFrom readr write_csv locale
#' @importFrom sf st_as_sf st_join
NULL

#' Internal function for updating .Renviron variables
#' @keywords internal
write_to_renviron <- function(key, value) {
  home_dir <- Sys.getenv("HOME")
  renviron_path <- file.path(home_dir, ".Renviron")

  if (!file.exists(renviron_path)) {
    file.create(renviron_path)
  }

  lines <- readLines(renviron_path, warn = FALSE)
  new_entry <- paste0(key, "='", value, "'")
  key_pattern <- paste0("^", key, "=.*")

  # Update the existing key or add new one
  if (any(grepl(key_pattern, lines))) {
    lines <- gsub(key_pattern, new_entry, lines)
  } else {
    lines <- c(lines, new_entry)
  }

  writeLines(lines, renviron_path)

  # Set for current session
  do.call(Sys.setenv, as.list(setNames(value, key)))
}

#' Internal function for removing variables from .Renviron file
#' @keywords internal
remove_from_renviron <- function(key) {
  home_dir <- Sys.getenv("HOME")
  renviron_path <- file.path(home_dir, ".Renviron")

  if (!file.exists(renviron_path)) {
    return(invisible(FALSE))
  }

  lines <- readLines(renviron_path, warn = FALSE)
  key_pattern <- paste0("^", key, "=.*")

  lines_to_keep <- lines[!grepl(key_pattern, lines)]

  if (length(lines_to_keep) < length(lines)) {
    writeLines(lines_to_keep, renviron_path)
    return(invisible(TRUE))
  }

  invisible(FALSE)
}

#' Get the internal storage directory for the package
#' @keywords internal
get_storage_path <- function(package = "RBiologer") {
  storage_path <- tools::R_user_dir(package, which = "data")

  if (!dir.exists(storage_path)) {
    dir.create(storage_path, recursive = TRUE)
  }

  return(storage_path)
}

#' Internal helper function to retrieve multiple environment variables based on a prefix
#' @keywords internal
get_multiple_env_vars <- function(prefix, server_keys) {
  # 1. Construct the list of environment variable names to check (e.g., "BIOLOGER_TOKEN_RS")
  env_var_names <- paste0(prefix, "_", server_keys)

  # 2. Retrieve all values from the system environment
  values <- Sys.getenv(env_var_names)

  # 3. Filter for values that are non-empty (i.e., credentials have been set)
  valid_indices <- which(nzchar(values))

  # 4. If nothing is set, return an empty named vector
  if (length(valid_indices) == 0) {
    return(setNames(character(0), character(0)))
  }

  # 5. Return a named vector: names are the lowercase server key (e.g., 'rs', 'hr'),
  # and values are the token/URL.
  valid_values <- values[valid_indices]
  names(valid_values) <- tolower(server_keys[valid_indices])

  valid_values
}

#' @keywords internal
get_biologer_base_url <- function() {
  servers <- toupper(names(BIOLOGER_URLS))
  urls <- get_multiple_env_vars("BIOLOGER_BASE_URL", servers)

  if (length(urls) == 0) {
    stop("No Biologer server URLs found. Please use the function
         'biologer_login(token, server)' to setup your base URL(s).", call. = FALSE)
  }

  urls
}

#' @keywords internal
get_biologer_token <- function() {
  servers <- toupper(names(BIOLOGER_URLS))
  tokens <- get_multiple_env_vars("BIOLOGER_TOKEN", servers)

  if (length(tokens) == 0) {
    stop("No Biologer access tokens found. Use helper function 
         'biologer_login(token, server)' to set a new token(s).", call. = FALSE)
  }

  tokens
}

#' Extracts a specific vernacular name from the translations string based on the locale code.
#'
#' @param translations_string A character string containing locale-value pairs (e.g., "en=name|hr=ime").
#' @param locale_code A character string specifying the locale key to extract (e.g., "en", "hr", "sr-Latn").
#' @return The extracted vernacular name (character string), or NA if the locale is not found.
#' @export
extract_translation <- function(translations_string, locale_code) {
  if (is.na(translations_string) || translations_string == "") {
    return(NA_character_)
  }

  pattern <- paste0("^.*(?:\\||^)", locale_code, "=(.*?)(?:\\||$).*$")

  result <- sub(pattern, "\\1", translations_string, perl = TRUE)

  if (result == translations_string) {
    NA_character_
  } else {
    result
  }
}

#' @title Calculate Coarsened Centroids for Observations
#'
#' @description
#' Calculates the centroid coordinates of a regular square grid cell (e.g., 10x10 km)
#' containing each observation's coordinates. This is primarily used for anonymizing
#' geographically restricted data. The function handles both data.table objects
#' (with standard latitude/longitude columns) and sf point objects (by extracting
#' coordinates from the geometry column).
#'
#' @param data A data.table or an sf object containing point geometry (sfc_POINT).
#'   If a data.table, it must contain 'decimalLatitude' and 'decimalLongitude' columns.
#' @param cell_size Integer. The size of the square grid cell in meters used for coarsening.
#'   Defaults to 10000 (10 km).
#' @param lat_col Character string. The name of the latitude column in the data.table.
#'   Defaults to "decimalLatitude". (Note: The function hardcodes "decimalLatitude"
#'   internally, but this argument is kept for standard signature compliance.)
#' @param lon_col Character string. The name of the longitude column in the data.table.
#'   Defaults to "decimalLongitude". (Note: The function hardcodes "decimalLongitude"
#'   internally, but this argument is kept for standard signature compliance.)
#'
#' @return The original data object (as a data.table) with two new columns:
#'   'mgrs10k_centroid_lon' and 'mgrs10k_centroid_lat', containing the WGS84 coordinates
#'   of the calculated centroid.
#'
#' @details
#' The function projects the coordinates to the appropriate UTM zone (Universal
#' Transverse Mercator), snaps them to the grid defined by 'cell_size', and then
#' transforms the cell center back to WGS84 (EPSG:4326) latitude and longitude.
#'
#' @importFrom data.table data.table copy :=
#' @importFrom sf st_as_sf st_transform st_coordinates st_geometry st_drop_geometry
#' @keywords spatial coordinates anonymization
add_utm_centroid <- function(data, cell_size = 10000) {

  if (inherits(data, "sf")) {
    # If spatial data from sf package
    if (!inherits(sf::st_geometry(data), "sfc_POINT")) {
      stop("Input 'data' is an sf object but not of type POINT (sfc_POINT).", call. = FALSE)
    }
    coords <- sf::st_coordinates(data)
    lat <- coords[, "Y"]
    lon <- coords[, "X"]
    out <- data.table::as.data.table(sf::st_drop_geometry(data))
    out[, `:=`(decimalLatitude = lat, decimalLongitude = lon)]
  } else {
    # If regular data.table
    stopifnot(inherits(data, "data.table"))
    out <- data.table::copy(data)
    if (!("decimalLatitude" %in% names(out) && "decimalLongitude" %in% names(out))) {
      stop(paste0("Input 'data' is a data.table but is missing required columns: ",
                  "decimalLatitude and/or decimalLongitude"), call. = FALSE)
    }
    lat <- out[["decimalLatitude"]]
    lon <- out[["decimalLongitude"]]
  }
  zone <- ifelse(
    is.na(lon),
    NA_integer_,
    floor((lon + 180) / 6) + 1L
  )
  is_north <- lat >= 0

  epsg <- ifelse(
    is_north,
    32600L + zone,   # northern hemisphere
    32700L + zone    # southern hemisphere
  )

  # Prepare result columns for the centroid coordinates only
  out[, `:=`(
    mgrs10k_centroid_lon = as.numeric(NA),
    mgrs10k_centroid_lat = as.numeric(NA)
  )]

  # Process each CRS (zone+hemisphere) in chunks
  for (crs in unique(epsg[!is.na(epsg)])) {
    idx <- which(epsg == crs)
    if (!length(idx)) next

    # a) points in WGS84 for this subset
    pts_sub <- sf::st_as_sf(
      out[idx],
      coords = c("decimalLongitude", "decimalLatitude"),
      crs = 4326,
      remove = FALSE
    )

    # b) transform to this UTM zone
    pts_utm <- sf::st_transform(pts_sub, crs)
    utm_coords <- sf::st_coordinates(pts_utm)
    e <- utm_coords[, 1]  # eastings (m)
    n <- utm_coords[, 2]  # northings (m)

    # c) snap to 10 km grid and compute centroids in UTM
    cell_x <- floor(e / cell_size)
    cell_y <- floor(n / cell_size)

    cent_e <- cell_x * cell_size + cell_size / 2
    cent_n <- cell_y * cell_size + cell_size / 2

    # d) transform centroids back to WGS84
    cent_sf_utm <- sf::st_as_sf(
      data.frame(cent_e = cent_e, cent_n = cent_n),
      coords = c("cent_e", "cent_n"),
      crs = crs
    )

    cent_sf_ll <- sf::st_transform(cent_sf_utm, 4326)
    cent_ll <- sf::st_coordinates(cent_sf_ll)

    # e) write back only the centroid coordinates
    out[idx, `:=`(
      mgrs10k_centroid_lon = cent_ll[, 1],
      mgrs10k_centroid_lat = cent_ll[, 2]
    )]
  }

  out
}

#' @title Convert sf Points to data.table with Coordinates
#' @description
#' Takes an sf object (POINT geometry), extracts the coordinates (Latitude/Longitude),
#' drops the geometry column, and returns a clean data.table.
#' It automatically transforms data to WGS84 (EPSG:4326) to ensure
#' 'decimalLatitude' and 'decimalLongitude' are in degrees.
#'
#' @param data An object of class \code{sf}. Must contain POINT geometries.
#' @param target_crs Integer. The EPSG code to transform coordinates into before extraction.
#' Defaults to 4326 (WGS 84) to match Darwin Core standards.
#'
#' @return A \code{data.table} with the original attributes plus 'decimalLongitude'
#' and 'decimalLatitude' columns. The geometry column is removed.
#'
#' @importFrom sf st_transform st_coordinates st_drop_geometry st_crs
#' @importFrom data.table as.data.table :=
#' @export
sf_points_to_dt <- function(data, target_crs = 4326) {

  # 1. Validation
  if (!inherits(data, "sf")) {
    stop("Input 'data' must be an sf object.", call. = FALSE)
  }

  # 2. Transform CRS if necessary (Ensure we get Degrees, not Meters)
  # We check if a CRS exists and if it differs from the target
  input_crs <- sf::st_crs(data)
  if (!is.na(input_crs) && input_crs$epsg != target_crs) {
    data <- sf::st_transform(data, target_crs)
  }

  # 3. Extract Coordinates
  # st_coordinates returns a matrix with X and Y columns
  coords <- sf::st_coordinates(data)

  # 4. Convert to data.table (dropping geometry)
  dt <- data.table::as.data.table(sf::st_drop_geometry(data))

  # 5. Add Coordinate Columns
  # Check if X/Y exist in coords matrix (handles 2D and 3D points safely)
  dt[, `:=`(
    decimalLongitude = coords[, "X"],
    decimalLatitude  = coords[, "Y"]
  )]

  # Set column order
  data.table::setcolorder(dt, DWC_COLUMN_ORDER)

  return(dt)
}