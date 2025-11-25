#' @import httr
#' @import jsonlite
#' @import dplyr
#' @importFrom readr write_csv locale
NULL

#' Set or Update the 'per_page' Query Parameter in a URL
#'
#' Ensures that a given URL for the Biologer API includes the 'per_page'
#' query parameter, setting it to a specified value (defaulting to 1000).
#'
#' This function safely sets the pagination size for API requests by:
#' 1. Appending '?per_page=X' if no query parameters exist.
#' 2. Appending '&per_page=X' if other query parameters exist.
#' 3. Overwriting an existing 'per_page' parameter if found.
#'
#' @param url A character string representing the URL to be modified.
#'   Can be the initial base API path or a 'next' link from a previous API response.
#' @param per.page An integer specifying the desired number of records per page.
#'   Defaults to 1000.
#' @return A character string representing the modified URL with the 'per_page' parameter set.
#'   Returns \code{NULL} if the input \code{url} is \code{NULL}.
#' @keywords internal
set_per_page <- function(url, per.page = 1000) {
  if (is.null(url)) {
    return(NULL)
  }

  per_page_str <- as.character(per.page)
  pattern <- "per_page=[0-9]+"

  if (grepl(pattern, url)) {
    # per_page already exists in the URL. Replace its value.
    new_url <- gsub(pattern, paste0("per_page=", per_page_str), url)
  } else {
    # per_page does not exist. Append it to the URL.
    if (grepl("\\?", url)) {
      new_url <- paste0(url, "&per_page=", per_page_str)
    } else {
      new_url <- paste0(url, "?per_page=", per_page_str)
    }
  }

  URLencode(new_url)
}

#' @keywords internal
get_fetch_start_url <- function(base_api_url, last_timestamp) {
  url <- set_per_page(base_api_url)

  if (last_timestamp != "0") {
    url <- paste0(url, "&updated_after=", last_timestamp)
  }

  URLencode(url)
}

#' Collapse nested 'types' data frame into a semicolon-separated string of names.
#' @param type_df A data frame containing 'name' column, or an empty list/data frame.
#' @keywords internal
collapse_types <- function(type_df) {
  if (is.data.frame(type_df) && nrow(type_df) > 0 && "name" %in% names(type_df)) {
    paste(type_df$name, collapse = "; ")
  } else {
    ""
  }
}

#' Collapse nested 'photos' data frame into a semicolon-separated string of URLs.
#' @param photos_df A data frame containing 'url' column, or an empty list/data frame.
#' @keywords internal
collapse_photo_urls <- function(photos_df) {
  if (is.data.frame(photos_df) && nrow(photos_df) > 0 && "url" %in% names(photos_df)) {
    paste(photos_df$url, collapse = "; ")
  } else {
    ""
  }
}

#' Collapse nested 'translations' data frame into a semicolon-separated string of locale=name pairs.
#' @param translations_df A data frame containing 'locale' and 'native_name' columns, or an empty list/data frame.
#' @keywords internal
collapse_translations <- function(translations_df) {
  if (is.null(translations_df) || !is.data.frame(translations_df) || nrow(translations_df) == 0) {
    return(NA_character_)
  }

  valid_translations <- translations_df[
    !is.na(translations_df$native_name) & translations_df$native_name != "",
    c("locale", "native_name")
  ]

  if (nrow(valid_translations) == 0) {
    return(NA_character_)
  }

  translated_pairs <- paste0(
    valid_translations$locale,
    "=",
    valid_translations$native_name
  )

  paste(translated_pairs, collapse = ";")
}

#' Collapse nested 'stages' data frame into a semicolon-separated string of stage names.
#' @param stages_df A data frame containing 'name' column, or an empty list/data frame.
#' @keywords internal
collapse_stages <- function(stages_df) {
  if (is.data.frame(stages_df) && nrow(stages_df) > 0 && "name" %in% names(stages_df)) {
    paste(stages_df$name, collapse = "; ")
  } else {
    ""
  }
}

#' Collapse nested 'synonyms' data frame into a semicolon-separated string of names.
#' @param synonyms_df A data frame containing synonym names, or an empty list/data frame.
#' @return A character string of collapsed synonym names, or an empty string if missing.
#' @keywords internal
collapse_synonyms <- function(synonyms_df) {
  if (is.data.frame(synonyms_df) && nrow(synonyms_df) > 0 && "name" %in% names(synonyms_df)) {
    paste(synonyms_df$name, collapse = "; ")
  } else {
    ""
  }
}

#' Formats a list of publication authors into a standard citation string.
#'
#' @param authors_list A list column element containing a data frame of authors
#'   with 'last_name' and 'first_name' columns.
#' @keywords internal
format_authors <- function(authors_list) {
  # 1. Handle empty cases
  if (!is.data.frame(authors_list) || nrow(authors_list) == 0) {
    return("")
  }

  # 2. Prepare full citation string (Last Name, Initial)
  formatted_authors <- paste0(
    authors_list$last_name,
    ", ",
    substr(authors_list$first_name, 1, 1),
    "."
  )

  n_authors <- length(formatted_authors)

  # 3. Apply the specific citation rules
  if (n_authors == 1) {
    # Rule 1: One author
    formatted_authors[1]
  } else if (n_authors == 2) {
    # Rule 2: Two authors
    paste(formatted_authors, collapse = " and ")
  } else if (n_authors >= 3) {
    # Rule 3: Three or more authors
    paste0(formatted_authors[1], " et al.")
  } else {
    "" # Should not happen :)
  }
}

#' Display dynamic, single-line progress update in the console.
#'
#' @param records_fetched The total number of records fetched so far.
#' @param total_records The total number of records expected (or NULL if unknown).
#' @param current_count The number of records fetched in the current page (optional, for debugging).
#' @keywords internal
display_progress <- function(records_fetched, total_records) {
  # Only run dynamic update in an interactive session
  if (interactive()) {
    output_string <- "\r" # Carriage return to overwrite the current line
    if (!is.null(total_records) && total_records > 0) {
      progress_percentage <- round(records_fetched / total_records * 100, 1)
      output_string <- paste0(
        output_string,
        "Fetched ",
        format(records_fetched, big.mark = ","), "/",
        format(total_records, big.mark = ","),
        " records (",
        round(progress_percentage), "%)  "
      )
    } else {
      # Total_records is unknown or zero
      output_string <- paste0(
        output_string,
        "Fetched ",
        format(records_fetched, big.mark = ","),
        " records so far."
      )
    }

    cat(output_string) # Print the string without a newline
  } else {
    # Fallback for non-interactive sessions (e.g., Rscript)
    # We use message() here to ensure output is captured in logs/standard out.
    if (!is.null(total_records)) {
      progress_percentage <- round(records_fetched / total_records * 100, 1)
      message(
        "Progress: ",
        format(records_fetched, big.mark = ","), "/",
        format(total_records, big.mark = ","),
        " (", progress_percentage, "%)"
      )
    } else {
      message("Progress: Fetched ", format(records_fetched, big.mark = ","), " records so far.")
    }
  }
}

#' Fetch Paginated Observations and Save/Resume using Timestamp
#'
#' Resumes the download from the last successfully fetched record's timestamp.
#'
#' @param filename Character string for the path to save the final data
#' (e.g., "observations.csv").
#' @param api_url URL of the Biologer API used to access the data.
#' @param token JWT authorization token.
#' @param columns_to_keep Select columns to keep in the final dataset. Server
#' will return many columns, which are not required for R Biologer package to work.
#' @keywords internal
#' @export
get_data_from_api <- function(api_url, token, filename = NULL, columns_to_keep) {
  if (is.null(filename)) {
    message("Due to its size, data should be saved into a file. Please provide CSV file for saving data.")
    break
  }

  checkpoint_path <- paste0(filename, ".checkpoint.rds")
  last_success_timestamp <- "0"
  records_fetched <- 0
  total_records <- NULL

  if (file.exists(checkpoint_path)) {
    checkpoint <- readRDS(checkpoint_path)
    if (!is.null(checkpoint$next_url)) {
      # Check if the last run was interrupted
      url <- set_per_page(url = checkpoint$next_url)
      records_fetched <- checkpoint$records_fetched
      total_records <- checkpoint$total_records
      message(paste0("Resuming interrupted paginated fetch from: ", url))
    } else {
      # Starting an Incremental Fetch (previous run completed successfully)
      last_success_timestamp <- checkpoint$last_success_timestamp
      url <- get_fetch_start_url(api_url, last_success_timestamp)
      records_fetched <- 0
      total_records <- NULL
      message(paste0("Starting incremental fetch after timestamp: ", last_success_timestamp))
    }
  } else {
    # Initial Full Download
    url <- get_fetch_start_url(api_url, last_success_timestamp)
    message("Starting initial full fetch from URL: ", url)
  }

  repeat {
    if (is.null(url)) break

    h <- curl::new_handle()
    curl::handle_setheaders(h, Authorization = paste("Bearer", token), Accept = "application/json")
    result <- curl::curl_fetch_memory(url, handle = h)

    # Handle server rate/overload conditions
    if (result$status_code %in% c(429, 503)) {
      # Exponential backoff for 503
      wait <- ifelse(result$status_code == 429, 15, 5 + runif(1, 0, 5))
      message(
        "Server returned ", result$status_code, ". Waiting ",
        round(wait, 1), " seconds before retry…"
      )
      Sys.sleep(wait)
      next
    }

    # If the server return error code
    if (result$status_code != 200) {
      warning(paste("Failed to download page, error code:", result$status_code))
      break
    }

    data <- jsonlite::fromJSON(rawToChar(result$content), flatten = TRUE)

    # Extract total record count on first page
    if (is.null(total_records)) {
      total_records <- tryCatch(as.integer(data$meta$total), error = function(e) NULL)
      if (!is.null(total_records)) {
        if (total_records == 0) {
          message("The data is already downloaded. No need to update…")
          break
        } else {
          message("Total records to download: ", format(total_records, big.mark = ","))
        }
      }
    }

    # Add this page
    current_count <- nrow(data$data)
    records_fetched <- records_fetched + current_count

    # Display progress
    display_progress(records_fetched = records_fetched, total_records = total_records)

    # 1. Save the data
    current_page_data <- data$data # downloaded batch of data

    # Define columns to keep in the final dataset
    if (!is.null(columns_to_keep) && length(columns_to_keep) > 0) {
      columns_to_select <- intersect(columns_to_keep, names(current_page_data))
      current_page_data <- current_page_data[, columns_to_select]
    }

    # Some data is saved in a complex list. We need to process those before saving in
    # simple CSV file.
    # Process the 'types' column
    if ("types" %in% names(current_page_data) && is.list(current_page_data$types)) {
      current_page_data$types <- unlist(lapply(current_page_data$types, collapse_types))
    }
    # Process the 'photos' column
    if ("photos" %in% names(current_page_data) && is.list(current_page_data$photos)) {
      current_page_data$photos <- unlist(lapply(current_page_data$photos, collapse_photo_urls))
    }
    # Process the 'publication.authors' column
    if ("publication.authors" %in% names(current_page_data) && is.list(current_page_data$publication.authors)) {
      current_page_data$publication.authors <- unlist(lapply(current_page_data$publication.authors, format_authors))
    }
    # Process the 'translations' column
    if ("translations" %in% names(current_page_data) && is.list(current_page_data$translations)) {
      current_page_data$translations <- unlist(lapply(current_page_data$translations, collapse_translations))
    }
    # Process the 'stages' column
    if ("stages" %in% names(current_page_data) && is.list(current_page_data$stages)) {
      current_page_data$stages <- unlist(lapply(current_page_data$stages, collapse_stages))
    }
    # Process the 'synonyms' column
    if ("synonyms" %in% names(current_page_data) && is.list(current_page_data$synonyms)) {
      current_page_data$synonyms <- unlist(lapply(current_page_data$synonyms, collapse_synonyms))
    }

    data.table::fwrite(current_page_data, file = filename, append = file.exists(filename))

    # 2. Save the progress
    url <- data$links$`next`
    checkpoint <- list(
      records_fetched = records_fetched,
      total_records = total_records
    )
    if (!is.null(url)) {
      checkpoint$next_url <- url
    }
    saveRDS(checkpoint, checkpoint_path)

    # 3. Stop and save the progress after complete download
    if (is.null(url)) {
      message("Download complete. Finalizing…")
      final_checkpoint <- list(
        last_success_timestamp = as.character(as.integer(Sys.time())),
        records_fetched = records_fetched,
        total_records = total_records,
        next_url = NULL
      )
      saveRDS(final_checkpoint, checkpoint_path)
      message("Checkpoint saved for next incremental run.")
      break
    }

    url <- set_per_page(url = url)
    #Sys.sleep(0.5)
  }

  # Remove duplicated entries (only keep the new ones)
  if (file.exists(filename)) {
    full_data <- data.table::fread(filename, fill = TRUE)
    is_duplicate <- duplicated(full_data$id, fromLast = TRUE)
    final_data <- full_data[!is_duplicate, ]
    if (nrow(full_data) > nrow(final_data)) {
      message("Removing duplicated data to keep the new online changes. ",
              "Total rows before: ", nrow(full_data), "; after: ",
              nrow(final_data))
      data.table::fwrite(final_data, file = filename)
    }
  }

  message("The data is saved locally in ", filename)
}

#' Main function used to get data from all logged-in Biologer servers
#'
#' @description This function iterates through all servers for which credentials 
#' are stored and downloads field observations, literature observations, and 
#' taxonomic data for each active server.
#'
#' @export
get_biologer_data <- function() {
  tokens <- get_biologer_token()
  urls <- get_biologer_base_url()

  active_servers <- intersect(names(tokens), names(urls))
  if (length(active_servers) == 0) {
    stop("No active server credentials found. Please use 'biologer_login(server, token)' ",
         "for at least one server.", call. = FALSE)
  }

  storage_path <- get_storage_path()

  for (server_key in active_servers) {

    server_url <- urls[server_key]
    server_token <- tokens[server_key]
    server_display <- toupper(server_key)

    message("\n========================================================")
    message(paste0("  STARTING DATA DOWNLOAD FOR SERVER: ", server_display))
    message("========================================================\n")

    # File suffix is now server-specific (e.g., _rs.csv)
    file_suffix <- paste0("_", server_key, ".csv")

    # --- Step 1/3: Downloading Field Observation data ---
    message(paste0("* Step 1/3 [", server_display, "]: Downloading Field Observation data."))
    get_data_from_api(
      api_url = paste0(server_url, "/public-field-observations"),
      token = server_token,
      # Server-specific filename
      filename = file.path(storage_path, paste0("biologer_field_observations", file_suffix)),
      columns_to_keep = FIELD_OBS_COLUMNS
    )

    # --- Step 2/3: Downloading Literature Observation data ---
    message(paste0("* Step 2/3 [", server_display, "]: Downloading Literature Observation data."))
    get_data_from_api(
      api_url = paste0(server_url, "/literature-observations"),
      token = server_token,
      # Server-specific filename
      filename = file.path(storage_path, paste0("biologer_literature_observations", file_suffix)),
      columns_to_keep = LITERATURE_OBS_COLUMNS
    )

    # --- Step 3/3: Downloading Taxonomic data ---
    message(paste0("* Step 3/3 [", server_display, "]: Downloading Taxonomic data."))
    get_data_from_api(
      api_url = paste0(server_url, "/taxa"),
      token = server_token,
      # Server-specific filename
      filename = file.path(storage_path, paste0("biologer_taxa", file_suffix)),
      columns_to_keep = TAXA_COLUMNS
    )
  }

  message("\n--------------------------------------------------------")
  message("All data downloads and consolidations complete.")
  message("--------------------------------------------------------")

  invisible(TRUE)
}

#' Open Biologer Data
#'
#' Opens the Biologer data CSV file as a dataframe.
#'
#' @param path A character string specifying the path to the CSV file.
#'             If NULL, defaults to the path used in `get.biologer.data()`.
#' @param verbose Logical, whether to display messages about data loading. Defaults to TRUE.
#' @return A dataframe containing the Biologer data.
#' @export
open.data <- function(path = "biologer_data.csv", verbose = TRUE) {
  # Check if the file exists
  if (!file.exists(path)) {
    stop("The file does not exist at the specified path: ", path)
  }

  # Read the CSV into a dataframe
  df <- tryCatch(
    {
      readr::read_csv(
        path,
        locale = readr::locale(encoding = "UTF-8"),
        show_col_types = FALSE # Suppress column spec messages
      )
    },
    error = function(e) {
      stop("Failed to read the file: ", e$message)
    }
  )

  # Log success message if verbose is TRUE
  if (verbose) {
    message("Data successfully loaded from: ", path)
    message("Rows: ", nrow(df), ", Columns: ", ncol(df))
  }

  return(df)
}


#' Subset Biologer Data
#'
#' Subset a Biologer dataframe based on specified column-value conditions.
#'
#' @param df A dataframe to subset. Defaults to the result of `open.data()` if NULL.
#' @param ... Named arguments specifying column names and the values to subset by.
#'            For example: `sex = "value"`, `taxon.name = "value"`, or
#'            `taxon.id = c("value1", "value2")`.
#' @return A subsetted dataframe.
#' @export
subset.biologer <- function(df = NULL, ...) {
  # Load default dataset if df is not supplied
  if (is.null(df)) {
    df <- open.data(verbose = FALSE) # Suppress messages when auto-loading
  }

  # Convert variable conditions into a list
  conditions <- list(...)

  # Ensure all provided column names exist in the dataframe
  invalid_cols <- setdiff(names(conditions), names(df))
  if (length(invalid_cols) > 0) {
    stop("Invalid column names: ", paste(invalid_cols, collapse = ", "))
  }

  # Dynamically build filtering logic
  subset_df <- df
  for (col in names(conditions)) {
    values <- conditions[[col]]
    if (length(values) > 1) {
      # Use `%in%` for multiple values
      subset_df <- subset_df[subset_df[[col]] %in% values, ]
    } else {
      # Use `==` for single value
      subset_df <- subset_df[subset_df[[col]] == values, ]
    }
  }

  # Return the subsetted dataframe
  return(subset_df)
}


#############################################################

#' Plot RH_2018 Shapefile
#'
#' Loads and plots the RH_2018 shapefile included in the package.
#'
#' @return A ggplot object showing the RH_2018 map.
#' @export
plot.hr <- function() {
  library(sf)
  library(ggplot2)

  # Locate and load the shapefile
  shapefile_path <- system.file("extdata", "RH_2018.shp", package = "biologerR")
  if (shapefile_path == "") stop("Shapefile not found in the package.")

  rh_data <- sf::st_read(shapefile_path, quiet = TRUE)

  # Create the plot
  ggplot(data = rh_data) +
    geom_sf(fill = "lightblue", color = "black") +
    theme_minimal()
}


#' Plot Zupanije JSON
#'
#' Loads and plots the Zupanije.json file included in the package.
#'
#' @return A ggplot object showing the Zupanije map.
#' @export
plot.zupanija <- function() {
  library(sf)
  library(ggplot2)

  # Locate the GeoJSON file
  json_path <- system.file("extdata", "zupanije.json", package = "biologerR")
  if (json_path == "") stop("GeoJSON file not found in the package.")

  # Read the GeoJSON
  zupanije_sf <- sf::st_read(json_path, quiet = TRUE)

  # Add region centroids for labeling
  zupanije_sf$centroid <- sf::st_centroid(zupanije_sf$geometry)
  centroids <- sf::st_coordinates(zupanije_sf$centroid)
  zupanije_sf$lon <- centroids[, 1]
  zupanije_sf$lat <- centroids[, 2]

  # Plot with labels
  ggplot(data = zupanije_sf) +
    geom_sf(fill = "lightgreen", color = "darkgreen") +
    geom_text(aes(x = lon, y = lat, label = name), size = 2, color = "blue") +
    theme_minimal()
}


#' Plot Biologer Observations on a Map
#'
#' Plots latitude and longitude of observations from the Biologer dataframe (`df`) on
#' either the RH_2018 or Zupanije map.
#'
#' @param fill Column name to use for coloring points. Default is "taxon.name".
#' @param shape Column name to use for point shapes. Default is NULL (no shapes).
#' @param layer Map layer to use: "rh" for RH_2018.shp or "zupanije" for Zupanije.json.
#' @param df A dataframe containing Biologer data with latitude and longitude columns.
#' @param title Plot title. Default is NULL (no title).
#' @param x_axis_label X-axis label. Default is NULL (no label).
#' @param y_axis_label Y-axis label. Default is NULL (no label).
#' @param legend_fill_title Title for the fill legend. Default is NULL (no title).
#' @param legend_shape_title Title for the shape legend. Default is NULL (no title).
#' @param dot_size Size of the points. Default is 2.
#' @param layer_fill_color Fill color for the map layer. Default is "lightblue".
#' @param layer_border_color Border color for the map layer. Default is "black".
#' @param layer_border_width Border width for the map layer. Default is 0.5.
#' @param palette Color palette for points. Can be a predefined name or a custom named vector.
#'
#' @return A ggplot object showing observations on the selected map layer.
#' @export
plot.biologer <- function(fill = "taxon.name", shape = NULL, layer = "rh", df,
                          title = NULL, x_axis_label = NULL, y_axis_label = NULL,
                          legend_fill_title = NULL, legend_shape_title = NULL,
                          dot_size = 2,
                          layer_fill_color = "lightblue",
                          layer_border_color = "black",
                          layer_border_width = 0.5,
                          palette = "viridis") {
  library(sf)
  library(ggplot2)
  library(scales)

  # Remove rows with missing coordinates
  df <- df[!is.na(df$latitude) & !is.na(df$longitude), ]
  message(nrow(df), " rows remain after removing rows with missing latitude or longitude.")

  # Load the selected map layer
  map_layer <- NULL
  if (layer == "rh") {
    shapefile_path <- system.file("extdata", "RH_2018.shp", package = "biologerR")
    if (shapefile_path == "") stop("RH_2018 shapefile not found in the package.")
    map_layer <- sf::st_read(shapefile_path, quiet = TRUE)
  } else if (layer == "zupanije") {
    json_path <- system.file("extdata", "zupanije.json", package = "biologerR")
    if (json_path == "") stop("Zupanije GeoJSON file not found in the package.")
    map_layer <- sf::st_read(json_path, quiet = TRUE)
  } else {
    stop("Invalid layer specified. Use 'rh' for RH_2018 or 'zupanije'.")
  }

  # Convert the Biologer data to an sf object
  df_sf <- sf::st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

  # Transform Biologer data to match the CRS of the map layer
  df_sf <- sf::st_transform(df_sf, crs = sf::st_crs(map_layer))

  # Generate a dynamic palette if needed
  unique_fill_values <- unique(df_sf[[fill]])
  if (is.character(palette) && !is.null(names(palette))) {
    # Custom palette provided
    if (!all(unique_fill_values %in% names(palette))) {
      missing_values <- setdiff(unique_fill_values, names(palette))
      stop("The custom palette is missing colors for: ", paste(missing_values, collapse = ", "))
    }
    colors <- palette
  } else if (is.character(palette) && length(palette) == 1) {
    # Predefined palette
    color_count <- length(unique_fill_values)
    if (palette == "viridis") {
      colors <- viridis::viridis(color_count)
    } else if (palette == "plasma") {
      colors <- viridis::plasma(color_count)
    } else if (palette == "rainbow") {
      colors <- rainbow(color_count)
    } else {
      colors <- scales::hue_pal()(color_count)
    }
    names(colors) <- unique_fill_values
  } else {
    stop("Invalid palette. Use a predefined palette name or a named list of colors.")
  }

  # Build the ggplot
  p <- ggplot() +
    geom_sf(data = map_layer, fill = layer_fill_color, color = layer_border_color, size = layer_border_width) +
    geom_sf(
      data = df_sf,
      aes_string(color = fill, shape = shape),
      size = dot_size
    ) +
    scale_color_manual(values = colors) +
    theme_minimal()

  # Add custom title and axis labels
  if (!is.null(title)) {
    p <- p + ggtitle(title)
  }
  if (!is.null(x_axis_label)) {
    p <- p + xlab(x_axis_label)
  }
  if (!is.null(y_axis_label)) {
    p <- p + ylab(y_axis_label)
  }

  # Customize the legends if titles are provided
  if (!is.null(legend_fill_title)) {
    p <- p + scale_color_manual(name = legend_fill_title, values = colors)
  }
  if (!is.null(legend_shape_title)) {
    p <- p + scale_shape_discrete(name = legend_shape_title)
  }

  return(p)
}


#' Count Observations Per Zupanija
#'
#' Counts the number of observations per Zupanija based on the coordinates of rows in the dataframe.
#'
#' @param fill Column name to group and count rows by. Default is "taxon.name".
#' @param df A dataframe containing latitude and longitude columns.
#' @param selected_zupanije Optional vector of županija names to filter by. Default is NULL (all županije).
#'
#' @return A dataframe with counts for each Zupanija and a TOTAL row.
#' @export
zupanije.biologer <- function(fill = "taxon.name", df, selected_zupanije = NULL) {
  library(sf)
  library(dplyr)

  # Remove rows with missing coordinates
  df <- df[!is.na(df$latitude) & !is.na(df$longitude), ]
  message(nrow(df), " rows remain after removing rows with missing latitude or longitude.")

  # Load Zupanije map layer
  json_path <- system.file("extdata", "zupanije.json", package = "biologerR")
  if (json_path == "") stop("Zupanije GeoJSON file not found in the package.")
  zupanije_sf <- sf::st_read(json_path, quiet = TRUE)

  # Filter županije if specified
  if (!is.null(selected_zupanije)) {
    if (!all(selected_zupanije %in% zupanije_sf$name)) {
      invalid_zupanije <- selected_zupanije[!selected_zupanije %in% zupanije_sf$name]
      stop("Invalid županija names: ", paste(invalid_zupanije, collapse = ", "))
    }
    zupanije_sf <- zupanije_sf[zupanije_sf$name %in% selected_zupanije, ]
  }

  # Convert the Biologer data to an sf object
  df_sf <- sf::st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

  # Spatial join to find which Zupanija each point falls into
  joined_data <- sf::st_join(df_sf, zupanije_sf, join = sf::st_within)

  # Count rows per Zupanija and grouping variable
  if (fill == "none") {
    counts <- joined_data %>%
      group_by(name) %>%
      summarise(count = n(), .groups = "drop") %>%
      mutate(!!fill := "ALL") # Add a default "ALL" column when no grouping
  } else {
    counts <- joined_data %>%
      group_by(!!sym(fill), name) %>%
      summarise(count = n(), .groups = "drop")
  }

  # Add TOTAL row for each group and for the entire dataset
  total_per_group <- counts %>%
    group_by(!!sym(fill)) %>%
    summarise(name = "TOTAL", count = sum(count), .groups = "drop")

  total_overall <- counts %>%
    summarise(!!sym(fill) := "TOTAL", name = "TOTAL", count = sum(count), .groups = "drop")

  # Combine everything into a single dataframe
  final_counts <- bind_rows(counts, total_per_group, total_overall)

  # Arrange for better readability
  final_counts <- final_counts %>%
    arrange(!!sym(fill), name)

  # Print the result
  print(final_counts)
  return(final_counts)
}


#' Plot Biologer Observations for Specific Zupanije
#'
#' Plots observations within one or more Zupanije based on coordinates and user specifications.
#'
#' @param fill Column name to use for coloring points. Default is "taxon.name".
#' @param shape Column name to use for point shapes. Default is NULL (no shapes).
#' @param zupanija Vector of Zupanija names to filter and plot.
#' @param df A dataframe containing latitude and longitude columns.
#' @param title Plot title. Default is NULL (no title).
#' @param x_axis_label X-axis label. Default is NULL (no label).
#' @param y_axis_label Y-axis label. Default is NULL (no label).
#' @param legend_fill_title Title for the fill legend. Default is NULL (no title).
#' @param legend_shape_title Title for the shape legend. Default is NULL (no title).
#' @param dot_size Size of the points. Default is 2.
#' @param layer_fill_color Fill color for the map layer. Default is "lightblue".
#' @param layer_border_color Border color for the map layer. Default is "black".
#' @param layer_border_width Border width for the map layer. Default is 0.5.
#' @param palette Color palette for points. Can be a predefined name or a custom named vector.
#'
#' @return A ggplot object showing observations for the specified Zupanije.
#' @export
plot.zupanije <- function(fill = "taxon.name", shape = NULL, zupanija, df,
                          title = NULL, x_axis_label = NULL, y_axis_label = NULL,
                          legend_fill_title = NULL, legend_shape_title = NULL,
                          dot_size = 2,
                          layer_fill_color = "lightblue",
                          layer_border_color = "black",
                          layer_border_width = 0.5,
                          palette = "viridis") {
  library(sf)
  library(ggplot2)
  library(scales)

  # Remove rows with missing coordinates
  df <- df[!is.na(df$latitude) & !is.na(df$longitude), ]
  message(nrow(df), " rows remain after removing rows with missing latitude or longitude.")

  # Load Zupanije map layer
  json_path <- system.file("extdata", "zupanije.json", package = "biologerR")
  if (json_path == "") stop("Zupanije GeoJSON file not found in the package.")
  zupanije_sf <- sf::st_read(json_path, quiet = TRUE)

  # Filter for the selected Zupanije
  zupanije_filtered <- zupanije_sf[zupanije_sf$name %in% zupanija, ]
  if (nrow(zupanije_filtered) == 0) stop("None of the specified Zupanije were found.")

  # Convert the Biologer data to an sf object
  df_sf <- sf::st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

  # Transform Biologer data to match the CRS of the Zupanija map layer
  df_sf <- sf::st_transform(df_sf, crs = sf::st_crs(zupanije_sf))

  # Spatial join to filter points within the selected Zupanije
  points_in_zupanije <- sf::st_join(df_sf, zupanije_filtered, join = sf::st_within, left = FALSE)

  # Generate a dynamic palette if needed
  unique_fill_values <- unique(points_in_zupanije[[fill]])
  if (is.character(palette) && !is.null(names(palette))) {
    # Custom palette provided
    if (!all(unique_fill_values %in% names(palette))) {
      missing_values <- setdiff(unique_fill_values, names(palette))
      stop("The custom palette is missing colors for: ", paste(missing_values, collapse = ", "))
    }
    colors <- palette
  } else if (is.character(palette) && length(palette) == 1) {
    # Predefined palette
    color_count <- length(unique_fill_values)
    if (palette == "viridis") {
      colors <- viridis::viridis(color_count)
    } else if (palette == "plasma") {
      colors <- viridis::plasma(color_count)
    } else if (palette == "rainbow") {
      colors <- rainbow(color_count)
    } else {
      colors <- scales::hue_pal()(color_count)
    }
    names(colors) <- unique_fill_values
  } else {
    stop("Invalid palette. Use a predefined palette name or a named list of colors.")
  }

  # Build the ggplot
  p <- ggplot() +
    geom_sf(data = zupanije_filtered, fill = layer_fill_color, color = layer_border_color, size = layer_border_width) +
    geom_sf(
      data = points_in_zupanije,
      aes_string(color = fill, shape = shape),
      size = dot_size
    ) +
    scale_color_manual(values = colors) +
    theme_minimal()

  # Add custom title and axis labels
  if (!is.null(title)) {
    p <- p + ggtitle(title)
  }
  if (!is.null(x_axis_label)) {
    p <- p + xlab(x_axis_label)
  }
  if (!is.null(y_axis_label)) {
    p <- p + ylab(y_axis_label)
  }

  # Customize the legends if titles are provided
  if (!is.null(legend_fill_title)) {
    p <- p + scale_color_manual(name = legend_fill_title, values = colors)
  }
  if (!is.null(legend_shape_title)) {
    p <- p + scale_shape_discrete(name = legend_shape_title)
  }

  return(p)
}

#################################### 3
#' Plot Observations Within a Polygon
#'
#' This function plots observations within a user-defined polygon,
#' providing both a detailed plot with observations and a location overview.
#'
#' @param fill Column name for coloring points. Default is "taxon.name".
#' @param shape Column name for point shapes. Default is NULL (no shapes).
#' @param polygon_coords A data frame or list of data frames defining polygon(s) with longitude and latitude columns.
#' @param df A dataframe containing latitude and longitude columns, as well as any additional data for plotting.
#' @param layer Map layer to use for the background: "rh" or "zupanije". Default is "zupanije".
#' @param zupanija Vector of Zupanija names to filter and plot. Default is NULL (plots all).
#' @param padding Padding in meters around the polygon to adjust the map extent. Default is 0.
#' @param title Title for the plot with observations. Default is NULL.
#' @param x_axis_label Label for the X-axis. Default is NULL.
#' @param y_axis_label Label for the Y-axis. Default is NULL.
#' @param legend_fill_title Title for the fill legend. Default is NULL.
#' @param legend_shape_title Title for the shape legend. Default is NULL.
#' @param dot_size Size of the observation points. Default is 2.
#' @param layer_fill_color Fill color for the map layer. Default is "lightblue".
#' @param layer_border_color Border color for the map layer. Default is "black".
#' @param layer_border_width Border width for the map layer. Default is 0.5.
#' @param polygon_fill_color Fill color for the polygon. Default is "orange".
#' @param polygon_border_color Border color for the polygon. Default is "red".
#' @param polygon_border_width Border width for the polygon. Default is 1.
#' @param palette Color palette for points. Can be a predefined palette (e.g., "viridis")
#' or a custom named vector. Default is "viridis".
#'
#' @return A list containing two ggplot objects:
#'   - `plot_with_points`: Plot showing the observations within the polygon.
#'   - `polygon_location`: Plot showing only the polygon over the selected map layer.
#' @export
plot.polygon <- function(fill = "taxon.name",
                         shape = NULL,
                         polygon_coords,
                         df,
                         layer = "zupanije",
                         zupanija = NULL,
                         padding = 0,
                         title = NULL,
                         x_axis_label = NULL,
                         y_axis_label = NULL,
                         legend_fill_title = NULL,
                         legend_shape_title = NULL,
                         dot_size = 2,
                         layer_fill_color = "lightblue",
                         layer_border_color = "black",
                         layer_border_width = 0.5,
                         polygon_fill_color = "orange",
                         polygon_border_color = "red",
                         polygon_border_width = 1,
                         palette = "viridis") {
  library(sf)
  library(ggplot2)
  library(scales)

  # Handle single polygon or list of polygons
  if (inherits(polygon_coords, "data.frame")) {
    polygon_coords_list <- list(polygon_coords)
  } else if (is.list(polygon_coords) && all(sapply(polygon_coords, inherits, "data.frame"))) {
    polygon_coords_list <- polygon_coords
  } else {
    stop("polygon_coords must be a data frame or a list of data frames.")
  }

  # Create sf polygons with closing points
  create_polygon <- function(coords) {
    # Ensure the first and last points match to 'close' the polygon
    if (!all(coords[1, ] == coords[nrow(coords), ])) {
      coords <- rbind(coords, coords[1, ])
    }
    sf::st_polygon(list(as.matrix(coords)))
  }

  polygons_sfg <- lapply(polygon_coords_list, create_polygon)
  polygons_sfc <- sf::st_sfc(polygons_sfg, crs = 4326)
  polygons_sf <- sf::st_sf(geometry = polygons_sfc)

  # Calculate bounding box with optional padding
  if (padding > 0) {
    combined_poly <- sf::st_union(polygons_sf)
    centroid <- sf::st_centroid(combined_poly)
    coords <- sf::st_coordinates(centroid)
    lon <- coords[1, "X"]
    lat <- coords[1, "Y"]
    zone <- floor((lon + 180) / 6) + 1
    hemisphere <- ifelse(lat >= 0, "north", "south")
    crs_utm <- sprintf("+proj=utm +zone=%d +%s +datum=WGS84", zone, hemisphere)

    combined_poly_utm <- sf::st_transform(combined_poly, crs = crs_utm)
    buffered_utm <- sf::st_buffer(combined_poly_utm, dist = padding)
    buffered_poly <- sf::st_transform(buffered_utm, crs = 4326)
    bbox <- sf::st_bbox(buffered_poly)
  } else {
    bbox <- sf::st_bbox(polygons_sf)
  }
  xlim <- c(bbox["xmin"], bbox["xmax"])
  ylim <- c(bbox["ymin"], bbox["ymax"])

  # Remove rows with missing coordinates
  df <- df[!is.na(df$latitude) & !is.na(df$longitude), ]
  message(nrow(df), " rows remain after removing NA coordinates.")
  df_sf <- sf::st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

  # Load the requested map layer
  if (layer == "rh") {
    shapefile_path <- system.file("extdata", "RH_2018.shp", package = "biologerR")
    if (shapefile_path == "") stop("RH_2018 shapefile not found in the biologerR package.")
    map_layer <- sf::st_read(shapefile_path, quiet = TRUE)
  } else if (layer == "zupanije") {
    json_path <- system.file("extdata", "zupanije.json", package = "biologerR")
    if (json_path == "") stop("Zupanije GeoJSON not found in the biologerR package.")
    map_layer <- sf::st_read(json_path, quiet = TRUE)

    # Filter if specific zupanija is provided
    if (!is.null(zupanija)) {
      map_layer <- map_layer[map_layer$name %in% zupanija, ]
      if (nrow(map_layer) == 0) {
        stop("None of the specified Zupanija names were found in the map layer.")
      }
    }
  } else {
    stop("Invalid layer. Use 'rh' or 'zupanije'.")
  }

  # Ensure map layer has correct CRS
  map_layer <- sf::st_transform(map_layer, crs = 4326)

  # Filter points that fall within the polygons
  points_in_poly <- sf::st_join(df_sf, polygons_sf, join = sf::st_within, left = FALSE)

  # Prepare color palette
  unique_fill <- unique(points_in_poly[[fill]])
  if (is.null(unique_fill)) {
    warning("No points found within the polygon(s). No points will be plotted.")
    unique_fill <- character(0) # handle empty
  }

  if (is.character(palette) && !is.null(names(palette))) {
    # A custom named palette was provided
    if (!all(unique_fill %in% names(palette))) {
      missing <- setdiff(unique_fill, names(palette))
      stop("The provided palette is missing colors for: ", paste(missing, collapse = ", "))
    }
    colors <- palette
  } else if (is.character(palette) && length(palette) == 1) {
    # One of the predefined palettes
    n <- length(unique_fill)
    colors <- switch(palette,
      "viridis" = viridis::viridis(n),
      "plasma"  = viridis::plasma(n),
      "rainbow" = rainbow(n),
      # default fallback:
      scales::hue_pal()(n)
    )
    names(colors) <- unique_fill
  } else {
    stop("Invalid palette. Use a named vector of colors or a predefined palette (e.g., 'viridis').")
  }

  # Main plot (with points)
  plot_main <- ggplot() +
    geom_sf(
      data = map_layer,
      fill = layer_fill_color,
      color = layer_border_color,
      linewidth = layer_border_width
    ) +
    geom_sf(
      data = polygons_sf,
      fill = polygon_fill_color,
      color = polygon_border_color,
      linewidth = polygon_border_width
    ) +
    # Only add geom_sf for points if we have any
    {
      if (nrow(points_in_poly) > 0) {
        geom_sf(
          data = points_in_poly,
          aes_string(color = fill, shape = shape),
          size = dot_size
        )
      }
    } +
    {
      if (length(unique_fill) > 0) {
        scale_color_manual(values = colors)
      }
    } +
    coord_sf(xlim = xlim, ylim = ylim) +
    labs(
      title = title,
      x = x_axis_label,
      y = y_axis_label,
      color = legend_fill_title,
      shape = legend_shape_title
    ) +
    theme_minimal()

  # Location overview plot (just the polygons and the chosen layer)
  plot_location <- ggplot() +
    geom_sf(
      data = map_layer,
      fill = layer_fill_color,
      color = layer_border_color,
      linewidth = layer_border_width
    ) +
    geom_sf(
      data = polygons_sf,
      fill = polygon_fill_color,
      color = polygon_border_color,
      linewidth = polygon_border_width
    ) +
    coord_sf() +
    labs(title = "Polygon Location Overview") +
    theme_minimal()

  # Return both plots in a list
  return(list(
    plot_with_points = plot_main,
    polygon_location = plot_location
  ))
}


#' Count Observations Per Polygon
#'
#' Counts the number of observations that fall within each provided polygon.
#'
#' @param fill Column name to group and count rows by. Default is "taxon.name".
#' @param polygon_coords A data frame or list of data frames defining polygon(s) with longitude and latitude columns.
#' @param polygon_names Optional vector of names for the polygons. If not provided, polygons will be numbered.
#' @param df A dataframe containing latitude and longitude columns, as well as the column specified in fill.
#'
#' @return A dataframe with counts for each polygon and a TOTAL row.
#' @export
polygon.counts <- function(fill = "taxon.name", polygon_coords, polygon_names = NULL, df) {
  library(sf)
  library(dplyr)

  # Remove rows with missing coordinates
  df <- df[!is.na(df$latitude) & !is.na(df$longitude), ]
  message(nrow(df), " rows remain after removing rows with missing latitude or longitude.")

  # Handle single polygon or list of polygons
  if (inherits(polygon_coords, "data.frame")) {
    polygon_coords_list <- list(polygon_coords)
  } else if (is.list(polygon_coords) && all(sapply(polygon_coords, inherits, "data.frame"))) {
    polygon_coords_list <- polygon_coords
  } else {
    stop("polygon_coords must be a data frame or a list of data frames.")
  }

  # Create polygon names if not provided
  if (is.null(polygon_names)) {
    polygon_names <- paste("Polygon", seq_along(polygon_coords_list))
  }
  if (length(polygon_names) != length(polygon_coords_list)) {
    stop("Length of polygon_names must match the number of polygons.")
  }

  # Create sf polygons with closing points
  create_polygon <- function(coords) {
    if (!all(coords[1, ] == coords[nrow(coords), ])) {
      coords <- rbind(coords, coords[1, ])
    }
    sf::st_polygon(list(as.matrix(coords)))
  }

  polygons_sfg <- lapply(polygon_coords_list, create_polygon)
  polygons_sfc <- sf::st_sfc(polygons_sfg, crs = 4326)
  polygons_sf <- sf::st_sf(
    name = polygon_names,
    geometry = polygons_sfc
  )

  # Convert the data to an sf object
  df_sf <- sf::st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

  # Spatial join to find which polygon each point falls into
  joined_data <- sf::st_join(df_sf, polygons_sf, join = sf::st_within)

  # Count rows per polygon and grouping variable
  if (fill == "none") {
    counts <- joined_data %>%
      group_by(name) %>%
      summarise(count = n(), .groups = "drop") %>%
      mutate(!!fill := "ALL") # Add a default "ALL" column when no grouping
  } else {
    counts <- joined_data %>%
      group_by(!!sym(fill), name) %>%
      summarise(count = n(), .groups = "drop")
  }

  # Add TOTAL row for each group and for the entire dataset
  total_per_group <- counts %>%
    group_by(!!sym(fill)) %>%
    summarise(name = "TOTAL", count = sum(count), .groups = "drop")

  total_overall <- counts %>%
    summarise(!!sym(fill) := "TOTAL", name = "TOTAL", count = sum(count), .groups = "drop")

  # Combine everything into a single dataframe
  final_counts <- bind_rows(counts, total_per_group, total_overall)

  # Arrange for better readability
  final_counts <- final_counts %>%
    arrange(!!sym(fill), name)

  # Print the result
  print(final_counts)
  return(final_counts)
}


###############################################################
#' Plot Observations Within EEA Grid Square(s)
#'
#' This function plots observations within user-selected EEA grid square(s). It provides both
#' a detailed plot with observations (if any fall within the squares) and a location overview
#' of the selected squares.
#'
#' @param fill Column name for coloring points. Default is "taxon.name".
#' @param shape Column name for point shapes. Default is NULL (no shapes). If provided, this
#'   column must exist in \code{df}; otherwise shape aesthetics are omitted.
#' @param grid_type Which grid layer to use: "1km" or "10km". Default is "1km".
#' @param square_id A vector of square IDs to filter and plot (e.g., c("10kmE459N247", "10kmE460N244")).
#'   Default is NULL (plots all squares in the chosen grid).
#' @param df A data frame containing \strong{at least} `longitude` and `latitude` columns, plus
#'   any additional columns you want for \code{fill} or \code{shape}.
#' @param layer Map layer to use for the background: "rh" or "zupanije". Default is "zupanije".
#' @param zupanija Vector of Zupanija names to filter and plot. Default is NULL (plots all).
#' @param padding Padding in meters around the selected square(s) to adjust the map extent. Default is 0.
#' @param title Title for the plot with observations. Default is NULL.
#' @param x_axis_label Label for the X-axis. Default is NULL.
#' @param y_axis_label Label for the Y-axis. Default is NULL.
#' @param legend_fill_title Title for the fill legend. Default is NULL.
#' @param legend_shape_title Title for the shape legend. Default is NULL.
#' @param dot_size Size of the observation points. Default is 2.
#' @param layer_fill_color Fill color for the map layer. Default is "lightblue".
#' @param layer_border_color Border color for the map layer. Default is "black".
#' @param layer_border_width Border width for the map layer. Default is 0.5.
#' @param grid_fill_color Fill color for the selected grid squares. Default is "orange".
#' @param grid_border_color Border color for the selected grid squares. Default is "red".
#' @param grid_border_width Border width for the selected grid squares. Default is 1.
#' @param palette Color palette for points. Can be a predefined palette (e.g., "viridis", "plasma",
#'   "rainbow") or a custom named vector. Default is "viridis".
#'
#' @return A list containing two ggplot objects:
#'   \itemize{
#'     \item \code{plot_with_points}: Plot showing the observations (if any) within the selected square(s).
#'     \item \code{square_location}: Plot showing only the selected square(s) over the chosen map layer.
#'   }
#' @export
#'
#' @examples
#' \dontrun{
#' # Example data
#' my_data <- data.frame(
#'   taxon.name = c("Species A", "Species B"),
#'   longitude  = c(16.0, 15.5),
#'   latitude   = c(45.8, 45.9),
#'   shape_col  = c("Circle", "Triangle") # Example shape column
#' )
#'
#' # Plot single 10×10 km square with shape
#' single_10km <- plot.squares(
#'   fill               = "taxon.name",
#'   shape              = "shape_col",
#'   grid_type          = "10km",
#'   square_id          = c("10kmE459N248"),
#'   df                 = my_data,
#'   layer              = "zupanije",
#'   zupanija           = c("Zagreb"),
#'   padding            = 5000,
#'   title              = "Single 10×10 km Square",
#'   x_axis_label       = "Longitude",
#'   y_axis_label       = "Latitude",
#'   legend_fill_title  = "Taxon Legend",
#'   legend_shape_title = "Shape Legend",
#'   dot_size           = 3,
#'   layer_fill_color   = "grey90",
#'   layer_border_color = "blue",
#'   layer_border_width = 0.8,
#'   grid_fill_color    = "yellow",
#'   grid_border_color  = "red",
#'   grid_border_width  = 1.5,
#'   palette            = "plasma"
#' )
#'
#' # Access the plots
#' single_10km$plot_with_points
#' single_10km$square_location
#' }
plot.squares <- function(fill = "taxon.name",
                         shape = NULL,
                         grid_type = c("1km", "10km"),
                         square_id = NULL,
                         df,
                         layer = "zupanije",
                         zupanija = NULL,
                         padding = 0,
                         title = NULL,
                         x_axis_label = NULL,
                         y_axis_label = NULL,
                         legend_fill_title = NULL,
                         legend_shape_title = NULL,
                         dot_size = 2,
                         layer_fill_color = "lightblue",
                         layer_border_color = "black",
                         layer_border_width = 0.5,
                         grid_fill_color = "orange",
                         grid_border_color = "red",
                         grid_border_width = 1,
                         palette = "viridis") {
  # Load required packages
  library(sf)
  library(ggplot2)
  library(scales)

  grid_type <- match.arg(grid_type)

  #------------------------------------------------------------------------
  # 1) Load the EEA grid shapefile (with × in filenames) from your package
  #------------------------------------------------------------------------
  if (grid_type == "1km") {
    grid_path <- system.file("extdata", "EEA_mreza", "RH_EEA_1×1km.shp", package = "biologerR")
    if (grid_path == "") {
      stop("1×1 km EEA shapefile not found in the biologerR package. Please check your installation.")
    }
    grid_layer <- sf::st_read(grid_path, quiet = TRUE)
  } else {
    grid_path <- system.file("extdata", "EEA_mreza", "RH_EEA_10×10km.shp", package = "biologerR")
    if (grid_path == "") {
      stop("10×10 km EEA shapefile not found in the biologerR package. Please check your installation.")
    }
    grid_layer <- sf::st_read(grid_path, quiet = TRUE)

    # Rename CellCode -> cellcode if needed
    if ("CellCode" %in% names(grid_layer) && !"cellcode" %in% names(grid_layer)) {
      grid_layer <- dplyr::rename(grid_layer, cellcode = "CellCode")
    }
  }

  # Make sure grid layer is in WGS84
  grid_layer <- sf::st_transform(grid_layer, crs = 4326)

  #------------------------------------------------------------------------
  # 2) Filter squares if square_id provided; else all squares
  #------------------------------------------------------------------------
  if (!is.null(square_id)) {
    sel_idx <- which(grid_layer$cellcode %in% square_id)
    if (length(sel_idx) == 0) {
      stop("None of the specified square_id values were found in the chosen grid.")
    }
    grid_layer <- grid_layer[sel_idx, ]
  }

  #------------------------------------------------------------------------
  # 3) Compute bounding box with optional padding
  #------------------------------------------------------------------------
  combined_grid <- sf::st_union(grid_layer)
  if (padding > 0) {
    centroid <- sf::st_centroid(combined_grid)
    coords <- sf::st_coordinates(centroid)
    lon <- coords[1, "X"]
    lat <- coords[1, "Y"]
    zone <- floor((lon + 180) / 6) + 1
    hemisphere <- ifelse(lat >= 0, "north", "south")
    crs_utm <- sprintf("+proj=utm +zone=%d +%s +datum=WGS84", zone, hemisphere)

    combined_grid_utm <- sf::st_transform(combined_grid, crs = crs_utm)
    buffered_utm <- sf::st_buffer(combined_grid_utm, dist = padding)
    buffered_grid <- sf::st_transform(buffered_utm, crs = 4326)
    bbox <- sf::st_bbox(buffered_grid)
  } else {
    bbox <- sf::st_bbox(combined_grid)
  }
  xlim <- c(bbox["xmin"], bbox["xmax"])
  ylim <- c(bbox["ymin"], bbox["ymax"])

  #------------------------------------------------------------------------
  # 4) Prepare points data: remove NAs, convert to sf
  #------------------------------------------------------------------------
  df <- df[!is.na(df$latitude) & !is.na(df$longitude), ]
  message(nrow(df), " rows remain after removing NA coordinates.")

  df_sf <- sf::st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

  #------------------------------------------------------------------------
  # 5) Load the map layer (rh or zupanije), filter by zupanija if needed
  #------------------------------------------------------------------------
  if (layer == "rh") {
    shapefile_path <- system.file("extdata", "RH_2018.shp", package = "biologerR")
    if (shapefile_path == "") {
      stop("RH_2018 shapefile not found in the biologerR package. Please check your installation.")
    }
    map_layer <- sf::st_read(shapefile_path, quiet = TRUE)
  } else if (layer == "zupanije") {
    json_path <- system.file("extdata", "zupanije.json", package = "biologerR")
    if (json_path == "") {
      stop("Zupanije GeoJSON not found in the biologerR package. Please check your installation.")
    }
    map_layer <- sf::st_read(json_path, quiet = TRUE)

    if (!is.null(zupanija)) {
      map_layer <- map_layer[map_layer$name %in% zupanija, ]
      if (nrow(map_layer) == 0) {
        stop("None of the specified Zupanija names were found in the map layer.")
      }
    }
  } else {
    stop("Invalid layer. Use 'rh' or 'zupanije'.")
  }

  map_layer <- sf::st_transform(map_layer, crs = 4326)

  #------------------------------------------------------------------------
  # 6) Join points with the selected grid squares
  #------------------------------------------------------------------------
  points_in_grid <- sf::st_join(df_sf, grid_layer, join = sf::st_within, left = FALSE)

  #------------------------------------------------------------------------
  # 7) Prepare color palette
  #------------------------------------------------------------------------
  unique_fill <- unique(points_in_grid[[fill]])
  if (is.null(unique_fill) || length(unique_fill) == 0) {
    # No points found
    warning("No points found within the selected square(s). Plotting squares only.")
    unique_fill <- character(0)
  }

  if (is.character(palette) && !is.null(names(palette))) {
    # A custom named palette was provided
    if (!all(unique_fill %in% names(palette))) {
      missing <- setdiff(unique_fill, names(palette))
      stop("The provided palette is missing colors for: ", paste(missing, collapse = ", "))
    }
    colors <- palette
  } else if (is.character(palette) && length(palette) == 1) {
    # A known palette name
    n <- length(unique_fill)
    colors <- switch(palette,
      "viridis" = viridis::viridis(n),
      "plasma"  = viridis::plasma(n),
      "rainbow" = rainbow(n),
      scales::hue_pal()(n) # fallback
    )
    names(colors) <- unique_fill
  } else {
    stop("Invalid palette. Use a named vector of colors or a predefined palette (e.g., 'viridis').")
  }

  #------------------------------------------------------------------------
  # 8) Build the main plot
  #------------------------------------------------------------------------
  # We will conditionally include the points layer if we have any points.
  # Also we check if the shape column is valid in points_in_grid.

  have_points <- nrow(points_in_grid) > 0
  shape_is_valid <- have_points && !is.null(shape) && shape %in% names(points_in_grid)

  # Start the plot with the base layer (map + grid squares)
  plot_main <- ggplot() +
    geom_sf(
      data = map_layer,
      fill = layer_fill_color,
      color = layer_border_color,
      linewidth = layer_border_width
    ) +
    geom_sf(
      data = grid_layer,
      fill = grid_fill_color,
      color = grid_border_color,
      linewidth = grid_border_width
    )

  # If there are any points within the squares, add them
  if (have_points) {
    if (shape_is_valid) {
      plot_main <- plot_main +
        geom_sf(
          data = points_in_grid,
          aes_string(color = fill, shape = shape),
          size = dot_size
        )
    } else {
      # shape column doesn't exist => color only
      plot_main <- plot_main +
        geom_sf(
          data = points_in_grid,
          aes_string(color = fill),
          size = dot_size
        )
    }
    # scale_color_manual for fill
    if (length(unique_fill) > 0) {
      plot_main <- plot_main + scale_color_manual(values = colors)
    }
  }

  # Add the rest of the labels, coords, theme
  plot_main <- plot_main +
    coord_sf(xlim = xlim, ylim = ylim) +
    labs(
      title = title,
      x = x_axis_label,
      y = y_axis_label,
      color = legend_fill_title,
      shape = legend_shape_title
    ) +
    theme_minimal()

  #------------------------------------------------------------------------
  # 9) Build the location overview (no points, just squares & map)
  #------------------------------------------------------------------------
  plot_location <- ggplot() +
    geom_sf(
      data = map_layer,
      fill = layer_fill_color,
      color = layer_border_color,
      linewidth = layer_border_width
    ) +
    geom_sf(
      data = grid_layer,
      fill = grid_fill_color,
      color = grid_border_color,
      linewidth = grid_border_width
    ) +
    coord_sf() +
    labs(title = "Selected Square(s) Location Overview") +
    theme_minimal()

  #------------------------------------------------------------------------
  # 10) Return both plots in a list
  #------------------------------------------------------------------------
  return(list(
    plot_with_points = plot_main,
    square_location  = plot_location
  ))
}


########################################
#' Count Observations per EEA Grid Square
#'
#' Counts the number of observations that fall within each EEA grid square (1×1 km or 10×10 km).
#'
#' @param fill Character. Column name to group and count rows by. Default is `"taxon.name"`.
#'   If you set \code{fill = "none"}, it won't group by any column and will count total
#'   observations per square.
#' @param grid_type Which grid layer to use: `"1km"` or `"10km"`. Default is `"1km"`.
#' @param square_id Optional vector of square IDs (e.g., \code{"10kmE459N247"}) to filter.
#'   If not provided, all squares in the chosen grid layer will be included.
#' @param df A data frame containing \strong{at least} \code{longitude}, \code{latitude}, and
#'   any column specified by \code{fill}.
#'
#' @return A \code{data.frame} with counts for each square (identified by \code{cellcode})
#'   and a \strong{TOTAL} row. Columns:
#'   \itemize{
#'     \item The grouping column specified by \code{fill} (or "ALL" if \code{fill = "none"}).
#'     \item \code{cellcode} (the grid square ID or "TOTAL").
#'     \item \code{count} (the number of observations).
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Example data with 2 observations:
#' obs <- data.frame(
#'   taxon.name = c("Species A", "Species B"),
#'   longitude  = c(16.0, 15.5),
#'   latitude   = c(45.8, 45.9)
#' )
#'
#' # Count how many observations of each taxon fall into each 10km square
#' # that you specify:
#' grid_counts <- squares.count(
#'   fill      = "taxon.name",
#'   grid_type = "10km",
#'   square_id = c("10kmE459N247", "10kmE460N244"),
#'   df        = obs
#' )
#'
#' # If you want to count all squares in the 1km grid, do:
#' all_1km_counts <- squares.count(
#'   fill      = "taxon.name",
#'   grid_type = "1km",
#'   square_id = NULL, # all squares
#'   df        = obs
#' )
#'
#' # Or if you just want total counts per square (no grouping),
#' # use fill = "none":
#' all_10km_nogroup <- squares.count(
#'   fill      = "none",
#'   grid_type = "10km",
#'   df        = obs
#' )
#' }
squares.count <- function(fill = "taxon.name",
                          grid_type = c("1km", "10km"),
                          square_id = NULL,
                          df) {
  # Required packages
  library(sf)
  library(dplyr)

  grid_type <- match.arg(grid_type)

  # 1) Remove rows with missing lat/long in the observation data
  df <- df[!is.na(df$latitude) & !is.na(df$longitude), ]
  message(nrow(df), " rows remain after removing rows with missing latitude or longitude.")

  # 2) Load the requested EEA grid (1km or 10km) from your package
  if (grid_type == "1km") {
    grid_path <- system.file("extdata", "EEA_mreza", "RH_EEA_1×1km.shp", package = "biologerR")
    if (grid_path == "") {
      stop("1×1 km EEA shapefile not found in the biologerR package. Please check your installation.")
    }
    squares_sf <- sf::st_read(grid_path, quiet = TRUE)
  } else {
    grid_path <- system.file("extdata", "EEA_mreza", "RH_EEA_10×10km.shp", package = "biologerR")
    if (grid_path == "") {
      stop("10×10 km EEA shapefile not found in the biologerR package. Please check your installation.")
    }
    squares_sf <- sf::st_read(grid_path, quiet = TRUE)
    # Rename CellCode -> cellcode if needed
    if ("CellCode" %in% names(squares_sf) && !"cellcode" %in% names(squares_sf)) {
      squares_sf <- dplyr::rename(squares_sf, cellcode = "CellCode")
    }
  }

  # Make sure squares are in WGS84
  squares_sf <- sf::st_transform(squares_sf, crs = 4326)

  # 3) Optionally filter squares by square_id
  if (!is.null(square_id)) {
    sel_idx <- which(squares_sf$cellcode %in% square_id)
    if (length(sel_idx) == 0) {
      stop("None of the specified square_id values were found in the chosen grid.")
    }
    squares_sf <- squares_sf[sel_idx, ]
  }

  # 4) Convert observation data to sf
  df_sf <- sf::st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

  # 5) Spatial join: which square does each observation fall into?
  joined_data <- sf::st_join(df_sf, squares_sf, join = sf::st_within)

  # 6) Summarize the counts per square
  if (fill == "none") {
    # No grouping => just total counts per square
    counts <- joined_data %>%
      group_by(cellcode) %>%
      summarise(count = n(), .groups = "drop") %>%
      mutate(!!fill := "ALL") # add a filler column for uniform structure
  } else {
    # Group by the specified fill column + the square cellcode
    counts <- joined_data %>%
      group_by(!!sym(fill), cellcode) %>%
      summarise(count = n(), .groups = "drop")
  }

  # 7) Create TOTAL row(s)
  #  - One total row per fill group
  #  - One overall TOTAL row
  total_per_group <- counts %>%
    group_by(!!sym(fill)) %>%
    summarise(cellcode = "TOTAL", count = sum(count), .groups = "drop")

  total_overall <- counts %>%
    summarise(!!sym(fill) := "TOTAL", cellcode = "TOTAL", count = sum(count))

  # Combine them into one data frame
  final_counts <- bind_rows(counts, total_per_group, total_overall)

  # 8) Arrange for better readability
  final_counts <- final_counts %>%
    arrange(!!sym(fill), cellcode)

  # 9) Print and return
  print(final_counts)
  return(final_counts)
}
