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
    paste(photos_df$url, collapse = "|")
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

  paste(translated_pairs, collapse = "|")
}

#' Collapse nested 'stages' data frame into a separated string of stage names.
#' @description
#' The output format is 'ID=Name', e.g., '5=egg|4=adult'.
#' @param stages_df A data frame containing 'id' and 'name' columns, or an empty list/data frame.
#' @keywords internal
collapse_stages <- function(stages_df) {
  if (is.data.frame(stages_df) &&
    nrow(stages_df) > 0 &&
    "name" %in% names(stages_df) &&
    "id" %in% names(stages_df)) {
    id_name_pairs <- paste(stages_df$id, stages_df$name, sep = "=")
    paste(id_name_pairs, collapse = "|")
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
    paste(synonyms_df$name, collapse = "|")
  } else {
    ""
  }
}

#' Check if a single activity record is a relevant identification event (creation or change).
#'
#' @param activity_row A single row (as a list) from the activity data frame.
#' @return TRUE if it's a taxon-related event (created/updated with taxon data), FALSE otherwise.
#' @keywords internal
is_identification_event <- function(activity_row) {
  # Check for explicit change event
  is_change <- is_taxon_change_event(activity_row$properties)

  # Check for initial creation
  is_creation <- (activity_row$description == "created")

  # Return if there was a creation event or an explicit update
  is_change || is_creation
}

#' Check if a single activity record is a taxon change event.
#'
#' @param property_list A single list element from the 'properties' column.
#' @return TRUE if it's a taxon change, FALSE otherwise.
#' @keywords internal
is_taxon_change_event <- function(property_list) {
  # Check for 'old'
  if (!is.list(property_list) || !("old" %in% names(property_list))) {
    return(FALSE)
  }

  old_list <- property_list$old

  # Check for 'taxon' inside 'old'
  if (!is.list(old_list) || !("taxon" %in% names(old_list))) {
    return(FALSE)
  }

  taxon_list <- old_list$taxon

  # Check for 'label' inside 'taxon'
  if (!is.list(taxon_list) || !("label" %in% names(taxon_list))) {
    return(FALSE)
  }

  taxon_label <- taxon_list$label

  # Return TRUE if the label is a non-empty string
  if (is.character(taxon_label) && length(taxon_label) == 1 && nchar(taxon_label) > 0) {
    return(TRUE)
  }

  FALSE
}

#' Collapse all taxa change records from the nested 'activity' column into a single string.
#'
#' @param activity_df A data frame containing activity records for a single observation.
#' @return A character string of collapsed taxa changes, or an empty string if none are found.
#' @keywords internal
collapse_activity_taxa_changes <- function(activity_df) {
  if (!is.data.frame(activity_df) || nrow(activity_df) == 0 || !("properties" %in% names(activity_df))) {
    return("")
  }

  # Apply the helper function to every list element in the 'properties' column
  # and filter out the NULL results
  taxa_changes <- unlist(lapply(activity_df$properties, extract_taxa_from_activity))
  taxa_changes <- taxa_changes[!sapply(taxa_changes, is.null)]

  if (length(taxa_changes) > 0) {
    # Collapse all found changes into a single pipe-separated string
    paste(taxa_changes, collapse = "|")
  } else {
    ""
  }
}

#' Extract the old taxon name from a single 'properties' list element.
#'
#' It searches for 'properties$old$taxon$label'.
#'
#' @param property_list A single list element from the 'properties' column.
#' @return The old taxon name (character string) or an empty string if not found.
#' @keywords internal
extract_taxa_from_activity <- function(property_list) {
  if (!is.list(property_list) || !("old" %in% names(property_list))) {
    return(NULL)
  }

  old_list <- property_list$old

  if (!is.list(old_list) || !("taxon" %in% names(old_list))) {
    return(NULL)
  }

  taxon_list <- old_list$taxon

  if (!is.list(taxon_list) || !("label" %in% names(taxon_list))) {
    return(NULL)
  }

  taxon_label <- taxon_list$label

  if (is.character(taxon_label) && length(taxon_label) == 1 && nchar(taxon_label) > 0) {
    return(taxon_label)
  }

  NULL
}

#' Extracts the latest 'created_at' date of any successful taxon identification event
#' and formats it to the preferred ISO 8601 UTC format (YYYY-MM-DDTHH:MM:SSZ).
#'
#' @param activity_df A data frame containing activity records for a single observation.
#' @return The latest 'created_at' date string formatted to 'YYYY-MM-DDTHH:MM:SSZ', or NA if none are found.
#' @keywords internal
latest_identification_date <- function(activity_df) {
  if (!is.data.frame(activity_df) || nrow(activity_df) == 0 || !("created_at" %in% names(activity_df))) {
    return(NA_character_)
  }
  is_relevant_event <- apply(activity_df, MARGIN = 1, FUN = is_identification_event)
  relevant_events <- activity_df[is_relevant_event, ]
  if (nrow(relevant_events) == 0) {
    return(NA_character_)
  }
  latest_date_str <- max(relevant_events$created_at, na.rm = TRUE)
  if (is.infinite(latest_date_str)) {
    return(NA_character_)
  }
  cleaned_date_str <- sub("\\.\\d+Z$", "Z", latest_date_str)
  latest_date_utc <- as.POSIXct(
    cleaned_date_str,
    format = "%Y-%m-%dT%H:%M:%S",
    tz = "UTC"
  )
  formatted_date <- format(
    latest_date_utc,
    format = "%Y-%m-%dT%H:%M:%SZ",
    tz = "UTC"
  )
  formatted_date
}

#' @title Extracts the absolute latest 'created_at' date from any activity event.
#' @description
#' Finds the latest 'created_at' date regardless of the event type (identification,
#' comment, etc.) and formats it to the preferred ISO 8601 UTC format.
#'
#' @param activity_df A data frame containing activity records for a single observation.
#' @return The latest 'created_at' date string formatted to 'YYYY-MM-DDTHH:MM:SSZ', or NA if none are found.
#' @keywords internal
latest_activity_date <- function(activity_df) {
  if (!is.data.frame(activity_df) || nrow(activity_df) == 0 || !("created_at" %in% names(activity_df))) {
    return(NA_character_)
  }

  # Find the maximum 'created_at' date across ALL rows
  latest_date_str <- max(activity_df$created_at, na.rm = TRUE)

  if (is.infinite(latest_date_str) || is.na(latest_date_str)) {
    return(NA_character_)
  }

  # Clean and format the date string to ensure consistency (removes sub-second decimals)
  cleaned_date_str <- sub("\\.\\d+Z$", "Z", latest_date_str)

  # Parse the date string, specifying it is already in UTC
  latest_date_utc <- as.POSIXct(
    cleaned_date_str,
    format = "%Y-%m-%dT%H:%M:%S",
    tz = "UTC"
  )

  # Format the date back into the desired Darwon Core / ISO 8601 string
  formatted_date <- format(
    latest_date_utc,
    format = "%Y-%m-%dT%H:%M:%SZ",
    tz = "UTC"
  )

  formatted_date
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

#' @title Creates a unique ID-to-Name map from all stage definitions in the taxonomy table.
#' @description
#' Parses the 'stages' column (assumed format ID=Name|ID=Name) and builds a map.
#'
#' @param taxa_df data.table containing the 'stages' column with stage definitions.
#' @return Named character vector (Stage ID -> Stage Name).
create_stage_map <- function(taxa_df) {
  if (!("stages" %in% names(taxa_df))) {
    return(c())
  }

  # 1. Select and clean the stage definitions
  stage_defs <- na.omit(unique(taxa_df$stages))
  if (length(stage_defs) == 0) {
    return(c())
  }

  # 2. Split all definitions into single ID=Name pairs
  all_pairs <- unlist(strsplit(stage_defs, "|", fixed = TRUE))

  # 3. Split pairs into IDs and Names
  all_ids <- sub("=.*", "", all_pairs)
  all_names <- sub(".*=", "", all_pairs)

  # 4. Create the final named vector map, prioritizing unique keys
  # (Use rev() to prioritize later definitions, or keep unique(.) to prioritize first)
  stage_id_map <- setNames(
    object = all_names,
    nm = as.character(all_ids)
  )

  # Remove duplicates, keeping the last unique definition
  stage_id_map[!duplicated(names(stage_id_map), fromLast = TRUE)]
}

#' @title Maps stage IDs to names and formats the Darwin Core 'lifeStage' column.
#' @description
#' Uses a dynamic ID->Name map to convert the single stage ID from the 'stage_id'
#' column into the appropriate Darwin Core 'lifeStage' name. The function modifies
#' the input data.table 'dt' by reference.
#'
#' @param dt data.table object containing the single numeric ID in 'stage_id' column.
#' @param stage_id_map Named character vector where names are the numeric IDs (character
#'        type) and values are the stage names.
#' @return The modified data.table (returned by reference and invisibly).
#' @import data.table
map_stages_to_lifestage <- function(data, stage_id_map) {
  # 1. Map IDs to Names
  if (!("stage_id" %in% names(data))) {
    data[, lifeStage := NA_character_]
    data[, `:=`(stages = NULL)] # Clean up 'stages'
    return(invisible(data))
  }
  data[, temp_ids := as.character(stage_id)] # Ensure it is a character
  data[, lifeStage := stage_id_map[temp_ids]]

  # 2. Cleanup
  data[, `:=`(temp_ids = NULL)]

  invisible(data)
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
        "Downloading ",
        format(records_fetched, big.mark = ","), "/",
        format(total_records, big.mark = ","),
        " records (",
        round(progress_percentage), "%)  "
      )
    } else {
      # Total_records is unknown or zero
      output_string <- paste0(
        output_string,
        "Downloaded ",
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
      message("Progress: Downloaded ", format(records_fetched, big.mark = ","), " records so far.")
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
#' @import data.table
#' @export
get_data_from_api <- function(api_url, token, filename = NULL, columns_to_keep = NULL, verbose = TRUE) {
  if (is.null(filename)) {
    message("Due to its size, data should be saved into a file. Please provide CSV file for saving data.")
    return(NULL)
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
      if (verbose == TRUE) {
        message(paste0("Starting incremental fetch after timestamp: ", last_success_timestamp))
      }
    }
  } else {
    # Initial Full Download
    url <- get_fetch_start_url(api_url, last_success_timestamp)
    if (verbose == TRUE) {
      message("Starting initial full fetch from URL: ", url)
    }
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
          if (verbose == TRUE) {
            message("The data is already downloaded. No need to update…")
          }
          break
        } else {
          message("Total records to download: ", format(total_records, big.mark = ","))
        }
      }
    }

    # Add this page
    current_count <- nrow(data$data)
    if (is.null(current_count) || current_count == 0) {
      message("No more new records returned by server. Finalizing...")
      url <- NULL # This ensures the checkpoint logic below triggers the 'complete' state
    } else {
      records_fetched <- records_fetched + current_count
    }

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
    # Process the 'activity' column to extract and collapse taxa changes
    if ("activity" %in% names(current_page_data) && is.list(current_page_data$activity)) {
      # 1. Extract the latest identification date
      current_page_data$dateIdentified <- unlist(lapply(
        current_page_data$activity,
        latest_identification_date
      ))

      # 2. Extract the list of old taxon names
      current_page_data$previousIdentifications <- unlist(lapply(
        current_page_data$activity,
        collapse_activity_taxa_changes
      ))

      # 3. Extract the latest modification date
      current_page_data$modified <- unlist(lapply(
        current_page_data$activity,
        latest_activity_date
      ))

      # 4. Remove obsolete column
      current_page_data$activity <- NULL
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
    # Sys.sleep(0.5)
  }

  # Remove duplicated entries (only keep the new ones)
  if (file.exists(filename)) {
    full_data <- data.table::fread(filename, fill = TRUE)
    is_duplicate <- duplicated(full_data$id, fromLast = TRUE)
    final_data <- full_data[!is_duplicate, ]
    if (nrow(full_data) > nrow(final_data)) {
      message(
        "Removing duplicated data to keep the new online changes. ",
        "Total rows before: ", nrow(full_data), "; after: ",
        nrow(final_data)
      )
      data.table::fwrite(final_data, file = filename)
    }
  }

  if (verbose == TRUE) {
    message("The data is saved locally in ", filename)
  }
}

#' Fetch simple data from the API that does not require pagination and timestamps
#'
#' @param api_url URL of the Biologer API used to access the data.
#' @param token JWT authorization token.
#' @keywords internal
#' @export
get_data_from_simple_api <- function(api_url, token, filename = NULL) {
  repeat {
    h <- curl::new_handle()
    curl::handle_setheaders(h, Authorization = paste("Bearer", token), Accept = "application/json")
    result <- curl::curl_fetch_memory(api_url, handle = h)

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
      return(NULL)
    }

    data <- jsonlite::fromJSON(rawToChar(result$content), flatten = TRUE)
    if (is.null(filename)) {
      return(data$data)
    } else {
      data.table::fwrite(data$data, file = filename)
    }
  }
}

#' Main function used to get data from all logged-in Biologer servers
#'
#' @description This function iterates through all servers for which credentials
#' are stored and downloads field observations, literature observations, and
#' taxonomic data for each active server.
#'
#' @export
get_biologer_data <- function(verbose = TRUE) {
  tokens <- get_biologer_token()
  urls <- get_biologer_base_url()

  active_servers <- intersect(names(tokens), names(urls))
  if (length(active_servers) == 0) {
    stop("No active server credentials found. Please use 'biologer_login(server, token)' ",
      "for at least one server.",
      call. = FALSE
    )
  }

  storage_path <- get_storage_path()

  for (server_key in active_servers) {
    server_url <- urls[server_key]
    server_token <- tokens[server_key]
    server_display <- toupper(server_key)

    if (verbose == TRUE) {
      message("\n========================================================")
      message(paste0("  STARTING DATA DOWNLOAD FOR SERVER: ", server_display))
      message("========================================================\n")
    }

    # File suffix is now server-specific (e.g., _rs.csv)
    file_suffix <- paste0("_", server_key, ".csv")

    # ========================================================
    # 1. Downloading Field Observation data
    # ========================================================

    if (verbose == TRUE) {
      message(paste0("* Step 1 [", server_display, "]: Downloading Field Observation data."))
    }
    get_data_from_api(
      api_url = paste0(server_url, "/public-field-observations"),
      token = server_token,
      # Server-specific filename
      filename = file.path(storage_path, paste0("biologer_field_observations", file_suffix)),
      columns_to_keep = FIELD_OBS_COLUMNS,
      verbose = verbose
    )

    # ========================================================
    # 2. Downloading Literature Observation data
    # ========================================================

    if (verbose == TRUE) {
      message(paste0("* Step 2 [", server_display, "]: Downloading Literature Observation data."))
    }
    get_data_from_api(
      api_url = paste0(server_url, "/literature-observations"),
      token = server_token,
      # Server-specific filename
      filename = file.path(storage_path, paste0("biologer_literature_observations", file_suffix)),
      columns_to_keep = LITERATURE_OBS_COLUMNS,
      verbose = verbose
    )

    # ========================================================
    # 3. Downloading Taxonomic data
    # ========================================================

    if (verbose == TRUE) {
      message(paste0("* Step 3 [", server_display, "]: Downloading Taxonomic data."))
    }
    get_data_from_api(
      api_url = paste0(server_url, "/taxa"),
      token = server_token,
      # Server-specific filename
      filename = file.path(storage_path, paste0("biologer_taxa", file_suffix)),
      columns_to_keep = TAXA_COLUMNS,
      verbose = verbose
    )
  }

  if (verbose == TRUE) {
    message("\n--------------------------------------------------------")
    message("All data downloads completed.")
    message("--------------------------------------------------------")
  }

  invisible(TRUE)
}

#' @title Load and Consolidate Biologer Data
#' @description This function reads, joins, and consolidates observation (Field and
#' Literature) and taxonomic data downloaded from one or more Biologer servers. It
#' performs server-specific joins with taxonomy data and then merges all resulting
#' observations into a single, comprehensive \code{data.table}.
#' @param auto.download Logical. If \code{TRUE} (default), the function first calls
#' \code{\link{get_biologer_data}()} to download the latest data from all configured
#' Biologer servers before consolidation. If \code{FALSE}, it only loads existing
#' files from the storage path.
#' @return A single \code{data.table} containing all consolidated and merged observation
#' records. This \code{data.table} includes columns from both Field and Literature
#' observations, with non-matching columns filled with \code{NA}.
#' @details The function performs the following steps:
#' \enumerate{
#'   \item For each active Biologer server, it reads three files: Taxonomy (\code{biologer_taxa}),
#'         Field Observations (\code{biologer_field_observations}), and Literature Observations
#'         (\code{biologer_literature_observations}).
#'   \item It performs a left join (1:N) between the Observation dataframes and the Taxonomy
#'         dataframe using \code{taxon.id} (Observation) and \code{id} (Taxonomy).
#'   \item It uses \code{data.table::fread} for fast file reading and \code{data.table::rbindlist}
#'         with \code{fill = TRUE} for fast and safe row-wise consolidation across different servers and
#'         between the Field and Literature datasets.
#' }
#' @references Requires the \code{data.table} package.
#' @seealso \code{\link{get_biologer_data}}, \code{\link{biologer_login}}
#' @import data.table
#' @export
open_data <- function(auto.download = TRUE, verbose = TRUE) {
  # Get new data from the servers
  if (auto.download == TRUE) {
    get_biologer_data(verbose = verbose)
  }

  storage_path <- get_storage_path()
  active_servers <- intersect(names(get_biologer_token()), names(get_biologer_base_url()))

  if (length(active_servers) == 0) {
    stop("No active server credentials found. Cannot determine which files to load.
         Please use biologer_login() to add at least one Biologer server.", call. = FALSE)
  }

  all_field_data <- list()
  all_literature_data <- list()

  get_filename <- function(prefix, server_key) {
    file.path(storage_path, paste0(prefix, "_", server_key, ".csv"))
  }

  if (verbose == TRUE) {
    message("========================================================")
    message("  STARTING DATA CONSOLIDATION")
    message("========================================================")
  }

  for (server_key in active_servers) {
    server_display <- toupper(server_key)
    if (verbose == TRUE) {
      message(paste0("\n* Processing server: ", server_display))
    }

    taxa_file <- get_filename("biologer_taxa", server_key)
    field_file <- get_filename("biologer_field_observations", server_key)
    literature_file <- get_filename("biologer_literature_observations", server_key)

    # ========================================================
    # 1. Load and update Taxonomic data
    # ========================================================
    if (!file.exists(taxa_file)) {
      message(paste0("  - Skipping ", server_display, ": Taxonomic file is missing, use get_biologer_data()"))
      next
    }
    taxa_df <- data.table::fread(taxa_file, stringsAsFactors = FALSE, fill = TRUE)
    taxa_df[rank == "speciescomplex", rank := "species complex"] # Fix wrong naming
    taxa_df <- get_parent_taxa(taxa_df)
    taxa_df[, taxonomicStatus := data.table::fifelse(
      rank == "species complex",
      "unaccepted",
      "accepted"
    )]
    if ("translations" %in% names(taxa_df)) {
      locale_code <- "en"
      pattern <- paste0("^.*(?:\\||^)", locale_code, "=(.*?)(?:\\||$).*$")
      taxa_df[, vernacularName := sub(pattern, "\\1", translations, perl = TRUE)]
      taxa_df[vernacularName == translations | is.na(translations), vernacularName := NA_character_]
      taxa_df[vernacularName == "", vernacularName := NA_character_]
    } else {
      taxa_df$vernacularName <- NA_character_
    }
    stage_id_map <- create_stage_map(taxa_df)
    if (verbose && length(stage_id_map) > 0) {
      message(paste0("  - Stage map created with ", length(stage_id_map), " unique entries."))
    }

    # ========================================================
    # 2. Load and update Field data
    # ========================================================
    if (file.exists(field_file)) {
      col_types <- c(dateIdentified = "character")
      field_df <- data.table::fread(
        field_file,
        stringsAsFactors = FALSE,
        fill = TRUE,
        colClasses = col_types
      )
      # Add other required columns
      if (length(stage_id_map) > 0) {
        field_df <- map_stages_to_lifestage(field_df, stage_id_map)
      } else {
        field_df[, lifeStage := NA_character_]
      }
      field_df$basisOfRecord <- "HumanObservation"
      if ("types" %in% names(field_df)) {
        field_df[, typeOfRecord := fifelse(
          grepl("Photographed", types, fixed = TRUE),
          "Observed|Photographed",
          fifelse(
            grepl("Observed", types, fixed = TRUE),
            "Observed",
            NA_character_
          )
        )]
        field_df[, `dcterms:type` := fifelse(
          grepl("Photographed", types, fixed = TRUE),
          "StillImage",
          fifelse(
            grepl("Observed|Call|Exuviae", types),
            "Event",
            NA_character_
          )
        )]
      } else {
        field_df$typeOfRecord <- NA_character_
        field_df$`dcterms:type` <- NA_character_
      }
      field_df[, `dcterms:rightsHolder` := paste0("biologer.", server_key, " community")]
      field_df[, `dcterms:accessRights` :=
        fifelse(
          license == 10, "CC BY-SA 4.0",
          fifelse(
            license == 20, "CC BY-NC-SA 4.0",
            fifelse(
              license == 30, "CC BY-NC-SA 4.0 (limited coordinates)",
              fifelse(
                license == 35, "CC BY-SA 4.0 (closed for 3 years)",
                fifelse(
                  license == 40, "Closed",
                  NA_character_
                )
              )
            )
          )
        )]
      field_df[, `dcterms:license` :=
        fifelse(
          license == 10, "https://creativecommons.org/licenses/by-sa/4.0/",
          fifelse(
            license == 20, "https://creativecommons.org/licenses/by-nc-sa/4.0/",
            fifelse(
              license == 30, paste0("https://biologer.", server_key, "/licenses/partially-open-data-license"),
              fifelse(
                license == 35, paste0("https://biologer.", server_key, "/licenses/temporarily-closed-data-license"),
                fifelse(
                  license == 40, paste0("https://biologer.", server_key, "/licenses/closed-data-license"),
                  NA_character_
                )
              )
            )
          )
        )]

      if (verbose == TRUE) {
        message("  - Joining Field Observations with Taxonomy.")
      }
      field_merged <- merge(
        field_df,
        taxa_df,
        by.x = "taxon.id",
        by.y = "id",
        all.x = TRUE,
        sort = FALSE
      )
      all_field_data[[server_key]] <- field_merged
    } else {
      if (verbose == TRUE) {
        message("  - Field Observations file is missing!!!")
      }
    }

    # ========================================================
    # 3. Load and update Literature data
    # ========================================================
    if (file.exists(literature_file)) {
      literature_df <- data.table::fread(
        literature_file,
        stringsAsFactors = FALSE,
        fill = TRUE
      )
      # Add other required columns
      if (length(stage_id_map) > 0) {
        literature_df <- map_stages_to_lifestage(literature_df, stage_id_map)
      } else {
        literature_df[, lifeStage := NA_character_]
      }
      literature_df$basisOfRecord <- "Literature"
      literature_df$typeOfRecord <- NA_character_
      literature_df$`dcterms:type` <- "Text"
      literature_df[, `dcterms:rightsHolder` := paste0("biologer.", server_key, " community")]
      literature_df$`dcterms:accessRights` <- "CC BY-SA 4.0"
      literature_df$`dcterms:license` <- "https://creativecommons.org/licenses/by-sa/4.0/"

      if (verbose == TRUE) {
        message("  - Joining Literature Observations with Taxonomy.")
      }
      literature_merged <- merge(
        literature_df,
        taxa_df,
        by.x = "taxon.id",
        by.y = "id",
        all.x = TRUE,
        sort = FALSE
      )
      all_literature_data[[server_key]] <- literature_merged
    } else {
      if (verbose == TRUE) {
        message("  - Literature Observations file is missing!!!")
      }
    }
  }

  # ========================================================
  # 4. Merging data from all the servers
  # ========================================================

  if (verbose == TRUE) {
    message("\n* Merging data from all the servers.")
  }
  merged_field_df <- data.table::rbindlist(all_field_data, fill = TRUE)
  merged_literature_df <- data.table::rbindlist(all_literature_data, fill = TRUE)

  if (verbose == TRUE) {
    message("\n* Final consolidation of Field and Literature data.")
  }
  final_dataset <- data.table::rbindlist(list(merged_field_df, merged_literature_df), fill = TRUE)

  # ========================================================
  # 5. Add aditional columns to the final dataset
  # ========================================================

  # 1. Create the minimumElevationInMeters column
  final_dataset[, minimumElevationInMeters := data.table::fifelse(
    !is.na(minimum_elevation),
    minimum_elevation,
    elevation
  )]

  # 2. Create the maximumElevationInMeters column
  final_dataset[, maximumElevationInMeters := data.table::fifelse(
    !is.na(maximum_elevation) & (maximum_elevation != minimum_elevation),
    maximum_elevation,
    NA_real_
  )]

  # Make the custom JSON term in dynamicProperties
  final_dataset[, found_dead_note := as.character(found_dead_note)]
  final_dataset[, project := as.character(project)]
  final_dataset[, dynamicProperties_parts := ""]
  # Add project name
  final_dataset[
    !is.na(project) & project != "",
    dynamicProperties_parts := paste0(dynamicProperties_parts, '"project": "', project, '"')
  ]
  # Add the dead comment and status
  final_dataset[
    found_dead == TRUE,
    dynamicProperties_parts := paste0(
      dynamicProperties_parts,
      data.table::fifelse(dynamicProperties_parts != "", ", ", ""),
      '"organismCondition": "Dead"'
    )
  ]
  final_dataset[
    !is.na(found_dead_note) & found_dead_note != "",
    dynamicProperties_parts := paste0(
      dynamicProperties_parts,
      data.table::fifelse(dynamicProperties_parts != "", ", ", ""),
      '"mortalityNote": "', found_dead_note, '"'
    )
  ]
  final_dataset[
    dynamicProperties_parts != "",
    dynamicProperties := paste0("{", dynamicProperties_parts, "}")
  ]
  final_dataset[dynamicProperties_parts == "", dynamicProperties := NA_character_]
  # Get the atlas code for bird breeding and notes into a single column
  final_dataset[, breeding_status := atlas_code_map[as.character(atlas_code)]]
  final_dataset[, note := data.table::fifelse(
    !is.na(breeding_status) & !is.na(note) & note != "",
    paste0(note, " | Breeding Status: ", breeding_status),
    data.table::fifelse(
      !is.na(breeding_status),
      paste0("Breeding Status: ", breeding_status),
      note
    )
  )]

  # Get the UTC dates correctly since they are in timezone Europe/Belgrade
  process_biologer_dates_to_utc(final_dataset)

  # Get the data for literature records that only has year and month range
  if ("collecting_start_year" %in% names(final_dataset)) {
    # Step 1a: Construct the start date string (YYYY-MM or YYYY)
    final_dataset[, eventDate_start_lit := data.table::fifelse(
      !is.na(collecting_start_year) & !is.na(collecting_start_month),
      paste0(collecting_start_year, "-", sprintf("%02d", collecting_start_month)), # YYYY-MM
      as.character(collecting_start_year) # YYYY
    )]

    # Step 1b: Construct the end date string (YYYY-MM or YYYY)
    final_dataset[, eventDate_end_lit := data.table::fifelse(
      !is.na(collecting_end_year) & !is.na(collecting_end_month),
      paste0(collecting_end_year, "-", sprintf("%02d", collecting_end_month)), # YYYY-MM
      as.character(collecting_end_year) # YYYY
    )]
    # Step 2: Define the target range string (YYYY/YYYY or YYYY-MM/YYYY-MM)
    final_dataset[, lit_date_range := data.table::fifelse(
      # Start and end are different
      !is.na(eventDate_start_lit) & !is.na(eventDate_end_lit) & (eventDate_start_lit != eventDate_end_lit),
      paste0(eventDate_start_lit, "/", eventDate_end_lit),
      # Start and end are the same
      eventDate_start_lit
    )]
    # Update eventDate
    final_dataset[, eventDate := data.table::fifelse(
      is.na(eventDate) & !is.na(lit_date_range),
      lit_date_range,
      eventDate
    )]
  }

  if (verbose == TRUE) {
    message("\n--------------------------------------------------------")
    message(paste0("Final dataset created with ", nrow(final_dataset), " rows and ", ncol(final_dataset), " columns."))
    message("--------------------------------------------------------")
  }

  # Renaming dataset to fitt into the DarwinCore
  data.table::setnames(
    final_dataset,
    c(
      "id", "taxon.id", "author", "rank",
      "name", "identifier", "latitude", "longitude",
      "accuracy", "location", "photos",
      "observer", "number", "note", "found_on",
      "georeferenced_by", "georeferenced_date",
      "original_date", "original_locality", "original_elevation", "original_coordinates",
      "original_identification", "original_identification_validity", "other_original_data",
      "place_where_referenced_in_publication", "publication.citation"
    ),
    c(
      "occurrenceID", "taxonID", "scientificNameAuthorship", "taxonRank",
      "scientificName", "identifiedBy", "decimalLatitude", "decimalLongitude",
      "coordinateUncertaintyInMeters", "locality", "associatedMedia",
      "recordedBy", "individualCount", "occurrenceRemarks", "substrate",
      "georeferencedBy", "georeferencedDate",
      "verbatimEventDate", "verbatimLocality", "verbatimElevation", "verbatimCoordinates",
      "verbatimIdentification", "identificationVerificationStatus", "verbatimLabel",
      "occurrenceDetails", "bibliographicCitation"
    )
  )

  # Remove columns that we don't need
  columns_to_drop <- c(
    "rank_level", "uses_atlas_codes", "ancestors_names", "license", "stages", "stage_id",
    "elevation", "minimum_elevation", "maximum_elevation", "project", "found_dead",
    "found_dead_note", "dynamicProperties_parts", "taxon.name", "parent_id", "breeding_status",
    "collecting_start_year", "collecting_start_month", "collecting_end_year", "collecting_end_month",
    "eventDate_start_lit", "eventDate_end_lit", "lit_date_range"
  )
  final_dataset[, (columns_to_drop) := NULL]

  # Reorder columns
  data.table::setcolorder(final_dataset, DWC_COLUMN_ORDER)

  return(final_dataset)
}

#' Extract and Map Parent Taxa Ranks to New Columns
#'
#' Takes a data.table containing taxonomic data with comma-separated ancestry
#' strings and maps the ancestor names to their official rank (e.g., 'kingdom',
#' 'phylum') as new columns in the data.table. This process is optimized for
#' performance using data.table's `tstrsplit`, `melt`, and `dcast`.
#'
#' @param data_table A \code{data.table} object. It must contain the columns:
#'   \itemize{
#'     \item \code{ancestors_names} (character): The comma-separated string of parent taxa names.
#'     \item \code{name} (character): The name of the taxon (used for rank lookup).
#'     \item \code{rank} (character): The taxonomic rank of the taxon (used for new column headers).
#'   }
#'
#' @return A \code{data.table} object that is a copy of the input, augmented with
#'   new columns. These new columns are named after the unique taxonomic ranks
#'   found (e.g., \code{kingdom}, \code{phylum}), and are populated with the
#'   corresponding ancestor name from the \code{ancestors_names} string.
#'   Temporary columns (\code{RowID}, \code{TaxaName_*}) are removed.
#'
#' @import data.table stringi
#' @importFrom data.table :=
#' @importFrom data.table .I
#' @importFrom data.table .SD
#' @export
#' @import data.table stringi
#' @import data.table stringi
#' @import data.table stringi
get_parent_taxa <- function(data_table) {
  DT <- data.table::copy(data_table) # Make a copy of the data_table
  DT[, RowID := .I] # Create ID
  DT[, rank := tolower(trimws(rank))] # Clean up

  # Create a lookup table (Name -> Rank)
  rank_lookup <- unique(DT[, .(name, rank)])

  # 1. Process Ancestors (Split & Melt)
  max_ranks <- max(stringi::stri_count_fixed(DT$ancestors_names, ","), na.rm = TRUE) + 1
  new_cols <- paste0("TaxaName_", 1:max_ranks)
  # Split ancestors into temporary columns
  DT[, (new_cols) := data.table::tstrsplit(ancestors_names, ",", fixed = TRUE)]
  # Melt ancestors to long format.
  # Result table: RowID | variable | TaxaName (ancestor name)
  long_ancestors <- data.table::melt(
    DT,
    id.vars = "RowID",
    measure.vars = new_cols,
    value.name = "TaxaName",
    variable.name = "SplitID"
  )
  # Remove entries where TaxaName is NA (empty ancestors)
  long_ancestors <- na.omit(long_ancestors, cols = "TaxaName")

  # Join with rank_lookup to get the 'rank' for each ancestor name
  # Result table: RowID | TaxaName | rank
  long_ancestors_mapped <- rank_lookup[long_ancestors, on = c(name = "TaxaName")]

  # 2. Process Current Taxon
  # Create a long-format table for the CURRENT row's taxon
  # Result table: RowID | TaxaName | rank
  long_current <- DT[, .(RowID, TaxaName = name, rank)]

  # 3. Combine and Pivot
  # Stack ancestors and current taxon into one list.
  combined_long <- rbind(
    long_ancestors_mapped[, .(RowID, TaxaName = name, rank)],
    long_current
  )

  # Pivot to Wide Format
  # This creates the final columns: RowID, kingdom, phylum, class, species, etc.
  final_ranks_wide <- data.table::dcast(
    combined_long,
    RowID ~ rank,
    value.var = "TaxaName",
    fun.aggregate = function(x) x[1] # Take the first match if duplicates exist (rare)
  )

  # Identify the new rank columns we just created (excluding RowID)
  new_rank_cols <- setdiff(names(final_ranks_wide), "RowID")

  # Remove these columns from the original DT if they exist.
  cols_to_drop <- intersect(names(DT), new_rank_cols)
  if (length(cols_to_drop) > 0) {
    DT[, (cols_to_drop) := NULL]
  }

  # Remove the temporary split columns from DT
  DT[, (new_cols) := NULL]

  # Merge the new, fully populated rank columns back into DT
  result <- final_ranks_wide[DT, on = "RowID"]

  # Remove the columns we don't need
  redundant_rank_cols <- c("species", "subspecies", "species complex", "speciescomplex")
  cols_to_remove <- intersect(names(result), redundant_rank_cols)
  if (length(cols_to_remove) > 0) {
    result[, (cols_to_remove) := NULL]
  }

  # Clean up RowID
  result[, RowID := NULL]

  # Get the specificEpithet column (2nd part of the name column for species and subspecies)
  result[, specificEpithet := NA_character_]
  target_ranks <- c("species", "subspecies")
  result[
    rank %in% target_ranks,
    specificEpithet := data.table::tstrsplit(name, " ", fixed = TRUE, keep = 2L)
  ]

  # Get the infraspecificEpithet column (3nd part of the name column for subspecies)
  result[, infraspecificEpithet := NA_character_]
  result[
    rank %in% "subspecies",
    infraspecificEpithet := data.table::tstrsplit(name, " ", fixed = TRUE, keep = 2L)
  ]

  # Get the acceptedNameUsage column from name and author
  result[, acceptedNameUsage := name]
  result[
    !is.na(author) & author != "",
    acceptedNameUsage := paste(name, author, sep = " ")
  ]
  result[
    rank == "species complex",
    acceptedNameUsage := ""
  ]

  # Get the verbatimScientificName column from name and author, but ass "subsp." for plants
  result[, verbatimScientificName := name]
  result[
    !is.na(author) & author != "",
    verbatimScientificName := paste(name, author, sep = " ")
  ]
  result[
    kingdom == "Plantae" & rank == "subspecies" & !is.na(author) & author != "",
    verbatimScientificName := paste(name, author, sep = " subsp. ")
  ]

  result
}

#' @title Process and Convert Biologer Dates to UTC
#' @description
#' Cleans raw year/month/day/time columns, converts local Belgrade time
#' (CET/CEST) to UTC, and formats the output into standard Darwin Core fields.
#' Modifies the input data.table 'dt' by reference.
#'
#' @param dt data.table object containing 'year', 'month', 'day', and 'time' columns.
#' @return The modified data.table (returned invisibly).
#' @import data.table
process_biologer_dates_to_utc <- function(dt) {
  # --- 1. Initial Setup and Cleaning ---
  # Add missing date columns (year, month, day, time) if they don't exist
  required_date_cols <- c("year", "month", "day", "time")
  for (col in required_date_cols) {
    if (!(col %in% names(dt))) {
      dt[, (col) := NA_integer_]
    }
  }

  # Prepare observation date parts and presence flag
  dt[, `:=`(
    obs_year = year,
    obs_month = data.table::fifelse(is.na(month), 1L, month),
    obs_day = data.table::fifelse(is.na(day), 1L, day),
    is_time_present = !is.na(time) & time != ""
  )]

  # --- 2. Local Timezone Parsing and UTC Conversion (for records with time) ---
  # Combine date and time into a single string for parsing
  dt[
    is_time_present == TRUE,
    datetime_local_str := paste(
      paste(
        obs_year,
        sprintf("%02d", obs_month),
        sprintf("%02d", obs_day),
        sep = "-"
      ),
      time,
      sep = " "
    )
  ]

  # Parse local time (Belgrade tz, handling DST)
  dt[
    is_time_present == TRUE,
    datetime_local := as.POSIXct(
      paste0(datetime_local_str, ":00"),
      format = "%Y-%m-%d %H:%M:%S",
      tz = "Europe/Belgrade"
    )
  ]

  # Format the UTC output from the parsed local time object
  dt[
    is_time_present == TRUE,
    `:=`(
      eventTime = format(datetime_local, format = "%H:%M:%S", tz = "UTC"),
      eventDate = format(datetime_local, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    )
  ]

  # --- 3. Date-Only Formatting (for records without time) ---
  # Create date_part for YYYY, YYYY-MM, or YYYY-MM-DD
  dt[, date_part := data.table::fifelse(
    !is_time_present & !is.na(day) & !is.na(month),
    paste(obs_year, obs_month, obs_day, sep = "-"),
    data.table::fifelse(
      !is_time_present & !is.na(month),
      paste(obs_year, obs_month, sep = "-"),
      data.table::fifelse(!is_time_present, as.character(obs_year), NA_character_)
    )
  )]

  # Assign final values for date-only records
  dt[
    is_time_present == FALSE,
    `:=`(
      eventTime = NA_character_,
      eventDate = date_part
    )
  ]

  # --- 4. Cleanup ---
  # Remove temporary and source columns
  dt[, `:=`(
    datetime_local_str = NULL,
    datetime_local = NULL,
    date_part = NULL,
    obs_year = NULL,
    obs_month = NULL,
    obs_day = NULL,
    is_time_present = NULL,
    time = NULL
  )]

  invisible(dt)
}
