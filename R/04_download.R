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