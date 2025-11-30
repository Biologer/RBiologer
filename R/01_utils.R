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