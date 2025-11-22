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

#' Get the internal storage directory for the package
#' @keywords internal
get_storage_path <- function(package = "RBiologer") {
  storage_path <- tools::R_user_dir(package, which = "data")

  if (!dir.exists(storage_path)) {
    dir.create(storage_path, recursive = TRUE)
  }

  return(storage_path)
}

#' @keywords internal
get_biologer_base_url <- function() {
  url <- Sys.getenv("BIOLOGER_BASE_URL")
  if (url == "") {
    stop("Biologer server URL is not found. Please use the function
         'biologer_login(token, server)' to setup tour base URL.", call. = FALSE)
  }
  url
}

#' @keywords internal
get_biologer_token <- function() {
  token <- Sys.getenv("BIOLOGER_TOKEN")
  if (token == "") {
    stop("Biologer access token not found. Use helper function 
         'biologer_login(token, server)' to set a new token.", call. = FALSE)
  }
  token
}