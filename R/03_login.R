#' @title Sets the Biologer server and access token
#'
#' @description This function allows user to set the Biologer server (sr, hr, ba
#' or me) and access token for the API calls. Obtain access token from User >
#' Preferences > API Tokens. Your server and access token will be saved in local 
#' ~/.Renviron file and loaded automatically in your R session.
#'
#' @param server Alias of the Biologer server. Available values are: 'rs', 'hr', 'bs', and 'me'.
#' @param token String representing the access token obtained from the user preferences
#' on the Biologer web page.
#'
#' @return Invisible logical calue (TRUE) if the server and token are successfully saved.
#' The function stops the execution and displays the error if the server/token is invalid.
#'
#' @note
#' To obtain access token, the user must be logged in to Biologer web page using your browser.
#' Here you can obtain the token by selecting User > Preferences > API Tokens > Generate token.
#' The variables will be set automatically for current session, but you need to restart R session
#' for the changes to became permanent.
#'
#' @examples
#' \dontrun{
#' # Example: Setting the access token for Croatian Biologer server
#' biologer.login(
#'   server = "hr"
#'   token = "TOKEN_OBRAINED FROM THE BROWSER",
#' )
#'
#' # Taken and URL are not availavle in internal package functions.
#' }
#'
#' @export
biologer_login <- function(server = "rs", token) {
  # 1. Check the imput parameters
  if (missing(token) || !is.character(token) || nchar(token) < 20) {
    stop("Please enter a valid access token.", call. = FALSE)
  }

  server_key <- tolower(server)

  if (!server_key %in% names(BIOLOGER_URLS)) {
    stop(paste0("Unknown Biologer platform selected. Available options are: ",
                paste(names(BIOLOGER_URLS), collapse = ", ")), call. = FALSE)
  }

  base_url <- BIOLOGER_URLS[[server_key]]

  # 2. Save data to .Renviron
  token_env_var <- paste0("BIOLOGER_TOKEN_", toupper(server_key))
  url_env_var <- paste0("BIOLOGER_BASE_URL_", toupper(server_key))


  # Check if the user is already logged in
  if (nzchar(Sys.getenv(token_env_var))) {
    stop(paste0("⚠️ You are already logged in to the '", server_key,
                "' server (", base_url, ").\n",
                "Please first log out using: biologer_logout(server = \"",
                server_key, "\")"),
         call. = FALSE)
  }

  # Write the values to the env file
  write_to_renviron(token_env_var, token)
  write_to_renviron(url_env_var, base_url)

  message(paste0("Access token and server (", base_url, ") is successfully saved ",
                 "in your .Renviron file and loaded in the current session.\n"))

  invisible(TRUE)
}

#' @title Logs out from a specific Biologer server and deletes local data
#'
#' @description This function removes the access token and base URL for a
#' specified Biologer server from the current R session, deletes them from
#' the local ~/.Renviron file (requires a helper function), and deletes
#' all downloaded data files associated with that server.
#'
#' @param server Alias of the Biologer server to log out from.
#' Available values are: 'rs', 'hr', 'bs', and 'me'.
#'
#' @return Invisible logical value (TRUE) if successful.
#' The function stops the execution and displays an error if the server is unknown.
#'
#' @note
#' To fully remove variables from your persistent .Renviron file, this function
#' assumes the existence of a helper function, e.g., 'remove_from_renviron(var_name)'.
#' You will need to implement this helper function in your package.
#'
#' @examples
#' \dontrun{
#' # Example: Logging out from the Croatian Biologer server
#' biologer_logout(server = "hr")
#' }
#'
#' @export
biologer_logout <- function(server = "rs") {

  server_key <- tolower(server)

  if (!server_key %in% names(BIOLOGER_URLS)) {
    stop(paste0("Unknown Biologer platform selected. Available options are: ",
                paste(names(BIOLOGER_URLS), collapse = ", ")), call. = FALSE)
  }

  token_env_var <- paste0("BIOLOGER_TOKEN_", toupper(server_key))
  url_env_var <- paste0("BIOLOGER_BASE_URL_", toupper(server_key))

  # Check if already logged in
  if (!nzchar(Sys.getenv(token_env_var))) {
    message(paste0("You are already logged out from the '", server_key,
                   "' server. No action necessary."))
    invisible(FALSE)
    return(invisible(FALSE))
  }

  # Delete local database
  storage_path <- get_storage_path()
  file_suffix <- paste0("_", server_key, ".csv")

  base_filenames <- c(
    "biologer_field_observations",
    "biologer_literature_observations",
    "biologer_taxa"
  )

  files_to_delete <- c()
  for (name in base_filenames) {
    # .csv data files
    data_file <- file.path(storage_path, paste0(name, file_suffix))
    files_to_delete <- c(files_to_delete, data_file)

    # .rds checkpoint files
    checkpoint_file <- paste0(data_file, ".checkpoint.rds")
    files_to_delete <- c(files_to_delete, checkpoint_file)
  }

  deleted_count <- 0
  for (f in files_to_delete) {
    if (file.exists(f)) {
      unlink(f)
      deleted_count <- deleted_count + 1
    }
  }

  # Clean the env file
  Sys.unsetenv(token_env_var)
  Sys.unsetenv(url_env_var)
  remove_from_renviron(token_env_var)
  remove_from_renviron(url_env_var)

  message(paste0("Successfully logged out from '", server_key, "'."))
  message(paste0("Token and URL variables (", token_env_var, ", ", url_env_var, ") removed from current session."))

  if (deleted_count > 0) {
    message(paste0("Local data files (", deleted_count, " files) for '",
                   server_key, "' were deleted from ", storage_path, "."))
  } else {
    message(paste0("No local data files found to delete for '", server_key, "'."))
  }

  invisible(TRUE)
}