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
  write_to_renviron("BIOLOGER_TOKEN", token)
  write_to_renviron("BIOLOGER_BASE_URL", base_url)

  message(paste0("Access token and server (", base_url, ") is successfully saved ",
                 "in your .Renviron file and loaded in the current session.\n"))

  invisible(TRUE)
}