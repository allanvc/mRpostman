#' @title IMAP Connection Configuration
#' @description Configure and create a new IMAP connection.
#' @param url A character string containing the IMAP server address
#' @param username A character string containing the username.
#' @param password A character string containing the user's password.
#' @param xoauth2_bearer A character string containing the oauth2 bearer token.
#' @param use_ssl A logical indicating the use or not of Secure Sockets Layer
#'   encryption when connecting to the IMAP server. Default is \code{TRUE}.
#' @param verbose If \code{FALSE}, mutes the flow of information between the
#'   server and the client. Default is \code{FALSE}.
#' @param buffersize The size in bytes for the receive buffer. Default is
#'   16000 bytes or 16kb, which means it will use the libcurl's default value.
#'   According to the libcurl's documentation, the maximum buffersize is 512kb
#'   (or 512000 bytes), but any number passed to \code{buffersize} is treated
#'   as a request, not an order.
#' @param timeout_ms Time in milliseconds (ms) to wait for the execution or
#'   re-execution of a command. Default is 0, which means that no timeout limit is
#'   set.
#' @param ... Further curl parameters (see \code{curl::curl_options}) that
#'   can be used with the IMAP protocol. Only for advanced users.
#' @return A new `ImapCon` object.
#' @export
#' @examples
#' \dontrun{
#' # w/ Plain authentication
#' con <- configure_imap(
#'   url="imaps://outlook.office365.com",
#'   username="user@agency.gov.br",
#'   password=rstudioapi::askForPassword(),
#'   verbose = TRUE)
#'
#' # w/ OAuth2.0 authentication
#' con <- configure_imap(
#'   url="imaps://outlook.office365.com",
#'   username="user@agency.gov.br",
#'   verbose = TRUE,
#'   xoauth2_bearer = "XX.Ya9...")
#' }
configure_imap <- function(url,
                           username,
                           password = NULL,
                           xoauth2_bearer = NULL,
                           use_ssl = TRUE,
                           verbose = FALSE,
                           buffersize = 16000,
                           timeout_ms = 0,
                           ...) {

  con <- ImapCon$new(url,
                     username,
                     password = password,
                     xoauth2_bearer = xoauth2_bearer,
                     use_ssl = use_ssl,
                     verbose = verbose,
                     buffersize = buffersize,
                     timeout_ms = timeout_ms,
                     ...)

  # fresh_connect is not supported anymore because the same handle is used during an
  #  IMAP session. We let the handle manage the connection pool

  return(con)

}
