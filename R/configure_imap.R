#' @title IMAP Connection Configuration
#' @description Configure and create a new IMAP connection.
#' @param url A character string containing the IMAP server address
#' @param username A character string containing the username.
#' @param password A character string containing the user's password.
#' @param xoauth2_bearer A character string containing the oauth2 bearer token.
#' @param use_ssl A logical indicating the use or not of Secure Sockets Layer
#' encryption when connecting to the IMAP server. Default is \code{TRUE}.
#' @param verbose If \code{FALSE}, mutes the flow of information between the
#' server and the client. Default is \code{FALSE}.
#' @param buffersize The size in bytes for the receive buffer. Default is
#'   16000 bytes or 16kb, which means it will use the libcurl's default value.
#'   According to the libcurl's documentation, the maximum buffersize is 512kb
#'   (or 512000 bytes), but any number passed to \code{buffersize} is treated
#'   as a request, not an order.
#' @param timeout_ms Time in milliseconds (ms) to wait for the execution or
#' re-execution of a command. Default is 5000ms (or 5 seconds). If a first
#' execution is frustrated, an error handler in each function (depending on
#' the \code{retries} value), will try to reconnect or re-execute the command.
#' @param ... Further curl parameters (see \code{curl::curl_options}) that
#' can be used with the IMAP protocol. Only for advanced users.
#' @return A new `ImapCon` object.
#' @export
configure_imap <- function(url,
                           username,
                           password = NULL,
                           xoauth2_bearer = NULL,
                           use_ssl = TRUE,
                           verbose = FALSE,
                           buffersize = 16000,
                           # fresh_connect = FALSE,
                           timeout_ms = 5000,
                           ...) {

  con <- ImapCon$new(url,
                     username,
                     password = password,
                     xoauth2_bearer = xoauth2_bearer,
                     use_ssl = use_ssl,
                     verbose = verbose,
                     buffersize = buffersize,
                     # fresh_connect = fresh_connect,
                     timeout_ms = timeout_ms,
                     ...) #ok!

  return(con)

}
