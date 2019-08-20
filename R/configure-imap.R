#' @title IMAP Settings
#'
#' @description Configure IMAP server settings.
#'
#' @param url String containing the IMAP server address
#' @param username String containing user's name.
#' @param password String containing user's password.
#' @param show_pass How to store user's password in imapconf object. Default is
#'     \code{FALSE}.
#' @param use_ssl A Logical indicating the use or not of Secure Sockets Layer
#'     encryption when connecting to IMAP server. Default is \code{TRUE}.
#' @param verbose If \code{FALSE}, mutes the output from the server. Default is
#'     \code{FALSE}.
#' @param buffersize The size in bytes for curl's receive buffer. Default is
#'     16000 bytes or 16kb, which means it will use the default value of libcurl.
#'     Maximum buffersize in libcurl documentation is 512kb (or 512000 bytes),
#'     but curl package accepts 2147483647 bytes without returning errors.
#' @param fresh_connect If \code{TRUE}, opens a new curl connection for each
#'     IMAP command to be executed. Default is \code{NULL}. Nonetheless, most of
#'     mRpostman functions will set \code{fresh_connect = TRUE} for the retires
#'     in case of an connection error.
#' @param timeout_ms Time in miliseconds (ms) to wait until a connection or a
#'     command to be executed. Default is 5000ms (or 5 seconds). If a first
#'     execution is frustated, an error handler in each function (depending on
#'     \code{retires} value), will try to reconnect or re-execute the command.
#' @param ... Further parameters added by mRpostman functions or IMAP parameters
#'     listed in \code{curl::curl_options}. Only for advanced users.
#'
#' @return An object of class \code{imapconf} containing the settings
#'     needed to connect to the IMAP server.
#'
#' @references \url{https://curl.haxx.se/libcurl/c/CURLOPT_BUFFERSIZE.html}
#'
#' @family config
#'
#' @examples
#' \dontrun{
#'
#' # Gmail config example:
#' library(mRpostman)
#' imapconf <- configure_imap(url="imaps://imap.gmail.com",
#'                            username="your_gmail_user",
#'                            password=rstudioapi::askForPassword()
#'                           )
#' }
#' @export
#'
configure_imap <- function(url, username, password, show_pass = FALSE, use_ssl = TRUE,
                        verbose = FALSE,
                        buffersize = 16000, fresh_connect = FALSE,
                        timeout_ms = 5000, ...) {
  # suggests only most common options

  # checks:
  assertthat::assert_that(is.character(url),
                          msg='Argument "url" must be a string, e.g. "imaps://imap.servername.com".')

  assertthat::assert_that(is.character(username),
                          msg='Argument "username" must be a string, e.g. "myusername".')

  assertthat::assert_that(is.character(password),
                          msg='Argument "password" must be a string, e.g. "mypassword".')

  assertthat::assert_that(is.logical(show_pass),
                          msg='Argument "show_pass" must be a logical.')

  assertthat::assert_that(is.logical(use_ssl),
                          msg='Argument "use_ssl" must be a logical.')

  assertthat::assert_that(is.logical(verbose),
                          msg='Argument "verbose" must be a logical.')

  assertthat::assert_that(all(
    is.numeric(buffersize),
    buffersize >= 1 &&
    buffersize <= 2147483647L
    ),
    msg='Argument "buffersize" must be an integer between 1 and 2147483647.')

  assertthat::assert_that(is.logical(fresh_connect),
                          msg='Argument "fresh_connect" must be a logical.')

  assertthat::assert_that(is.numeric(timeout_ms),
                          msg='Argument "timeout_ms" must be a number indicating the time in milliseonds.')

  # hiding password
  if (!isTRUE(show_pass)) password <- charToRaw(password)

  argg <- c(as.list(environment()), list(...))

  class(argg) <- c("imapconf")

  return(argg)

}
