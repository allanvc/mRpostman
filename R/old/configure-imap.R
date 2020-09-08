#' @title IMAP Settings v0.3.2
#'
#' @description Configure IMAP server settings.
#'
#' @param url String containing the IMAP server address
#' @param username String containing the username.
#' @param password String containing user's password.
#' @param xoauth2_bearer String containing the oauth2 bearer token.
#' @param use_ssl A Logical indicating the use or not of Secure Sockets Layer
#'     encryption when connecting to IMAP server. Default is \code{TRUE}.
#' @param verbose If \code{FALSE}, mutes the output from the server. Default is
#'     \code{FALSE}.
#' @param buffersize The size in bytes for curl's receive buffer. Default is
#'     16000 bytes or 16kb, which means it will use the default value of libcurl.
#'     Maximum buffersize in libcurl documentation is 512kb (or 512000 bytes),
#'     but curl package accepts 2147483647 bytes without returning errors.
#' @param fresh_connect If \code{TRUE}, opens a new curl connection for each
#'     IMAP command to be executed. Default is \code{NULL}.
#' @param timeout_ms Time in miliseconds (ms) to wait until a connection or a
#'     command to be executed (or re-executed). Default is 5000ms (or 5 seconds).
#'     If a first execution is frustated, an error handler in each function
#'     (depending on the \code{retires} value), will try to reconnect or
#'     re-execute the command.
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
#' # Gmail connection configuration example:
#' library(mRpostman)
#' configure_imap(url="imaps://your.imap.server.com",
#'                username="your_username",
#'                password=rstudioapi::askForPassword()
#'               )
#' }
#' @export
#'
configure_imap <- function(con_params) {
  # suggests only most common options

  checks:
  assertthat::assert_that(is.character(url),
                          msg='Argument "url" must be a string, e.g. "imaps://imap.servername.com".')

  assertthat::assert_that(is.character(username),
                          msg='Argument "username" must be a string, e.g. "myusername".')

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
                          msg='Argument "timeout_ms" must be a number indicating the time in milliseconds.')

  conn_params <- c(as.list(environment()), list(...))

  conn_params$url <- utils::URLencode(gsub("/+$", "", url))

  # checking password or xoauth2_bearer
  if (conn_params$password != "" & conn_params$xoauth2_bearer != "") {

    stop('Only one authentication method allowed: supply an oauth2 token to
         "xoauth2_bearer" or a plain string to "password".')

  } else if (conn_params$password == "" & conn_params$xoauth2_bearer == "") {

    stop('Choose at least one authentication method: supply an oauth2 token to
         "xoauth2_bearer" or a plain string to "password".')

  } else if (conn_params$password != ""){

    assertthat::assert_that(is.character(conn_params$password),
                            msg='Argument "password" must be a string, e.g. "my_pass".')

    # remove empty xoauth2_bearer field
    conn_params$xoauth2_bearer <- NULL

  } else {

    assertthat::assert_that(is.character(conn_params$xoauth2_bearer),
                            msg='Argument "xoauth2_bearer" must be a string, e.g. "ya29.a0AXX...".')

    # remove empty password field
    conn_params$password <- NULL

  }

  # inserted config_handle in configure_imap v0.3.2
  # for: hiding the password as an C pointer (curl handle)
  # configure_imap will return a list that contains a curl handle now and may contain other itens such as folder
  # the other functions will add items to this list

  imapconf <- config_conn_handle(conn_params)

  # assign(imapconf, config_conn_handle(conn_params), envir = IMAP_conn)

  class(imapconf) <- c("imapconf")

  # return(invisible(NULL))

}
