#' Configure a curl handle and other parameters for IMAP connection
#' @param url A character string containing the IMAP server address
#' @param username A character string containing the username.
#' @param password A character string containing user's password.
#' @param xoauth2_bearer A character string containing the oauth2 bearer token.
#' @param use_ssl A logical indicating the use or not of Secure Sockets Layer
#'   encryption when connecting to IMAP server. Default is \code{TRUE}.
#' @param verbose If \code{FALSE}, mutes the flow of information between the
#'   server and the client. Default is \code{FALSE}.
#' @param buffersize The size in bytes for curl's receive buffer. Default is
#'   16000 bytes or 16kb, which means it will use the default value of libcurl.
#'   According to libcurl's documentation, the maximum buffersize is 512kb
#'   (or 512000 bytes), but any number passe to \code{buffersize} is treated
#'   as a request, not an order.
#' @param timeout_ms Time in milliseconds (ms) to wait for the execution or
#'   re-execution of a command. Default is 0, which means that no timeout limit is
#'   set.
#' @param ... Further curl parameters (see \code{curl::curl_options}) that
#'   can be used with the IMAP protocol. Only for advanced users.
#' @noRd
config_con_handle_and_params <- function(url, username, password, xoauth2_bearer,
                                         use_ssl, verbose, buffersize, timeout_ms,
                                         ...) {

  other_argg_list <- list(...)

  default_params <- list()

  # print(other_argg_list)

  assertthat::assert_that(
    !("fresh_connect" %in% names(other_argg_list)),
    msg='Argument "fresh_connect" is not accepted by mRpostman since version 0.9.0.0.')

  assertthat::assert_that(
    is.character(url),
    msg='Argument "url" must be a string, e.g. "imaps://imap.servername.com".')

  url <- utils::URLencode(gsub("/+$", "", url))
  check_url <- grepl("^(imap|imaps)://\\w", url)

  assertthat::assert_that(
    isTRUE(check_url),
    msg='Invalid url! Try the following format: "imaps://imap.servername.com".')

  default_params <- c(default_params, "url" = url)

  # if (!is.character(con_params$username)) {
  #   stop('Argument "username" must be a string, e.g. "myusername@myserver.com".')
  # }
  assertthat::assert_that(
    is.character(username),
    msg='Argument "username" must be a string, e.g. "myusername@myserver.com".')

  default_params <- c(default_params, "username" = username)

  # if (!is.logical(con_params$use_ssl)) {
  #   stop('Argument "use_ssl" must be a logical.')
  # }
  assertthat::assert_that(
    is.logical(use_ssl),
    msg='Argument "use_ssl" must be a logical.')

  default_params <- c(default_params, "use_ssl" = use_ssl)

  # if (!is.logical(con_params$verbose)) {
  #   stop('Argument "verbose" must be a logical.')
  # }
  assertthat::assert_that(
    is.logical(verbose),
    msg='Argument "verbose" must be a logical.')

  default_params <- c(default_params, "verbose" = verbose)

  # if (!all(is.numeric(con_params$buffersize), con_params$buffersize >= 1)) {
  #   stop('Argument "buffersize" must be an integer greater than 1.')
  # }
  assertthat::assert_that(all(
    is.numeric(buffersize),
    buffersize >= 16000
  ),
  msg='Argument "buffersize" must be an integer equal or greater than 16000, the minimum value for the libcurl buffer.')

  default_params <- c(default_params, "buffersize" = buffersize)

  # if (!is.logical(con_params$fresh_connect)) {
  #   stop('Argument "fresh_connect" must be a logical.')
  # }
  # assertthat::assert_that(is.logical(con_params$fresh_connect),
  #                         msg='Argument "fresh_connect" must be a logical.')

  # if (!is.numeric(con_params$timeout_ms)) {
  #   stop('Argument "timeout_ms" must be a number indicating the time in milliseconds.')
  # }
  assertthat::assert_that(
    is.numeric(timeout_ms),
    msg='Argument "timeout_ms" must be a number indicating the time in milliseconds.')

  default_params <- c(default_params, "timeout_ms" = timeout_ms)

  # checking password or xoauth2_bearer
  if (!is.null(password) & !is.null(xoauth2_bearer)) {

    stop('Only one authentication method allowed: supply an oauth2 token to
         "xoauth2_bearer" or a plain string to "password".')

  } else if (is.null(password) & is.null(xoauth2_bearer)) {

    stop('Choose at least one authentication method: supply an oauth2 token to
         "xoauth2_bearer" or a plain string to "password".')

  } else if (!is.null(password)){

    # if (!is.character(con_params$password)) {
    #   stop('Argument "password" must be a string, e.g. "my_pass".')
    # }
    assertthat::assert_that(
      is.character(password),
      msg='Argument "password" must be a string, e.g. "my_pass".')

    default_params <- c(default_params, "password" = password)
    # # remove empty xoauth2_bearer field
    # xoauth2_bearer <- NULL

  } else {

    # if (!is.character(con_params$xoauth2_bearer)) {
    #   stop('Argument "xoauth2_bearer" must be a string, e.g. "ya29.a0AXX...".')
    # }
    assertthat::assert_that(
      is.character(xoauth2_bearer),
      msg='Argument "xoauth2_bearer" must be a string, e.g. "ya29.a0AXX...".')

    default_params <- c(default_params, "xoauth2_bearer" = xoauth2_bearer)
    # # remove empty password field
    # password <- NULL

  }

  default_params <- c(default_params, other_argg_list)

  # print(default_params)

  # config handle
  id_to_drop <- c()
  for (i in 1:length(default_params)) {

    if (is.null(default_params[[i]]) || names(default_params)[i] == "url") {
      id_to_drop <- append(id_to_drop, i)
    }

  }

  # print(default_params)

  handle_params <- default_params[-id_to_drop]

  h <- curl::new_handle()

  do.call(curl::handle_setopt, c(h, handle_params))

  # cleaning default_params to assign to self

  # print(default_params)

  id_to_drop2 <- c()
  for (j in 1:length(default_params)) {

    if (is.null(default_params[[j]]) || names(default_params)[j] == "password" ||
        names(default_params)[j] == "xoauth2_bearer" ) {

      id_to_drop2 <- append(id_to_drop2, j)

    }

  }

  con_params <- default_params[-id_to_drop2]

  # print(default_params)

  # print(con_params)

  # other_params = con_params[id_to_drop]

  # return(c(other_params, "con_handle" = h))

  # return(c("con_handle" = h))

  # print(con_params_clean)

  # assign to self
  # print(con_params)

  return(list("con_params" = con_params, "con_handle" = h))
}
