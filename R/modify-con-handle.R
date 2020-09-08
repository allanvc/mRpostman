#' Reset curl handle parameters for an existing IMAP connection
#' @param ... Further curl parameters (see \code{curl::curl_options}) that
#' can be used with the IMAP protocol. Only for advanced users.
#' @noRd
modify_con_handle <- function(self, ...) {

  # only receives curl parameters for the handle

  # checks:
  # if (!is.character(con_params$url)) {
  #   stop('Argument "url" must be a string, e.g. "imaps://imap.servername.com".')
  # }

  argg_list <- list(...)

  # print(argg_list)

  if ("username" %in% names(argg_list)) {

    assertthat::assert_that(
      is.character(argg_list$username),
      msg='Argument "username" must be a string, e.g. "myusername@myserver.com".')
  }

  if ("use_ssl" %in% names(argg_list)) {

    assertthat::assert_that(
      is.logical(argg_list$use_ssl),
      msg='Argument "use_ssl" must be a logical.')
  }

  if ("verbose" %in% names(argg_list)) {

    assertthat::assert_that(
      is.logical(argg_list$verbose),
      msg='Argument "verbose" must be a logical.')
  }

  if ("buffersize" %in% names(argg_list)) {

    assertthat::assert_that(all(
      is.numeric(argg_list$buffersize),
      argg_list$buffersize >= 16000
    ),
    msg='Argument "buffersize" must be an integer equal or greater than 16000, the minimum value for the libcurl buffer.')
  }

  if ("timeout_ms" %in% names(argg_list)) {

    assertthat::assert_that(
      is.numeric(argg_list$timeout_ms),
      msg='Argument "timeout_ms" must be a number indicating the time in milliseconds.')
  }

  if ("password" %in% names(argg_list)) {
    # na reset da password, automaticamente seta xoauth2_bearer <- NULL e vice versa
    assertthat::assert_that(
      is.character(argg_list$password),
      msg='Argument "password" must be a string, e.g. "my_pass".')

    argg_list$xoauth2_bearer <- NULL
  }

  if ("xoauth2_bearer" %in% names(argg_list)) {
    # passar todos osparametros para self$con_params$..., para nao ficarem perdidos no meio do self$
    # fazer um teste para nao atribuir password nem xoauth2_bearer
    # na reset da password, automaticamente seta xoauth2_bearer <- NULL se existir e vice versa e passa para essa função (ver se vai funcionar)
    assertthat::assert_that(
      is.character(argg_list$xoauth2_bearer),
      msg='Argument "xoauth2_bearer" must be a string, e.g. "ya29.a0AXX...".')

    argg_list$password <- NULL
  }

  # config handle
  # id_to_drop <- c()
  # for (i in 1:length(argg_list)) { # as a precaution
  #
  #   if (is.null(argg_list[[i]]) || names(argg_list)[i] == "url" ||
  #       names(argg_list)[i] == "folder" || names(argg_list)[i] == "self") {
  #     id_to_drop <- append(id_to_drop, i)
  #   }
  # }

  # print(argg_list)
  do.call(curl::handle_setopt, c(self$con_handle, argg_list)) # actually, argg_list will contain only one parameter

  invisible(TRUE)
}
