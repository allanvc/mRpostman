#' @title Curl Handle Configuration for Connection Settings v0.3.2
#'
#' @description Internal helper function for configuring curl handles used in
#'     almost all mRpostman functions.
#'
#' @param imapconf An object of class \code{imapconf} generated with
#'     \code{\link{configure_imap}}.
#'
#' @return A curl handle object.
#'
#' @family config helper
#'
#' @keywords internal
#'
config_conn_handle <- function(conn_params) {

  # config handle
  id_to_drop <- c()
  for (i in 1:length(conn_params)) {
    # names not to consider for handle
    # if (is.null(conn_params[[i]]) || names(conn_params)[i] == "url" ||
    #     names(conn_params)[i] == "mbox" || names(conn_params)[i] == "show_pass") {
    #   id_to_drop <- append(id_to_drop, i)
    # }

    if (is.null(conn_params[[i]]) || names(conn_params)[i] == "url" ||
        names(conn_params)[i] == "folder" || names(conn_params)[i] == "self") {
      id_to_drop <- append(id_to_drop, i)
    }
  }

  # decoding password if it is the case (if FALSE, it was coded in configureIMAP)
  #.. it is necessary to pass the argument to the handle
  # if (!isTRUE(conn_params$show.pass)) {
  #   conn_params$password <- rawToChar(conn_params$password)
  # }

  handle_params <- conn_params[-id_to_drop]

  h <- curl::new_handle()

  do.call(curl::handle_setopt, c(h, handle_params))

  other_params = conn_params[id_to_drop]

  return(c(other_params, "conn_handle" = h))
}
