#' @title Curl Handle Configuration
#'
#' @description Internal helper function for configuring curl handles used in
#'     almost all mRpostman functions.
#'
#' @param imapconf An object of class \code{imapconf} generated with
#'     \code{configure_imap}.
#'
#' @return A curl handle object.
#'
#' @family config helper
#'
#' @keywords internal
#'
config_handle <- function(imapconf) {

  # config handle
  id_to_drop <- c()
  for (i in 1:length(imapconf)) {
    # names not to consider for handle
    if (is.null(imapconf[[i]]) || names(imapconf)[i] == "url" ||
        names(imapconf)[i] == "mbox" || names(imapconf)[i] == "show_pass") {
      id_to_drop <- append(id_to_drop, i)
    }
  }

  # decoding password if it is the case (if FALSE, it was coded in configureIMAP)
  #.. it is necessary to pass the argument to the handle
  if (!isTRUE(imapconf$show.pass)) {
    imapconf$password <- rawToChar(imapconf$password)
  }

  handle_params <- imapconf[-id_to_drop]

  h <- curl::new_handle()

  do.call(curl::handle_setopt, c(h, handle_params))

  return(h)
}
