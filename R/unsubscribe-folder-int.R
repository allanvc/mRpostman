#' @description Unsubscribe from a mail folder (INTERNAL HELPER)
#' @param name A string containing the name of the mail folder to unsubscribe
#'   from.
#' @param mute A \code{logical}. Provides a confirmation message if the
#'   command is successfully executed. Default is \code{FALSE}.
#' @param retries Number of attempts to connect and execute the command.
#'   Default is \code{1}.
#' @noRd
unsubscribe_folder_int <- function(self, name, mute, retries) {

  check_args(name = name, mute = mute, retries = retries) # we have to pass
  #.. the argg as arg = arg, in order to the check_argg capture the names

  # forcing retries as an integer
  retries <- as.integer(retries)

  name2 <- adjust_folder_name(name)

  url <- self$con_params$url

  # isolating the handle
  h <- self$con_handle

  response <- imap_exec(self, customrequest = paste0('UNSUBSCRIBE ', name2),
                        retries = retries)$response

  if (!mute) {


    cat(paste0("\n::mRpostman: folder ", '"', name, '"', " unsubscribed.\n"))

  }
  


  invisible(TRUE)

}
