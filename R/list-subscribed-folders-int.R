#' @description List the subscribed mail folders in a mailbox (INTERNAL HELPER)
#' @param retries Number of attempts to connect and execute the command.
#' @noRd
list_subscribed_folders_int <- function(self, retries) {

  check_args(retries = retries) # we have to pass
  #.. the argg as arg = arg, in order to the check_argg capture the names

  # forcing retries as an integer
  retries <- as.integer(retries)

  url <- self$con_params$url

  # isolating the handle
  h <- self$con_handle

  response <- imap_exec(self, customrequest = 'LSUB "" *',
                        retries = retries)$response
  # LSUB shares the LIST response layout, only the command keyword differs
  final_output <- parse_folder_list(rawToChar(response$content), command = "LSUB")

  # sanitizing

  return(final_output)

}
