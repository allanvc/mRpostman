#' @description List the special-use mail folders in a mailbox (INTERNAL HELPER)
#' @param retries Number of attempts to connect and execute the command.
#' @noRd
list_special_use_folders_int <- function(self, retries) {

  check_args(retries = retries) # we have to pass
  #.. the argg as arg = arg, in order to the check_argg capture the names

  # SPECIAL-USE is an optional extension (RFC 6154) -- fail early with a clear
  # message if the server does not advertise it.
  assert_capability(self, "SPECIAL-USE", command = "list_special_use_folders",
                    rfc = "RFC 6154", retries = retries)

  # forcing retries as an integer
  retries <- as.integer(retries)

  url <- self$con_params$url

  # isolating the handle
  h <- self$con_handle

  response <- imap_exec(self, customrequest = 'LIST (SPECIAL-USE) "" *',
                        retries = retries)$response
  final_output <- parse_special_use(rawToChar(response$content))

  # sanitizing

  return(final_output)

}
