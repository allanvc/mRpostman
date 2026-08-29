#' @description List mail folders in a mailbox (INTERNAL HELPER)
#' @param retries Number of attempts to connect and execute the command.
#' @noRd
list_mail_folders_int <- function(self, retries, detailed = FALSE) {

  check_args(retries = retries) # we have to pass
  #.. the argg as arg = arg, in order to the check_argg capture the names

  assertthat::assert_that(is.logical(detailed), length(detailed) == 1,
                          msg='"detailed" must be a logical.')

  if (isTRUE(detailed)) {
    # LIST-EXTENDED (RFC 5258): one LIST with the folder attributes returned
    assert_capability(self, "LIST-EXTENDED", command = "list_mail_folders(detailed = TRUE)",
                      rfc = "RFC 5258", retries = retries)
    # LIST-MYRIGHTS (RFC 8440): the user's rights on each folder in the same
    # round trip, when the server supports it
    caps <- toupper(get_server_capabilities(self, retries = retries))
    with_rights <- "LIST-MYRIGHTS" %in% caps
    ret <- if (with_rights) "CHILDREN SUBSCRIBED SPECIAL-USE MYRIGHTS" else "CHILDREN SUBSCRIBED SPECIAL-USE"
    resp_char <- execute_simple_command(
      self, paste0('LIST "" "*" RETURN (', ret, ')'), retries)
    return(parse_list_extended(resp_char, my_rights = with_rights))
  }

  # forcing retries as an integer
  retries <- as.integer(retries)

  url <- self$con_params$url

  # isolating the handle
  h <- self$con_handle

  response <- imap_exec(self, customrequest = 'LIST "" *',
                        retries = retries)$response
  # v1.1.7 - parsing extracted to the shared parse_folder_list() helper
  final_output <- parse_folder_list(rawToChar(response$content), command = "LIST")

  # sanitizing

  return(final_output)

}
