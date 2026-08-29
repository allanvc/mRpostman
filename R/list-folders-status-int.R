#' List the mail folders together with their status counts (INTERNAL HELPER)
#'
#' Issues \code{LIST "" "*" RETURN (STATUS (<items>))} (LIST-STATUS, RFC
#' 5819), which returns the folder list and the \code{STATUS} data items of
#' every folder in a single round trip. Requires the server
#' \code{LIST-STATUS} capability.
#' @param items A character vector with the STATUS data items to request. Must
#'   be a subset of "MESSAGES", "RECENT", "UIDNEXT", "UIDVALIDITY", "UNSEEN".
#' @param retries Number of attempts to connect and execute the command.
#' @noRd
list_folders_status_int <- function(self, items, retries) {

  assertthat::assert_that(
    is.character(items),
    msg='"items" must be a character vector. See the valid STATUS data items.')

  items <- toupper(items)

  valid_items <- c("MESSAGES", "RECENT", "UIDNEXT", "UIDVALIDITY", "UNSEEN",
                   "SIZE", "HIGHESTMODSEQ")

  assertthat::assert_that(
    all(items %in% valid_items),
    msg=paste0('"items" must be a subset of: ',
               paste(valid_items, collapse = ", "), '.'))

  check_args(retries = retries)

  # extension items are gated on the capability that defines them
  if ("SIZE" %in% items) {
    assert_capability(self, "STATUS=SIZE", command = 'list_folders_status(items = "SIZE")',
                      rfc = "RFC 8438", retries = retries)
  }
  if ("HIGHESTMODSEQ" %in% items) {
    assert_capability(self, "CONDSTORE", command = 'list_folders_status(items = "HIGHESTMODSEQ")',
                      rfc = "RFC 7162", retries = retries)
  }

  assert_capability(self, "LIST-STATUS", command = "list_folders_status",
                    rfc = "RFC 5819", retries = retries)

  # forcing retries as an integer
  retries <- as.integer(retries)

  url <- self$con_params$url

  # isolating the handle
  h <- self$con_handle

  customrequest <- paste0('LIST "" "*" RETURN (STATUS (',
                          paste(items, collapse = " "), '))')

  response <- imap_exec(self, customrequest = customrequest,
                        retries = retries)$response
  # the untagged LIST/STATUS lines may arrive via headers or content
  resp_char <- paste(rawToChar(response$headers), rawToChar(response$content),
                     sep = "\r\n")
  final_output <- parse_list_status(resp_char, items)

  # sanitizing

  return(final_output)

}
