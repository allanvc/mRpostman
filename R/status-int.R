#' Request the status of a mail folder without selecting it (INTERNAL HELPER)
#' @param name A string containing the name of an existing mail folder on the
#'   user's mailbox. If no name is passed, the command will be executed using
#'   the previously selected mail folder name.
#' @param items A character vector with the STATUS data items to request. Must
#'   be a subset of "MESSAGES", "RECENT", "UIDNEXT", "UIDVALIDITY", "UNSEEN".
#' @param retries Number of attempts to connect and execute the command.
#'   Default is \code{1}.
#' @noRd
status_int <- function(self, name, items, retries) {

  # check
  if (!is.null(name)) { # name can also be NULL here
    assertthat::assert_that(
      is.character(name),
      msg='"name" must be of type character or NULL.')
  } else {
    # previous folder selection checking
    assertthat::assert_that(
      !is.na(self$con_params$folder),
      msg='No folder previously selected.')
  }

  # items check
  assertthat::assert_that(
    is.character(items),
    msg='"items" must be a character vector. See the valid STATUS data items.')

  items <- toupper(items)

  valid_items <- c("MESSAGES", "RECENT", "UIDNEXT", "UIDVALIDITY", "UNSEEN",
                   "SIZE", "HIGHESTMODSEQ", "MAILBOXID", "APPENDLIMIT")

  assertthat::assert_that(
    all(items %in% valid_items),
    msg=paste0('"items" must be a subset of: ',
               paste(valid_items, collapse = ", "), '.'))

  check_args(retries = retries)

  # extension items are gated on the capability that defines them
  if ("SIZE" %in% items) {
    assert_capability(self, "STATUS=SIZE", command = 'status(items = "SIZE")',
                      rfc = "RFC 8438", retries = retries)
  }
  if ("HIGHESTMODSEQ" %in% items) {
    assert_capability(self, "CONDSTORE", command = 'status(items = "HIGHESTMODSEQ")',
                      rfc = "RFC 7162", retries = retries)
  }
  if ("MAILBOXID" %in% items) {
    assert_capability(self, "OBJECTID", command = 'status(items = "MAILBOXID")',
                      rfc = "RFC 8474", retries = retries)
  }
  if ("APPENDLIMIT" %in% items) {
    assert_capability(self, "APPENDLIMIT", command = 'status(items = "APPENDLIMIT")',
                      rfc = "RFC 7889", prefix = TRUE, retries = retries)
  } # we have to pass
  #.. the argg as arg = arg, in order to the check_argg capture the names

  # forcing retries as an integer
  retries <- as.integer(retries)

  if (is.null(name)) {

    folder <- adjust_folder_name(self$con_params$folder)

  } else {

    folder <- adjust_folder_name(name) # only here
  }

  items_str <- paste0("(", paste(items, collapse = " "), ")")

  url <- self$con_params$url

  # isolating the handle
  h <- self$con_handle

  response <- imap_exec(self, customrequest = paste0("STATUS ", folder, " ", items_str),
                        retries = retries)$response
  # the untagged "* STATUS" line may arrive via headers or content
  resp_char <- paste(rawToChar(response$headers), rawToChar(response$content))
  status_out <- parse_status_counts(resp_char)

  # handle sanitizing
  return(status_out)

}
