#' Sort messages on the server (INTERNAL HELPER)
#' @param by A character vector of sort keys (subset of "ARRIVAL", "CC", "DATE",
#'   "FROM", "SIZE", "SUBJECT", "TO").
#' @param reverse A logical. If \code{TRUE}, each sort key is prefixed with
#'   \code{REVERSE} (descending order).
#' @param criteria A string with the search criteria to restrict the set to be
#'   sorted. Default is \code{"ALL"}.
#' @param use_uid A logical. If \code{TRUE}, issues \code{UID SORT} and returns
#'   UIDs instead of sequence numbers.
#' @param char_set A string with the charset of the search criteria. Default is
#'   \code{"UTF-8"}.
#' @param retries Number of attempts to connect and execute the command.
#' @noRd
sort_int <- function(self, by, reverse, criteria, use_uid, char_set, retries) {

  assertthat::assert_that(
    is.character(by),
    msg='"by" must be a character vector of sort keys.')

  by <- toupper(by)

  valid_keys <- c("ARRIVAL", "CC", "DATE", "FROM", "SIZE", "SUBJECT", "TO")

  assertthat::assert_that(
    all(by %in% valid_keys),
    msg=paste0('"by" must be a subset of: ', paste(valid_keys, collapse = ", "), '.'))

  assertthat::assert_that(
    is.logical(reverse),
    msg='"reverse" must be a logical.')

  assertthat::assert_that(
    is.character(criteria),
    msg='"criteria" must be of type character.')

  assertthat::assert_that(
    is.character(char_set),
    msg='"char_set" must be of type character.')

  check_args(use_uid = use_uid, retries = retries)

  # SORT is an optional extension (RFC 5256) -- fail early with a clear message
  # if the server does not advertise it (e.g. Gmail does not support SORT).
  assert_capability(self, "SORT", command = "sort", rfc = "RFC 5256",
                    retries = retries)

  # forcing retries as an integer
  retries <- as.integer(retries)

  keys <- if (isTRUE(reverse)) paste0("REVERSE ", by) else by
  keys_str <- paste0("(", paste(keys, collapse = " "), ")")

  prefix <- if (isTRUE(use_uid)) "UID SORT " else "SORT "
  customrequest <- paste0(prefix, keys_str, " ", char_set, " ", criteria)

  url <- self$con_params$url

  # isolating the handle
  h <- self$con_handle

  tryCatch({
    curl::handle_setopt(handle = h, customrequest = customrequest)
  }, error = function(e){
    stop("The connection handle is dead. Please, configure a new IMAP connection with configure_imap().")
  })

  response <- execute_ordered_search(self = self, url = url, handle = h,
                                     customrequest = customrequest,
                                     parser = parse_sort, retries = retries)

  return(response)

}
