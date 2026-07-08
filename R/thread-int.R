#' Thread messages on the server (INTERNAL HELPER)
#' @param algorithm A string with the threading algorithm, either
#'   \code{"REFERENCES"} or \code{"ORDEREDSUBJECT"}.
#' @param criteria A string with the search criteria to restrict the set to be
#'   threaded. Default is \code{"ALL"}.
#' @param use_uid A logical. If \code{TRUE}, issues \code{UID THREAD} and returns
#'   UIDs instead of sequence numbers.
#' @param char_set A string with the charset of the search criteria. Default is
#'   \code{"UTF-8"}.
#' @param retries Number of attempts to connect and execute the command.
#' @noRd
thread_int <- function(self, algorithm, criteria, use_uid, char_set, retries) {

  assertthat::assert_that(
    is.character(algorithm),
    msg='"algorithm" must be of type character.')

  algorithm <- toupper(algorithm)

  valid_algorithms <- c("REFERENCES", "ORDEREDSUBJECT")

  assertthat::assert_that(
    algorithm %in% valid_algorithms,
    msg=paste0('"algorithm" must be one of: ',
               paste(valid_algorithms, collapse = ", "), '.'))

  assertthat::assert_that(
    is.character(criteria),
    msg='"criteria" must be of type character.')

  assertthat::assert_that(
    is.character(char_set),
    msg='"char_set" must be of type character.')

  check_args(use_uid = use_uid, retries = retries)

  # THREAD is an optional extension (RFC 5256) advertised per-algorithm as
  # THREAD=REFERENCES / THREAD=ORDEREDSUBJECT. Fail early with a clear message
  # if the server does not support the requested algorithm (e.g. Gmail supports
  # neither).
  assert_capability(self, paste0("THREAD=", toupper(algorithm)),
                    command = "thread", rfc = "RFC 5256", retries = retries)

  # forcing retries as an integer
  retries <- as.integer(retries)

  prefix <- if (isTRUE(use_uid)) "UID THREAD " else "THREAD "
  customrequest <- paste0(prefix, algorithm, " ", char_set, " ", criteria)

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
                                     parser = parse_thread, retries = retries)

  return(response)

}
