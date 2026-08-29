#' Exchange client/server identification (INTERNAL HELPER)
#' @param fields A named character vector with the client id fields to send
#'   (e.g. \code{c(name = "mRpostman", version = "1.2.1")}), or \code{NULL} to
#'   send \code{ID NIL}.
#' @param retries Number of attempts to connect and execute the command.
#' @noRd
id_int <- function(self, fields, retries) {

  if (!is.null(fields)) {
    assertthat::assert_that(
      is.character(fields) && !is.null(names(fields)) && all(nzchar(names(fields))),
      msg='"fields" must be a named character vector (e.g. c(name = "mRpostman")), or NULL.')
  }

  check_args(retries = retries)

  # ID is an optional extension (RFC 2971) -- fail early with a clear message
  # if the server does not advertise it.
  assert_capability(self, "ID", command = "id", rfc = "RFC 2971",
                    retries = retries)

  # forcing retries as an integer
  retries <- as.integer(retries)

  if (is.null(fields)) {
    customrequest <- "ID NIL"
  } else {
    pairs <- paste0('"', names(fields), '" "', unname(fields), '"', collapse = " ")
    customrequest <- paste0("ID (", pairs, ")")
  }

  url <- self$con_params$url

  # isolating the handle
  h <- self$con_handle

  response <- imap_exec(self, customrequest = customrequest,
                        retries = retries)$response
  resp_char <- paste(rawToChar(response$headers),
                     rawToChar(response$content), sep = "\r\n")
  final_output <- parse_id(resp_char)

  # sanitizing

  return(final_output)

}
