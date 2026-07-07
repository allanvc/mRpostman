#' Execution engine for the SORT and THREAD commands
#'
#' Mirrors \code{execute_search()} (same request / folder-reselect / retry
#' logic) but delegates response parsing to \code{parser} and, crucially, does
#' \strong{not} apply \code{fix_search_stripping()} — SORT/THREAD responses are
#' ordered/grouped by the server and must not be re-sorted.
#' @param self The R6 connection object.
#' @param url A string containing the connection url.
#' @param handle A curl handle with the custom request already defined.
#' @param customrequest A string with the custom request (used to reset the
#'   handle on retry).
#' @param parser A function applied to \code{rawToChar(response$content)} that
#'   returns the parsed result (e.g. \code{parse_sort} or \code{parse_thread}).
#' @param retries Number of attempts to connect and execute the command.
#' @noRd
execute_ordered_search <- function(self, url, handle, customrequest, parser, retries) {

  # previous folder selection checking
  assertthat::assert_that(
    !is.na(self$con_params$folder),
    msg='No folder previously selected.')

  response <- tryCatch({
    curl::curl_fetch_memory(url, handle = handle)
  }, error = function(e){
    # print(e$message)
    response_error_handling(e$message[1])
  })

  if (is.null(response)) {
    count_retries = 0

    # reselect the folder, mirroring execute_search(): a long idle period can
    # drop the folder selection and yield "BAD ... not allowed now"
    select_folder_int(self, name = self$con_params$folder, mute = TRUE, retries = 0)

    while (is.null(response) && count_retries < retries) {
      count_retries = count_retries + 1

      # reset customrequest in handle
      tryCatch({
        curl::handle_setopt(handle = handle, customrequest = customrequest)
      }, error = function(e){
        stop("The connection handle is dead. Please, configure a new IMAP connection with configure_imap().")
      })

      response <- tryCatch({
        curl::curl_fetch_memory(url, handle = handle)
      }, error = function(e){
        # print(e$message)
        response_error_handling(e$message[1])
      })
    }

    if (is.null(response)) {
      stop('Request error: the server returned an error.')
    }
  }

  final_output <- parser(rawToChar(response$content))

  # handle sanitizing
  rm(handle)
  if (self$con_params$verbose) {
    Sys.sleep(0.01)  # wait for the end of the client-server conversation
  }
  return(final_output)

}
