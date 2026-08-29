#' Execution engine for all the complementary commands
#' @param self The R6 connection object.
#' @param url A string containing the url from the \code{IMAP_conn$imapconf} object.
#' @param handle A curl handle object with the custom request already defined.
#' @param customrequest A string containing the custom request to the server that will
#'     be added to the curl handle.
#' @param retries Number of attempts to connect and execute the command. Default
#'     is \code{1}.
#' @noRd
execute_complementary_operations <- function(self, url, handle,  customrequest,
                                             retries) {
  # url/handle are kept for signature compatibility: the shared handle is the
  # connection's own and the execution goes through the single choke point
  imap_exec(self, customrequest, retries = retries,
            needs_folder = TRUE)$response
}
