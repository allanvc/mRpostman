#' @description Request the server namespaces (INTERNAL HELPER)
#' @param retries Number of attempts to connect and execute the command.
#' @noRd
namespace_int <- function(self, retries) {

  check_args(retries = retries) # we have to pass
  #.. the argg as arg = arg, in order to the check_argg capture the names

  # NAMESPACE is an optional extension (RFC 2342) -- fail early with a clear
  # message if the server does not advertise it.
  assert_capability(self, "NAMESPACE", command = "namespace",
                    rfc = "RFC 2342", retries = retries)

  # forcing retries as an integer
  retries <- as.integer(retries)

  url <- self$con_params$url

  # isolating the handle
  h <- self$con_handle

  response <- imap_exec(self, customrequest = 'NAMESPACE',
                        retries = retries)$response
  final_output <- parse_namespace(rawToChar(response$content))

  # sanitizing

  return(final_output)

}
