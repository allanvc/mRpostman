#' List the server's IMAP capabilities (INTERNAL HELPER)
#' @param retries Number of attempts to connect and execute the command.
#'   Default is \code{1}.
#' @noRd
list_server_capabilities_int <- function(self, retries) {

  check_args(retries = retries) # we have to pass
  #.. the argg as arg = arg, in order to the check_argg capture the names

  # copying imapconf (in case we need to establish fresh_connect = TRUE if an error appear)
  # we dont need because we isolate the handle - it will find a new connection eventually

  # forcing retries as an integer
  retries <- as.integer(retries)

  url <- self$con_params$url

  # isolating the handle
  h <- self$con_handle

  response <- imap_exec(self, customrequest = "CAPABILITY",
                        retries = retries)$response

  pattern = '\\* CAPABILITY (.*?)\r\n' # works w/ gsub in every connection

  # server_capabilities <- strsplit(
  #   x = unlist(regmatches(rawToChar(response$headers),
  #                         regexec(pattern,
  #                                 rawToChar(response$headers),
  #                                 perl=TRUE)))[2],
  #   split = " ") # inconsistent

  server_capabilities <- unlist(regmatches(rawToChar(response$headers),
                                           gregexpr(pattern,
                                                    rawToChar(response$headers)
                                           )))

  server_capabilities <- strsplit(gsub('\\* CAPABILITY |\r\n', '',
                                       server_capabilities[length(server_capabilities)]
                                       ),
                                  split = " ")

  # sanitizing

  


  return(unlist(server_capabilities))

}
