#' List flags in a selected mail folder (INTERNAL HELPER)
#' @param retries Number of attempts to connect and execute the command.
#'   Default is \code{1}.
#' @noRd
list_flags_int <- function(self, retries) {

  if (is.na(self$con_params$folder)) {
    stop_no_folder()
  }

  check_args(retries = retries) # we have to pass
  #.. the argg as arg = arg, in order to the check_argg capture the names

  # forcing retries as an integer
  retries <- as.integer(retries)

  folder <- adjust_folder_name(self$con_params$folder)

  url <- self$con_params$url

  # isolating the handle
  h <- self$con_handle

  response <- imap_exec(self, customrequest = paste0("SELECT ", folder),
                        retries = retries)$response

  pattern = "(?<=FLAGS \\().+?(?=\\))" # using look operators
  # gets * FLAGS (...) and [PERMANENTFLAGS (...)]
  flags <- unlist(regmatches(rawToChar(response$headers),
                                gregexpr(pattern,
                                         rawToChar(response$headers),
                                         perl=TRUE)))

  # check if custom flags are allowed: v0.9.0
  custom_flags_check <- any(grepl(pattern = "\\\\\\*", flags))

  flags <- gsub("\\\\\\*", "", flags) # backslashes are symbols of system flags in IMAP
  # we cannot eliminate them
  # R uses \\

  if (length(flags) == 2) {
    all_flags <- flags[[1]]
    permanent_flags <- flags[[2]]
    custom_flags_allowed <- custom_flags_check #v0.9.0

  } else { # for Sun iPlanet Messaging Server 5.2
    all_flags <- ""
    permanent_flags <- flags[[1]]
    custom_flags_allowed <- custom_flags_check #v0.9.0
  }

  


  flags_out <- list()
  flags_out$flags  <- unlist(strsplit(x = all_flags, split = " "))
  flags_out$permanent_flags <- unlist(strsplit(x = permanent_flags, split = " "))
  flags_out$custom_flags_allowed <- custom_flags_allowed
  return(flags_out)
}
