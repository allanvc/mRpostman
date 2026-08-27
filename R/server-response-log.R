# Keeps the server lines of the most recent IMAP exchange, so that a command
# the server rejects with "NO" or "BAD" can be reported with the server's own
# reason instead of libcurl's generic "Quote command returned error".

.mR_last_response <- new.env(parent = emptyenv())
.mR_last_response$lines <- character(0)

#' Build the libcurl debug callback of a connection handle (INTERNAL HELPER)
#'
#' The callback records the server response lines (CURLINFO_HEADER_IN) of the
#' current exchange in \code{dbg$lines}, resetting them whenever a new command
#' is sent (CURLINFO_HEADER_OUT), and mirrors libcurl's usual verbose output
#' to stderr when \code{dbg$verbose} is \code{TRUE}. The handle is therefore
#' always created with \code{verbose = TRUE}, and the user-facing
#' \code{verbose} setting only controls the printing.
#' @param dbg An environment with the fields \code{verbose} and \code{lines}.
#' @noRd
make_debug_function <- function(dbg) {
  function(type, msg) {
    # 0 = text, 1 = header in, 2 = header out; 3/4 are body data and 5/6 are
    # raw TLS records, which are binary and of no interest here
    if (!(type %in% c(0L, 1L, 2L))) {
      return(invisible(NULL))
    }
    txt <- rawToChar(msg[msg != as.raw(0)])
    if (type == 2L) {          # header out: a new command is being sent
      dbg$lines <- character(0)
    } else if (type == 1L) {   # header in: server response line(s)
      new <- strsplit(txt, "\r?\n")[[1]]
      dbg$lines <- c(dbg$lines, new[nzchar(new)])
      .mR_last_response$lines <- dbg$lines
    }
    if (isTRUE(dbg$verbose)) {
      prefix <- switch(as.character(type), "0" = "* ", "1" = "< ", "2" = "> ",
                       NULL)
      if (!is.null(prefix)) {
        cat(prefix, txt, sep = "", file = stderr())
      }
    }
    invisible(NULL)
  }
}

#' The tagged NO/BAD line of the most recent exchange, if any (INTERNAL HELPER)
#' @return A string such as \code{"NO [CANNOT] ..."} (tag stripped), or
#'   \code{NA_character_}.
#' @noRd
last_server_error <- function() {
  l <- .mR_last_response$lines
  m <- grep("^[A-Za-z]+[0-9]+ (NO|BAD)( |$)", l, value = TRUE)
  if (length(m) == 0) {
    return(NA_character_)
  }
  sub("^[A-Za-z]+[0-9]+ ", "", m[length(m)])
}
