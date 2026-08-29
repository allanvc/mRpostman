#' Select a mail folder (INTERNAL HELPER).
#' @param name A \code{character} string containing the name of an existing mail folder on the
#'   user's mailbox.
#' @param mute A \code{logical}. Provides a confirmation message if the
#'   command is successfully executed. Default is \code{FALSE}.
#' @param retries Number of attempts to connect and execute the command. Default
#'   is \code{1}.
#' @noRd
select_folder_int <- function(self, name, mute, retries, condstore = FALSE) {

  check_args(name = name, mute = mute, retries = retries) # we have to pass
  #.. the argg as arg = arg, in order to the check_argg capture the names

  # forcing retries as an integer
  retries <- as.integer(retries)

  folder <- adjust_folder_name(name)
  # folder = name

  # self$imapconf$url <- utils::URLencode(gsub("/+$", "", self$url))
  url <- self$con_params$url

  # isolating the handle
  h <- self$con_handle
  # h <- curl::new_handle()
  # do.call(curl::handle_setopt, c(h, con$self)) # da erro aqui. eh melhor passar o handle de self igual ele cria o
  # self$auth

  # CONDSTORE (RFC 7162): "SELECT folder (CONDSTORE)" makes the server report
  # HIGHESTMODSEQ and MODSEQ in this session. The gate runs before imap_exec
  # sets the command, so its CAPABILITY fetch cannot interfere.
  if (isTRUE(condstore)) {
    assert_capability(self, "CONDSTORE", command = "select_folder(condstore = TRUE)",
                      rfc = "RFC 7162", retries = retries)
  }

  response <- imap_exec(self,
                        customrequest = paste0('SELECT ', folder,
                                               if (isTRUE(condstore)) " (CONDSTORE)" else ""),
                        retries = retries)$response

  if (!mute) {
    cat(paste0("\n::mRpostman: ", '"', name, '"', " selected.\n")) # v0.3.2
    # using the folder name without any transformation
  }

  # servers with CONDSTORE report the folder's HIGHESTMODSEQ in the SELECT
  # response; keep it on the connection for fetch_changes()/modseq()
  if (!is.null(response)) {
    self$con_params$highestmodseq <- parse_highestmodseq(
      paste(rawToChar(response$headers), rawToChar(response$content)))
  }

  invisible(name)
  # invisible(0L)

}
