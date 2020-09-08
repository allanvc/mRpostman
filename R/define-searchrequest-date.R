#' Define date search request
#' @inheritParams search_before
#' @param operation The type of date operation, eg. \code{BEFORE}, \code{SINCE},
#'   \code{ON}, and variations.
#' @param date_char A \code{character string} with format "DD-Mon-YYYY", e.g.
#'   "01-Apr-2019". We opted for not to use \code{Date} or \code{POSIX*} like
#'   objects, since IMAP servers use this unusual date format.
#' @param negate If \code{TRUE}, negates the search and seeks for "NOT SEARCH
#'   CRITERIA". Default is \code{FALSE}.
#' @param use_uid Default is \code{FALSE}. In this case, results will be
#'   presented as message's sequence numbers. A message sequence number is a
#'   message's relative position to the oldest message in the mailbox. It may
#'   change after deleting or moving messages. If a message is deleted, sequence
#'   numbers are reordered to fill the gap. If \code{TRUE}, the command will be
#'   performed using the \code{"UID"} or unique identifier, and results are
#'   presented as such. UIDs are always the same during the life cycle of a message.
#' @param flag Optional argument that sets one or more flags as an additional
#'   filter to the search. Use \href{#method-list_flags}{\code{ImapCon$list_flags()}}
#'   to list the flags in a selected mail folder. Default is \code{NULL}.
#' @param esearch A logical. Default is \code{FALSE}. If the IMAP server has
#'   \code{ESEARCH} capability, it can be used to optimize search results. It
#'   will condense the results: instead of writing down the whole sequences of messages'
#'   ids, such as \code{\{1 2 3 4 5\}}, it will be presented as \code{\{1:5\}},
#'   which decreases transmission costs. This argument can be used along with
#'   \code{buffersize} to avoid results stripping. Check if your IMAP server
#'   supports \code{ESEARCH} with
#'   \href{#method-list_server_capabilities}{\code{ImapCon$list_server_capabilities()}}.
#' @param handle A curl handle object.
#' @noRd
define_searchrequest_date <- function(operation, date_char, negate, use_uid, flag,
                                      esearch, handle) {

  # esearch
  if (isTRUE(esearch)) {
    esearch_string = "RETURN () "
  } else {
    esearch_string = NULL
  }

  # flag
  if (!is.null(flag)) {
    flag_string <- paste(flag, collapse = " ") #v0.9.0 (for more than one flag passed)
    flag_string = paste0(flag_string, " ")
  } else {
    flag_string = NULL
  }

  # use_uid
  if (isTRUE(use_uid)) {
    use_uid_string = "UID "
  } else {
    use_uid_string = NULL
  }

  # negate
  if (isTRUE(negate)) {
    negate_string = "NOT "
  } else {
    negate_string = NULL
  }

  customrequest <- paste0(use_uid_string, "SEARCH ", esearch_string, flag_string,
                          negate_string, operation, ' ', date_char)

  tryCatch({
    curl::handle_setopt(
      handle = handle,
      customrequest = customrequest)
  }, error = function(e){
    stop("The connection handle is dead. Please, configure a new IMAP connection with ImapCon$new().")
  })

  return(c(handle = handle, customrequest = customrequest))
}
