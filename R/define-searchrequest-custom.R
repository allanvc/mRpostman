#' Define custom search request
#' @param request A string directly specifying what to search or
#'   constructed by a combination of operators helper functions \code{\link{OR}}
#'   and \code{\link{AND}}, and criteria helper functions such as
#'   \code{\link{before}}, \code{\link{since}}, \code{\link{on}},
#'   \code{\link{sent_before}}, \code{\link{sent_since}}, \code{\link{sent_on}},
#'   \code{\link{flag}}, \code{\link{string}}, \code{\link{smaller_than}},
#'   \code{\link{larger_than}}, \code{\link{younger_than}}, or
#'   \code{\link{younger_than}}.
#' @param negate If \code{TRUE}, negates the search and seeks for "NOT SEARCH
#'   CRITERIA". Default is \code{FALSE}.
#' @param use_uid Default is \code{FALSE}. In this case, results will be
#'   presented as message's sequence numbers. A message sequence number is a
#'   message's relative position to the oldest message in the mailbox. It may
#'   change after deleting or moving messages. If a message is deleted, sequence
#'   numbers are reordered to fill the gap. If \code{TRUE}, the command will be
#'   performed using the \code{"UID"} or unique identifier, and results are
#'   presented as such. UIDs are always the same during the life cycle of a message.
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
define_searchrequest_custom <- function(request, negate, use_uid, esearch,
                                        handle) {

  # esearch
  if (isTRUE(esearch)) {
    esearch_string = "RETURN () "
  } else {
    esearch_string = NULL
  }

  # use_uid
  if (isTRUE(use_uid)) {
    use_uid_string = "UID "
  } else {
    use_uid_string = NULL
  }

  # # by
  # if (by == "UID") {
  #   by_string = "UID "
  # } else {
  #   by_string = NULL
  # }

  # negate
  if (isTRUE(negate)) {
    negate_string = "NOT "
  } else {
    negate_string = NULL
  }

  customrequest = paste0(use_uid_string, "SEARCH ", esearch_string, negate_string, "(", request, ")")

  tryCatch({
    curl::handle_setopt(
      handle = handle,
      customrequest = customrequest)
  }, error = function(e){
    stop("The connection handle is dead. Please, configure a new IMAP connection with ImapCon$new().")
  })


  return(c(handle = handle, customrequest = customrequest))

}
