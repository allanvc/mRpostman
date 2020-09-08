#' Execute a custom search (INTERNAL HELPER)
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
#' @param retries Number of attempts to connect and execute the command.
#'   Default is \code{1}.
#' @noRd
search_int <- function(self, request, negate, use_uid, esearch, retries) {

  check_args(negate = negate, use_uid = use_uid, esearch = esearch, retries = retries)
  # we have to pass
  #.. the argg as arg = arg, in order to the check_argg capture the names

  # forcing retries as an integer
  retries <- as.integer(retries)

  url <- self$con_params$url

  # isolating the handle
  h <- self$con_handle

  #define customrequest to this handle
  define_out <- define_searchrequest_custom(request, negate = negate,
                                            use_uid = use_uid,
                                            esearch = esearch, handle = h)

  h <- define_out$handle
  customrequest <- define_out$customrequest

  response <- execute_search(self = self, url = url, handle = h,
                             customrequest = customrequest, esearch, retries)

  return(response)

}
