#' Search by origination date (RFC-2822 Header - SENT BEFORE)
#' @param date_char A \code{character string} with format "DD-Mon-YYYY", e.g.
#'   "01-Apr-2019". We opt not to use \code{Date} or \code{POSIX*} like
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
#' @param retries Number of attempts to connect and execute the command.
#'   Default is \code{1}.
#' @noRd
search_sent_before_int <- function(self, date_char, negate, use_uid, flag, esearch,
                                   retries) {

  check_args(date_char = date_char, negate = negate, use_uid = use_uid, flag = flag,
             esearch = esearch, retries = retries)

  # forcing retries as an integer
  retries <- as.integer(retries)

  url <- self$con_params$url

  # isolating the handle
  h <- self$con_handle

  #define customrequest
  define_out <- define_searchrequest_date(operation = "SENTBEFORE",
                                 date_char = date_char, negate = negate,
                                 use_uid = use_uid, flag = flag, esearch = esearch,
                                 handle = h)

  h <- define_out$handle
  customrequest <- define_out$customrequest

  response <- execute_search(self = self, url = url, handle = h,
                             customrequest = customrequest, esearch, retries)

  return(response)

}
