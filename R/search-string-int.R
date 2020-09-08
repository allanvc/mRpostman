#' Search by string or expression (INTERNAL HELPER)
#' @param expr A character string specifying the word or expression to search
#'   for in messages.
#' @param where A mandatory character string specifying in which
#'   message's Section or Header Field to search for the provided string. For
#'   some available options, see \code{\link{section_or_field_options}}.
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
#' \code{buffersize} to avoid results stripping. Check if your IMAP server
#'   supports \code{ESEARCH} with
#'   \href{#method-list_server_capabilities}{\code{ImapCon$list_server_capabilities()}}.
#' @param retries Number of attempts to connect and execute the command.
#'   Default is \code{1}.
#' @noRd
search_string_int <- function(self, expr, where = NULL, negate = FALSE,
                              use_uid = FALSE, flag = NULL, esearch = FALSE, retries = 2) {


  # if ( (is.null(in_headerfield) && is.null(in_section)) ||
  #      (!is.null(in_headerfield) && !is.null(in_section)) ) {
  #   stop('Chosse one argument: "in_headerfield" or "in_section".')
  #
  # } else if (!is.null(in_headerfield)) {
  #   assertthat::assert_that(
  #     is.character(in_headerfield),
  #     msg='"in_headerfield" argument must be of type character. See headerfield_options().')
  # } else {
  #   assertthat::assert_that(
  #     is.character(in_section),
  #     msg='"in_section" argument must be of type character.')
  # }

  check_args(expr = expr, where = where, negate = negate, use_uid = use_uid,
             flag = flag, esearch = esearch, retries = retries)

  # forcing retries as an integer
  retries <- as.integer(retries)

  url <- self$con_params$url

  # isolating the handle
  h <- self$con_handle

  #define customrequest
  define_out <- define_searchrequest_string(expr = expr,
                                            where = where,
                                            negate = negate,
                                            use_uid = use_uid, flag = flag,
                                            esearch = esearch,
                                            handle = h)

  h <- define_out$handle
  customrequest <- define_out$customrequest

  response <- execute_search(self = self, url = url, handle = h,
                             customrequest = customrequest, esearch, retries)

  return(response)

}
