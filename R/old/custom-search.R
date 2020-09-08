#' @title Custom Search
#'
#' @description Allows a combination of several arguments using helper functions
#'     that serve as relational operators, such as \code{\link{OR}} and \code{\link{AND}}; and
#'     criteria helper functions such as \code{\link{before}}, \code{\link{since}},
#'     \code{\link{on}}, \code{\link{sent_before}}, \code{\link{sent_since}},
#'     \code{\link{sent_on}}, \code{\link{flag}}, \code{\link{string}},
#'     \code{\link{smaller_than}}, \code{\link{larger_than}}, \code{\link{younger_than}},
#'     or \code{\link{younger_than}}, in order to execute a
#'     custom search (with multiple arguments).
#'
#' @inheritParams check_args_custom_search
#'
#' @inherit search_before return
#'
#' @note IMAP queries follows Polish notation, i.e. operators such as \code{OR}
#'     come before arguments, e.g. "OR argument1 argument2". Therefore, the
#'     relational operators functions in this package should be used like the
#'     following examples: \code{OR(before("17-Apr-2015"), string("FROM", "Jim"))}.
#'     Even though there is no "AND" operator in IMAP, this package adds a helper
#'     funcion \code{\link{AND}} to indicate multiples arguments that must be searched
#'     together, e.g. \code{AND(since("01-Jul-2018"), smaller_than(16000))}.
#'
#' @family Custom-search operations
#' @family Date-search operations
#' @family Size-search operations
#' @family Flag-search operations
#' @family String-search operations
#' @family Within-search operations
#'
#' @examples
#'
#' \dontrun{
#'
#' # configure IMAP
#' library(mRpostman)
#' configure_imap(url="imaps://imap.gmail.com",
#'                username="your_gmail_user",
#'                password=rstudioapi::askForPassword()
#'               )
#'
#' select_folder(name = "TAM")
#'
#' # search1
#' result1 <- custom_search(custom_request = AND(string(section_or_field = "TO",
#'                                               string = "allan"),
#'                                               before(date_char = "12-Apr-2009")
#'                                               ))
#' # searches for messages containing the string "allan" in the "TO" field
#' # and that were received before "12-Apr-2009".
#'
#' # search2
#' result2 <- custom_search(custom_request = OR(string(section_or_field = "TEXT",
#'                                              string = "Contract 2872827"),
#'                                              string(section_or_field = "Subject",
#'                                              string = "Buying operation")
#'                                              ))
#' # searches for messages containing the string "Contract 2872827" in the
#' # "TEXT" section or the string "Buying operation" in the "Subject" field
#' # of the HEADER.
#'
#' }
#'
#' @export
#'
custom_search <- function(custom_request, negate = FALSE, by = "MSN",
                          esearch = FALSE, retries = 2) {

  check_args_custom_search(custom_request, negate, by, esearch, retries)

  # forcing retries as an integer
  retries <- as.integer(retries)

  # forcing server url to the format we need -- removing unnecessary final slashe(s)
  #.. and doing URL encoding
  url <- utils::URLencode(gsub("/+$", "", IMAP_conn$imapconf$url))

  # isolating the handle
  h <- IMAP_conn$imapconf$conn_handle

  #define customrequest to this handle
  define_out <- define_searchrequest_custom(custom_request, negate = negate, by = by,
                                   esearch = esearch, handle = h)

  h <- define_out$handle
  customrequest <- define_out$customrequest

  response <- execute_search(url = url, handle = h, customrequest = customrequest,
                             esearch, retries)

  return(response)

}
