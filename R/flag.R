#' Criterion constructor function to be combined in a custom search statement
#' @param name A string containing one or more flags to search for. Use
#'   \href{#method-list_flags}{\code{ImapCon$list_flags()}} to list the flags
#'   in a selected mail folder.
#' @param negate If \code{TRUE}, negates the search and seeks for "NOT SEARCH
#'   CRITERIA". Default is \code{FALSE}.
#' @family custom search
#' @examples
#' \dontrun{
#' # select folder & search
#' con$select_folder(name = "INBOX")
#' # search for messages with Flag "UNSEEN" AND NOT Smaller Than  512KB.
#' res <- con$search(request = AND(flag("UNSEEN"),
#'                                 smaller_than(size = 512000, negate = TRUE)))
#' }
#' @export
#'
flag <- function(name, negate = FALSE) {


  check_args(name, negate)

  # setting part of the search string

  # flag/name (especial)
  # if (!is.null(flag)) {
  flag_string <- paste(name, collapse = " ") #v0.9.0 (for more than one flag passed)
  flag_string = paste0(flag_string, "") # different here because flag is the main parameter of search
  # } else {
  #   flag_string = NULL
  # }

  if (!isTRUE(negate)) {
    out = paste0('(', flag_string, ')')

  } else {
    out = paste0('(NOT (', flag_string, '))')

  }

  return(out)

}
