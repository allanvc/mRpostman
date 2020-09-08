#' Search the minimum message id in the selected mail folder
#'   (depend on ESEARCH capability) (INTERNAL HELPER)
#' @param flag Mandatory parameter that specifies one or more flags as a
#'   filter to the searching operation. Use \href{#method-list_flags}{\code{ImapCon$list_flags()}}
#'   to list the flags in a selected mail folder.
#' @param use_uid Default is \code{FALSE}. In this case, results will be
#'   presented as message's sequence numbers. A message sequence number is a
#'   message's relative position to the oldest message in the mailbox. It may
#'   change after deleting or moving messages. If a message is deleted,
#'   sequence numbers are reordered to fill the gap. If \code{TRUE}, the
#'   command will be performed using the \code{"UID"} or unique identifier,
#'   and results are presented as such. UIDs are always the same during the
#'   life cycle of a message.
#' @param retries Number of attempts to connect and execute the command.
#'   Default is \code{1}.
#' @noRd
esearch_min_id_int <- function(self, flag, use_uid, retries) {

  # if (!is.character(flag)) {
  #   stop('"flag" argument must of type character.')
  # }
  assertthat::assert_that(
    is.character(flag),
    msg='"flag" argument must of type character.')

  check_args(use_uid = use_uid, retries = retries)

  flag_string <- paste(flag, collapse = " ") #v0.9.0 (for more than one flag passed)
  flag_string = paste0(flag_string, "") # different here because flag is the main parameter of search

  retries <- as.integer(retries)

  url <- self$con_params$url

  # isolating the handle
  h <- self$con_handle

  # setting customrequest
  if (isTRUE(use_uid)) {

    customrequest <- paste0("UID SEARCH RETURN (MIN) ", flag_string)


  } else {

    customrequest <- paste0("SEARCH RETURN (MIN) ", flag_string)

  }

  response <- execute_complementary_operations(self, url, handle = h, customrequest,
                                             retries)


  response <- as.numeric(as.character(
    stringr::str_match_all(
      string = rawToChar(response$content),
      pattern = "MIN ([\\d]+)\r\n")[[1]][,2]))

    names(response) <- c("MIN")

    # handle sanitizing
    rm(h)

    return(response)

}
