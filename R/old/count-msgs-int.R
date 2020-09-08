#' @title Count the Number of Messages
#'
#' @description Counts the number of messages with a respective flag in a folder.
#'
#' @param flag Optional parameter that adds a flag or flags as a filter to the
#'     counting operation. Use \code{\link{list_flags}} to list the flags in a
#'     selected mail folder.
#' @param use_uid Default is \code{FALSE}. In this case, results will be
#'     presented as message's sequence numbers. A message sequence number is a
#'     message's relative position to the oldest message in the mailbox.
#'     It may change after deleting or moving messages. If a message
#'     is deleted, sequence numbers are reordered to fill the gap.
#'     If \code{TRUE}, the command will be performed using the \code{"UID"} or
#'     unique identifier, and results are presented as such. UIDs are always the
#'     same during the life cycle of a message.
#' @param retries Number of attempts to connect and execute the command. Default
#'     is \code{2}.
#'
#' @return A numeric \code{vector} of length \code{1} containing the number of
#'     messages that meet the specified criteria.
#'
#' @family miscellaneous
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
#' select_folder(folder = "TAM")
#'
#' results <- select_folder(folder = "INBOX") %>%
#'     count_msgs(by = "UID", flag = "SEEN")
#'
#' }
#'
#' @keywords internal
#'
count_msgs_int <- function(self, flag, use_uid = FALSE, retries = 1) {

  # checks
  check_argg(flag = flag, use_uid = use_uid, retries = retries)

  # flag/name (especial)
  # if (!is.null(flag)) {
  flag_string <- paste(flag, collapse = " ") #v0.9.0 (for more than one flag passed)
  flag_string = paste0(flag_string, "") # different here because flag is the main parameter of search
  # } else {
  #   flag_string = NULL
  # }

  # forcing retries as an integer
  retries <- as.integer(retries)

  url <- self$url

  h <- self$con_handle

  # adding the SEARCH id RETURN COUNT customrequest
  if (isTRUE(use_uid)) {

    customrequest <- paste0("UID SEARCH RETURN (COUNT) ", flag_string)

  } else {

    customrequest <- paste0("SEARCH RETURN (COUNT) ", flag_string)

  }

  response <- execute_accessorial_operations(self, url, handle = h, customrequest,
                                             use_uid, retries)


  response <- as.numeric(as.character(
    stringr::str_match_all(
      string = rawToChar(response$content),
      pattern = "COUNT ([\\d]+)\r\n")[[1]][,2]))

  names(response) <- c("COUNT")
  # handle sanitizing
  rm(h)
  return(response)

}
