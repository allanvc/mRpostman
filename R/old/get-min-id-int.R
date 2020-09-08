#' @title Get Minimum Message ID in a Mailbox
#'
#' @description Get the smaller (minimum) message id in a Mailbox, considering
#'     messages with a specific flag.
#'
#' @inheritParams check_args_get_max_min_id
#'
#' @inherit get_max_id return
#'
#' @note \code{\link{get_min_id}} uses \code{ESEARCH} extension. Check if your server
#'     supports \code{ESEARCH} capability with \code{\link{list_server_capabilities}}.
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
#' select_folder(folder = "[Gmail]/Trash") %>%
#' result <- get_min_id(flag = "SEEN")
#'
#' }
#'
#' @keywords internal
#'
get_min_id_int <- function(self, flag, use_uid, retries) {

  # if (!is.character(flag)) {
  #   stop('"flag" argument must of type character.')
  # }
  assertthat::assert_that(
    is.character(flag),
    msg='"flag" argument must of type character.')

  check_argg(use_uid = use_uid, retries = retries)

  flag_string <- paste(flag, collapse = " ") #v0.9.0 (for more than one flag passed)
  flag_string = paste0(flag_string, "") # different here because flag is the main parameter of search

  retries <- as.integer(retries)

  url <- self$url

  # isolating the handle
  h <- self$con_handle

  # setting customrequest
  if (isTRUE(use_uid)) {

    customrequest <- paste0("UID SEARCH RETURN (MIN) ", flag_string)


  } else {

    customrequest <- paste0("SEARCH RETURN (MIN) ", flag_string)

  }

  response <- execute_accessorial_operations(self, url, handle = h, customrequest,
                                             use_uid, retries)


  response <- as.numeric(as.character(
    stringr::str_match_all(
      string = rawToChar(response$content),
      pattern = "MIN ([\\d]+)\r\n")[[1]][,2]))

    names(response) <- c("MIN")

    # handle sanitizing
    rm(h)

    return(response)

}
