#' @title Replace Flag(s) in Messages
#'
#' @description Replaces all flags in messages by one or more flags.
#'
#' @inheritParams check_args_add_replace_flags
#'
#' @note \code{\link{add_flags}}, \code{\link{remove_flags}} and \code{\link{replace_flags}}, accepts
#'     not only flags but also keywords which are custom flags defined by the user.
#'
#' @note IMAP servers do not allow setting a flag when a
#'     message already has its antonym version of it. For example, if a message
#'     with \code{MSN 1} already has the "SEEN" flag, it is not allowed to add "UNSEEN"
#'     to that. Instead, you have to first remove the "SEEN" flag
#'     \code{remove_flags(msg_id = 1, "SEEN")} and only then do
#'     \code{add_flags(msg_id = 1, "SEEN")}. Another option is to
#'     completely override all the flags of a message or a set of messages using
#'     \code{replace_flags(msg_id = 1, "SEEN")}.
#'
#' @inherit add_flags return
#'
#' @family miscellaneous
#'
#' @examples
#'
#' \dontrun{
#'
#' # configure IMAP
#' library(mRpostman)
#' configure_imap(url="imaps://your.imap.server.com",
#'                username="your_username",
#'                password=rstudioapi::askForPassword()
#'               )
#'
#' select_folder(name = "TAM")
#'
#' search_before(date_char = "10-may-2012", by = "UID") %>%
#'     replace_flags(flags_to_set = c("\\SEEN", "\\DRAFT"))
#'
#' }
#'
#' @export
#'
replace_flags <- function(msg_id, by = "MSN", flags_to_set, retries = 2) {

  check_args_add_replace_flags(msg_id, by, flags_to_set, retries)

  # forcing retries as an integer
  retries <- as.integer(retries)

  # forcing server url to the format we need -- removing unnecessary final slashe(s)
  #.. and doing URL encoding
  url <- utils::URLencode(gsub("/+$", "", IMAP_conn$imapconf$url))

  # isolating the handle
  h <- IMAP_conn$imapconf$conn_handle

  # prepare flag and msg_id strings
  flags_string <- paste(flags_to_set, collapse = " ")

  msg_id_string = paste0(msg_id, collapse = ",")


  if (by == "UID") {
    tryCatch({
      curl::handle_setopt(
        handle = h,
        customrequest = paste0("UID STORE ", msg_id_string, " FLAGS ", "(", flags_string, ")"))
    }, error = function(e){
      stop("The connection handle is dead. Please, configure a new IMAP connection with configure_imap().")
    })

  } else {
    tryCatch({
      curl::handle_setopt(
        handle = h,
        customrequest = paste0("STORE ", msg_id_string, " FLAGS ", "(", flags_string, ")"))
    }, error = function(e){
      stop("The connection handle is dead. Please, configure a new IMAP connection with configure_imap().")
    })

  }

  # REQUEST
  response <- tryCatch({
    curl::curl_fetch_memory(url, handle = h)
  }, error = function(e){
    # print(e$message)
    response_error_handling(e$message[1])
  })

  if (is.null(response)) {

    # it is not necessary to select again
    count_retries = 1 #the first try was already counted
    # FORCE appending fresh_connect
    # curl::handle_setopt(handle = h, fresh_connect = TRUE)

    while (is.null(response) && count_retries < retries) {
      count_retries = count_retries + 1
      # REQUEST
      response <- tryCatch({
        curl::curl_fetch_memory(url, handle = h)
      }, error = function(e){
        # print(e$message)
        response_error_handling(e$message[1])
      })
    }

    if (is.null(response)) {
      stop('Request error: the server returned an error.')
    }

  }

  # handle sanitizing
  rm(h)

  # final_output <- list("imapconf" = imapconf, "msg_id" = msg_id) # 2nd arg bit different from others
  # will allow users to pipe more operations after adding flags
  # return(final_output)
  invisible(msg_id)

}
