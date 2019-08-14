#' @title Replace Flag(s) in Messages
#'
#' @description Replaces all flags in messages by one or more flags.
#'
#' @inheritParams check_args_addReplaceFlags
#'
#' @note \link{addFlags}, \link{removeFlags} and \link{replaceFlags}, accepts
#'     not only flags but also keywords (such as \code{$Phishing} on Gmail's
#'     IMAP server) to add, remove or replace.
#'
#' @note IMAP servers do not allow setting the negative version of a flag when a
#'     message already has the positive version of it. If a message with
#'     \code{MSN 1} already has the "SEEN" flag, it is not allowed to add "UNSEEN"
#'     to that. Instead, you have to first remove the "SEEN" flag
#'     \code{removeFlags(imapconf, msg_id = 1, "SEEN")} and only then do
#'     \code{addFlags(imapconf, msg_id = 1, "SEEN")}. Another option is to
#'     complete override all the flags of a message or a set of messages using
#'     \code{replaceFlags(imapconf, msg_id = 1, "SEEN")}.
#'
#' @inherit addFlags return
#'
#' @family miscellaneous
#'
#' @examples
#'
#' \dontrun{
#'
#' # configure IMAP
#' library(mRpostman)
#' imapconf <- configureIMAP(url="imaps://imap.gmail.com",
#'                           username="your_gmail_user",
#'                           password=rstudioapi::askForPassword()
#'                           )
#'
#' result1 <- imapconf %>%
#'     selectMailbox(mbox = "TAM") %>%
#'     searchBefore(date_char = "10-may-2012", by = "UID") %$% #exposition pipe - two argg
#'     removeFlags(imapconf = imapconf, msg_id = msg_id, flags_to_set = c("SEEN", "DRAFT"))
#'
#' }
#'
#' @export
#'
replaceFlags <- function(imapconf, msg_id, by = "MSN", flags_to_set,
                            logical_output = TRUE, retries = 2){

  check_args_addReplaceFlags(imapconf, msg_id, by, flags_to_set, logical_output,
                       retries)

  # forcing retries as an integer
  retries <- as.integer(retries)

  # copying imapconf to return the original in the end
  new_imapconf = imapconf

  # config handle
  h <- config_handle(imapconf)

  # prepare flag and msg_id strings
  dollar_flags_check <- stringr::str_detect(flags_to_set, "^\\$")
  flags_string <- paste0("\\", flags_to_set[!dollar_flags_check], collapse = " ")
  flags_string <- paste(flags_string, flags_to_set[dollar_flags_check], collapse = " ")

  flags_string <- unique(unlist(strsplit(flags_string, " ")))
  flags_string <- paste0(flags_string, collapse = " ")

  msg_id_string = paste0(msg_id, collapse = ",")

  # adding the SEARCH or UID SEARCH before date customrequest parameter
  if(by == "UID"){
    curl::handle_setopt(
      handle = h,
      customrequest = paste0("UID STORE ", msg_id_string, " FLAGS ", "(", flags_string, ")"))

  } else{
    curl::handle_setopt(
      handle = h,
      customrequest = paste0("STORE ", msg_id_string, " FLAGS ", "(", flags_string, ")"))
  }

  # REQUEST
  response <- tryCatch({
    curl::curl_fetch_memory(new_imapconf$url, handle = h)
  }, error = function(e){
    return(NULL)
  })

  if(is.null(response)){

    # it is not necessary to select again
    count_retries = 1 #the first try was already counted
    # FORCE appending fresh_connect
    curl::handle_setopt(handle = h, fresh_connect = TRUE)

    while(is.null(response) && count_retries < retries){
      count_retries = count_retries+1
      response <- tryCatch({
        curl::curl_fetch_memory(new_imapconf$url, handle = h)

      }, error = function(e){
        return(NULL)
      })
    }

    if(is.null(response)){
      stop('An error ocurred while connecting. Please check the following and/or try again:\n
           - your internet connection status;\n
           - if imapconf options are valid;\n
           - the name of the Mailbox (argument "mbox").'
      )
    }

  }

  # handle sanitizing
  rm(h)

  if(isTRUE(logical_output)){
    final_output <- list("imapconf" = imapconf, "msg_id" = msg_id) # 2nd arg bit different from others
    # will allow users to pipe more operations after adding flags
    return(final_output)

  } else{

    return(TRUE)

  }

}
