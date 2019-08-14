#' @title Delete Messages
#'
#' @description Deletes messages from a mailbox.
#'
#' @inheritParams check_args_deleteMsg
#'
#' @return Depending on the \code{logical_output} parameter, returns a \code{list}
#'     of length \code{2} containing the \code{imapconf} object and previously
#'     informed message ids (parameter \code{msg_id}), or a logical vector of
#'     length \code{1} indicating the success (\code{TRUE}) of the mentioned
#'     operation.
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
#' # deleting based on search results
#' result1 <- imapconf %>%
#'     selectMailbox(mbox = "TAM") %>%
#'     searchBefore(date_char = "10-may-2012", by = "UID") %$% #modified pipe operator - pass two argg
#'     deleteMsg(imapconf = imapconf, msg_id = msg_id)
#'
#'
#' # deleting a specific msg_id without a previous search
#' result2 <- imapconf %>%
#'     selectMailbox(mbox = "INBOX") %>%
#'     deleteMsg(msg_id = 66128)
#'
#' }
#'
#' @export
#'
deleteMsg <- function(imapconf, msg_id, by = "MSN", logical_output = TRUE,
                      retries = 2){
  # flag = "UNSEEN"

  check_args_deleteMsg(imapconf, msg_id, by, logical_output, retries)

  # forcing retries as an integer
  retries <- as.integer(retries)

  # copying imapconf to return the original in the end
  new_imapconf = imapconf

  # config handle
  h <- config_handle(imapconf)

  msg_id_string = paste0(msg_id, collapse = ",")

  # setting as deleted first
  if(by == "UID"){
    curl::handle_setopt(
      handle = h,
      customrequest = paste0("UID STORE ", msg_id_string, " FLAGS (\\Deleted)"))

  } else{
    curl::handle_setopt(
      handle = h,
      customrequest = paste0("STORE ", msg_id_string, " FLAGS (\\Deleted)"))
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
