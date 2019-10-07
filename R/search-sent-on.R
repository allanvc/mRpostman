#' @inherit search_sent_before
#'
#' @family Date-search operations
#'
#' @examples
#'
#' \dontrun{
#'
#' # configure IMAP
#' library(mRpostman)
#' imapconf <- configure_imap(url="imaps://your.imap.server.com",
#'                            username="your_username",
#'                            password=rstudioapi::askForPassword()
#'                           )
#'
#' # search 1
#' result1 <- imapconf %>%
#'     select_mailbox(mbox = "INBOX") %>%
#'     search_sent_on(date_char = "17-Dec-2003")
#'
#' # search 2 (mailboxes with compound names)
#' result2 <- imapconf %>%
#'     select_mailbox(mbox = "Special Mailbox") %>%
#'     search_sent_on(date_char = "17-Sep-2018", flag = "UNDELETED")
#'
#' }
#'
#' @export
#'
search_sent_on <- function(imapconf, date_char, negate = FALSE, by = "MSN",
                         flag = NULL, esearch = FALSE, return_imapconf = TRUE,
                         retries = 2) {

  check_args_search_date(imapconf, date_char, negate, by, flag, esearch,
                        return_imapconf, retries)

  # forcing retries as an integer
  retries <- as.integer(retries)

  # copying imapconf to return the original in the end
  new_imapconf = imapconf

  # config handle
  h <- config_handle(new_imapconf)

  #define customrequest
  h <- define_searchrequest_date(operation = "SENTON",
                                 date_char = date_char, negate = negate,
                                 by = by, flag = flag, esearch = esearch,
                                 handle = h)

  # searching
  response <- tryCatch({
    curl::curl_fetch_memory(url = new_imapconf$url, handle = h)

  }, error = function(e) {
    return(NULL)

  })

  if (!is.null(response)) {
    pre_response <- stringr::str_match_all(rawToChar(response$content), 'ALL (.*)')[[1]][,2]
    pre_response <- eval(parse(text = paste0("c(", pre_response, ")")))
    # note to self: changed the SEARCH METHOD FOR ESEARCH
    # that optimizes the response, but we need to use eval(parse(.))

    if (length(pre_response) > 0) {
      response <- pre_response
      rm(pre_response)

    } else {
      response = 0

    }

  } else {
    count_retries = 1 #the first try was already counted
    # FORCE appending fresh_connect
    curl::handle_setopt(handle = h, fresh_connect = TRUE)

    select_mailbox(imapconf = new_imapconf, mbox = new_imapconf$mbox)

    while (is.null(response) && count_retries < retries) {
      count_retries = count_retries+1

      response <- tryCatch({
        curl::curl_fetch_memory(url = new_imapconf$url, handle = h)

      }, error = function(e) {
        return(NULL)

      })
    }

    if (!is.null(response)) {
      if (isTRUE(esearch)) {
        pre_response <- stringr::str_match_all(rawToChar(response$content), 'ALL (.*)')[[1]][,2]
        pre_response <- eval(parse(text = paste0("c(", pre_response, ")")))

      } else {
        pre_response <- as.numeric(
          as.character(
            stringr::str_extract_all(rawToChar(response$content), '\\d+')[[1]]
          )
        )

      }
      # note to self: changed the SEARCH METHOD FOR ESEARCH
      # that optimizes the response, but we need to use eval(parse(.))

      if (length(pre_response) > 0) {
        response <- pre_response
        rm(pre_response)

      } else {
        response = 0

      }

    } else {
      stop('An error ocurred while connecting. Please check the following and/or try again:\n
           - your internet connection status;\n
           - if the "flag" is valid, in case you provided one;\n
           - if imapconf options are valid.'

      )
    }

  }
  # handle sanitizing
  rm(h)
  response <- as.integer(as.character(response))

  # fix stripping
  response <- fix_search_stripping(response)

  if (length(response) > 5000) {
    warning('The server has returned > 5000 results and might have stripped some lines.\n
    You can check it by setting "verbose = TRUE" in configureIMAP().\n
    If there were stripped lines, consider increasing "buffersize" (Maximum is 2147483647) and/or setting "esearch = TRUE", if suported.
    ')
  }

  if (isTRUE(return_imapconf)) {
    final_output <- list("imapconf" = imapconf, "msg_id" = response)
    return(final_output)

  } else {

    return(response)

  }

}
