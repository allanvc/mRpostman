#' @title Custom Search
#'
#' @description Allows a combination of several arguments using helper functions
#'     that serve as relational operators, such as \link{OR} and \link{AND}; and
#'     criteria helper functions such as \link{before}, \link{since},
#'     \link{on}, \link{sent_before}, \link{sent_since}, \link{sent_on},
#'     \link{flag}, \link{string}, \link{smaller_than}, \link{larger_than},
#'     \link{younger_than}, or \link{younger_than}, in order to execute a
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
#'     funcion \link{AND} to indicate multiples arguments that must be searched
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
#' imapconf <- configure_imap(url="imaps://your.imap.server.com",
#'                            username="your_username",
#'                            password=rstudioapi::askForPassword()
#'                           )
#' # search1
#' result1 <- imapconf %>%
#'     select_mailbox(mbox = "TAM") %>%
#'     custom_search(custom_request = AND(string(section_or_field = "TO",
#'                                               string = "allan"),
#'                                        before(date_char = "12-Apr-2009")
#'                                       ))
#' # searches for messages containing the string "allan" in the "TO" field
#' # and that were received before "12-Apr-2009".
#'
#' # search2
#' result2 <- imapconf %>%
#'     select_mailbox(mbox = "INBOX") %>%
#'     custom_search(custom_request = OR(string(section_or_field = "TEXT",
#'                                              string = "Contract 2872827"),
#'                                       string(section_or_field = "Subject",
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
custom_search <- function(imapconf, custom_request, negate = FALSE,
                         by = "MSN", esearch = FALSE, return_imapconf = TRUE,
                         retries = 2) {

  check_args_custom_search(imapconf, custom_request, negate, by, esearch,
                          return_imapconf, retries)

  # forcing retries as an integer
  retries <- as.integer(retries)

  # copying imapconf to return the original in the end
  new_imapconf = imapconf

  # config handle
  h <- config_handle(new_imapconf)

  #define customrequest
  h <- define_searchrequest_custom(custom_request, negate = negate,
                                 by = by, esearch = esearch,handle = h)

  # searching
  response <- tryCatch({
    curl::curl_fetch_memory(url = new_imapconf$url, handle = h)

  }, error = function(e) {
    return(NULL)

  })

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
