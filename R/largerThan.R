#' @inherit smallerThan
#'
#' @family search
#' @family custom
#' @family criteria helper functions
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
#' # search
#' result <- imapconf %>%
#'     selectMailbox(mbox = "INBOX") %>%
#'     customSearch(custom_request =
#'                  OR(String(
#'                        section_or_field = "from", string = "allanvcq@@gmail.com"),
#'                     largerThan(
#'                        size = 512000)
#'                    )
#'                  )
#' # searches for messages containing the string "allanvcq@@gmail.com" in the field "FROM"
#'     OR those largerThan 512KB.
#'
#' }
#'
#' @export
#'
largerThan <- function(size, negate = FALSE){

  check_args_size(size, negate)

  # setting part of the search string

  if(!isTRUE(negate)){
    out = paste0('(LARGER ', size, ')')

  } else{
    out = paste0('(NOT (LARGER ', size, '))')

  }

  return(out)

}
