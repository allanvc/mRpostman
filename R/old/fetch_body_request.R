#' @title Full Fetch Loop
#'
#' @description Internal helper function for loop execution used in fetch
#'     functions.
#'
#' @inheritParams check_args_fetch_full_msg
#' @param handle A curl handle object.
#'
#' @return A \code{list} or/and text files containing the fetch results.
#'
#' @family fetch helpers
#' @family loop
#'
#' @keywords internal
#'
fetch_body_request <- function(self, msg, use_uid, peek, partial, write,
                               retries, handle) {

  # preparation

  # peek
  if (isTRUE(peek)) {
    body_string = " BODY.PEEK[]"
  } else {
    body_string = " BODY[]"
  }

  # partial
  if (!is.null(partial)) {
    partial_string = paste0("<", partial, ">")
  } else {
    partial_string = NULL
  }

  # use_uid
  if (isTRUE(use_uid)) {
    use_uid_string = "UID "
  } else {
    use_uid_string = NULL
  }

  msg_list <- execute_fetch_loop(self, msg, use_uid, write, retries, handle = h)

  return(msg_list)

}
