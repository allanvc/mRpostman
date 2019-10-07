#' @title Attachments Check
#'
#' @description Internal helper function for attachment checking in messages.
#'
#' @param msg An object of class \code{character} containing the whole MIME
#'     message.
#'
#' @return A \code{logical} containing \code{TRUE} if there is one or more
#'     attachments and \code{FALSE} if there is none.
#'
#' @family attachments helper
#'
#' @keywords internal
#'
has_attachment <- function(msg) {
  check <- grepl(pattern = "Content-Disposition: attachment",
                 x = msg)
  return(check)
}
