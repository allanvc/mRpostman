#' Attachments check
#' @param msg An object of type \code{character} containing the whole MIME
#'   message.
#' @noRd
has_attachment <- function(msg, call_from) {

  if (call_from == "list_attachments" | call_from == "get_attachments") {
    check <- grepl(pattern = "Content-Disposition: (attachment|inline)",
                    x = msg)
    return(check)

  } else {
    check1 <- grepl(pattern = "Content-Disposition: (attachment|inline)",
                   x = msg)

    check2 <- grepl(pattern = "attachment|inline",
                  x = msg, ignore.case = TRUE)
    return(any(check1, check2))
  }
}
