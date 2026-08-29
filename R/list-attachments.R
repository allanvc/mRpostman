#' List attachments and content-disposition types
#' @param msg_list A \code{list} containing the messages (body or text) fetched
#'   from the server.
#' @note Please, note that this is an independent function and not an R6 method
#'   that depends on the connection object. Therefore, it should be called alone
#'   without the ImapCon object.
#' @return A \code{list} of \code{data.frames} containing the filenames and their
#'   \code{Content-Disposition} types for each fetched message.
#' @family attachments
#' @examples
#' \dontrun{
#' con$select_folder(name = "INBOX")
#' # do a search followed by a fetch operation, then extract the attachments' list
#' out <- con$search_string(expr = "@k-state.edu", where = "FROM") %>%
#'   con$fetch_body()
#' att_list <- list_attachments(msg_list = out)
#'
#' # or
#' att_list <- con$search_string(expr = "@k-state.edu", where = "FROM") %>%
#'   con$fetch_body() %>%
#'   list_attachments()
#' }
#' @export
#'
list_attachments <- function(msg_list) {
  lifecycle::deprecate_warn("3.0.0", "list_attachments()", "extract_attachments()")

  check_args(msg_list = msg_list)

  attachments_list <- list()

  for (i in seq_along(msg_list)) {

    id <- names(msg_list[i])
    parts <- mime_attachment_parts(split_mime_parts(msg_list[[i]]), "both")

    if (nrow(parts) > 0) {
      forbiden_chars <- "[\\\\/:*?\"<>|]"
      filenames <- parts$filename
      filenames[is.na(filenames)] <- paste0("attachment_", which(is.na(filenames)), ".bin")
      out_df <- data.frame(
        filename = gsub(forbiden_chars, "", filenames),
        content_disposition = ifelse(is.na(parts$disposition), "attachment",
                                     parts$disposition),
        stringsAsFactors = FALSE)
      out <- list(out_df)
    } else {
      out <- NA
    }

    names(out) <- id
    attachments_list <- c(attachments_list, out)

  }

  return(attachments_list)

}
