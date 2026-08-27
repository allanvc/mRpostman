#' Mail folder name checking and adjustment
#' @param folder A character \code{vector} containing the folder name informed
#'   by the user inside ImapCon$select_folder().
#' @noRd
adjust_folder_name <- function(folder) {

  # IMAP transmits non-ASCII mailbox names in modified UTF-7 (RFC 3501,
  # section 5.1.3); names that already look encoded (pure ASCII with a
  # "&<base64>-" run) are left as given
  if (!grepl("^[\\x01-\\x7f]*$", folder, perl = TRUE) ||
      (grepl("&", folder, fixed = TRUE) && !grepl("&[A-Za-z0-9+,]*-", folder))) {
    folder <- imap_utf7_encode(folder)
  }

  # forcing folder to imap server accepted format
  folder <- gsub(" ", "%20", folder)

  folder_check <- grepl(pattern='^\\".*\\"$', x = folder)

  # we want to know if we have already added quotes

  if (!isTRUE(folder_check)) {
    folder <- paste0('"', folder, '"')
  }

  return(folder)

}

