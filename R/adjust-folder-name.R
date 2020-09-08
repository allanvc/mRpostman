#' Mail folder name checking and adjustment
#' @param folder An character \code{vector} containing the folder name informed
#'   by the user inside ImapCon$select_folder().
#' @noRd
adjust_folder_name <- function(folder) {

  # forcing folder to imap server accepted format
  folder <- gsub(" ", "%20", folder)

  folder_check <- grepl(pattern='^\\".*\\"$', x = folder)

  # we want to know if we have already added quotes

  if (!isTRUE(folder_check)) {
    folder <- paste0('"', folder, '"')
  }

  return(folder)

}

