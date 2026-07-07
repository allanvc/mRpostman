#' Parse a LIST/LSUB response into root and children folder names
#'
#' Shared parser for the untagged \code{* LIST ...} (\code{list_mail_folders})
#' and \code{* LSUB ...} (\code{list_subscribed_folders}) responses. Folder
#' names are extracted, \code{\\Noselect} entries are dropped, and names are
#' split into top-level (\code{root}) and hierarchical (\code{children}) using
#' the server-declared hierarchy separator. Kept command-parameterized because
#' the only structural difference between the two responses is the command
#' keyword.
#' @param content_char A \code{character} string with the server response body
#'   (typically \code{rawToChar(response$content)}).
#' @param command The IMAP command keyword that labels the untagged lines,
#'   either \code{"LIST"} or \code{"LSUB"}. Default is \code{"LIST"}.
#' @return A \code{list} with elements \code{root} and \code{children}.
#' @noRd
parse_folder_list <- function(content_char, command = "LIST") {

  occurrences_splitted <- strsplit(x = content_char,
                                   split = '\r\n\\*|\r\n')

  folder_check_noselect <- do.call(
    grepl, c(pattern = '\\\\Noselect', x = occurrences_splitted)
  )

  # folder names: a closing quote, a space, then the name up to \r\n
  pattern = '\" (.*?)\r\n'
  m <- gregexpr(pattern, content_char)
  occurrences_names <- regmatches(content_char, m)
  occurrences_names <- lapply(occurrences_names, function(x) gsub('\" |\"', "", x))
  occurrences_names <- unlist(lapply(occurrences_names, function(x) gsub('\r\n.*$', "", x)))

  # server-declared hierarchy separator, e.g. "/" (Gmail/Yahoo/AOL) or "|" (Yandex)
  hierarchy_sep <- unlist(regmatches(occurrences_splitted[[1]][1],
                              regexec(paste0(' ', command, ' \\(.*\\) (.*?) '),
                                      occurrences_splitted[[1]][1])
                          ))[2]
  # cleaning
  hierarchy_sep <- gsub('\\"', "", hierarchy_sep)

  if (!is.na(hierarchy_sep) && hierarchy_sep == "|") { # v0.9.1 - Yandex uses root|children
    hierarchy_sep <- paste0('\\', hierarchy_sep)
  }

  pattern_hierarchy_sep = paste0('.', hierarchy_sep, '.')

  folder_check_children <- do.call(
    grepl, c(pattern = pattern_hierarchy_sep, x = list(occurrences_names))
  )

  # dropping type \Noselect
  folder_check_children <- folder_check_children[!folder_check_noselect]
  occurrences_names <- occurrences_names[!folder_check_noselect]

  # separate which ones are folders and which are folders/children
  final_output <- list(root = NULL, children = NULL)
  final_output$root <- occurrences_names[!folder_check_children]
  final_output$children <- occurrences_names[folder_check_children]

  return(final_output)

}
