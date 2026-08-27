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
  occurrences_names <- imap_utf7_decode(occurrences_names)

  # server-declared hierarchy separator, e.g. "/" (Gmail/Yahoo/AOL) or "|" (Yandex)
  hierarchy_sep <- unlist(regmatches(occurrences_splitted[[1]][1],
                              regexec(paste0(' ', command, ' \\(.*\\) (.*?) '),
                                      occurrences_splitted[[1]][1])
                          ))[2]
  # cleaning
  hierarchy_sep <- gsub('\\"', "", hierarchy_sep)

  # a folder is a child when the separator occurs strictly inside its name;
  # the separator is matched literally ("." on Dovecot, "|" on Yandex, "/" on
  # Gmail), never as a regular expression
  folder_check_children <- if (is.na(hierarchy_sep) || !nzchar(hierarchy_sep)) {
    rep(FALSE, length(occurrences_names))
  } else {
    vapply(occurrences_names, function(nm) {
      nchar(nm) > 2 && grepl(hierarchy_sep, substr(nm, 2, nchar(nm) - 1), fixed = TRUE)
    }, logical(1), USE.NAMES = FALSE)
  }

  # dropping type \Noselect
  folder_check_children <- folder_check_children[!folder_check_noselect]
  occurrences_names <- occurrences_names[!folder_check_noselect]

  # separate which ones are folders and which are folders/children
  final_output <- list(root = NULL, children = NULL)
  final_output$root <- occurrences_names[!folder_check_children]
  final_output$children <- occurrences_names[folder_check_children]

  return(final_output)

}
