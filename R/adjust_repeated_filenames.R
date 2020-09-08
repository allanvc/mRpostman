#' Add sufix to filenames of a message if there is repetition
#' @param filenames A character string with a message content.
#' @noRd
adjust_repeated_filenames = function(filenames){

  # preparing the directory for saving
  duplicated_filenames <- duplicated(filenames)

  adjusted_filenames <- c()

  for (i in seq_along(filenames)) {
    if (duplicated_filenames[i]) {
      # the first one will never be repeated
      # .. so we assign i-1
      # f = paste0(filenames[i], "(", i, ")")
      f = paste0("(", i-1, ")", filenames[i])
      adjusted_filenames <- c(adjusted_filenames, f)
    } else {
      adjusted_filenames <- c(adjusted_filenames, filenames[i])
    }

  }

  return(adjusted_filenames)

}
