#' Examine the number of messages in a mail folder (INTERNAL HELPER)
#' @param name A string containing the name of an existing mail folder on the
#'   user's mailbox. If no name is passed, the command will be executed using the
#'   previously selected mail folder name.
#' @param retries Number of attempts to connect and execute the command.
#'   Default is \code{1}.
#' @noRd
examine_folder_int <- function(self, name, retries) {


  # check
  if (!is.null(name)) { # name can also be NULL here
    # if (!is.character(name)) {
    #   stop('"name" must be of type character or NULL.')
    # }
    assertthat::assert_that(
      is.character(name),
      msg='"name" must be of type character or NULL.')
  } else {
    # previous folder selection checking
    # if (is.na(self$folder)) {
    #   stop('No folder previously selected.')
    # }
    assertthat::assert_that(
      !is.na(self$con_params$folder),
      msg='No folder previously selected.')
  }

  check_args(retries = retries) # we have to pass
  #.. the argg as arg = arg, in order to the check_argg capture the names

  # forcing retries as an integer
  retries <- as.integer(retries)

  if (is.null(name)) {

    # folder <- self$folder
    folder <- adjust_folder_name(self$con_params$folder)

  } else {

    assertthat::assert_that(
      is.character(name),  # not mandatory
      msg='"name" must be of type character or NULL. Use list_mail_folders().')

    folder <- adjust_folder_name(name) # only here
  }

  url <- self$con_params$url

  # isolating the handle
  h <- self$con_handle

  response <- imap_exec(self, customrequest = paste0("EXAMINE ", folder),
                        retries = retries)$response

  exam_out <- parse_examine_counts(rawToChar(response$headers))

  


  # handle sanitizing
  return(exam_out)

}
