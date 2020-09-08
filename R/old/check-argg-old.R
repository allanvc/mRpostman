#' @title Internal function for checking function arguments
#'
#' @param ... Any argument passed to any function of the package.
#'
#' @family check argg
#'
#' @keywords internal
#'
check_argg <- function(...) {

  # v0.0.9
  # check all arguments of all functions, but the config_con_handle_and_params() ones,
  # and.. for deep internal functions

  # argg_list <- c(as.list(environment()), list(...))
  argg_list <- list(...)

  # print(argg_list)

  # mailbox operations
  if ("name" %in% names(argg_list)) {
    assertthat::assert_that(
      is.character(argg_list$name),
      msg='"name" must be of type character.')
  }

  if ("silent" %in% names(argg_list)) {
    assertthat::assert_that(
      is.logical(argg_list$silent),
      msg='"silent" must be a logical.')}

  if ("retries" %in% names(argg_list)) {
    assertthat::assert_that(
      is.numeric(argg_list$retries),
      assertthat::validate_that(argg_list$retries >= 0),
      msg='"retries" must be an integer equal or greater than 0.')

    if (argg_list$retries%%1 != 0) {
      warning('only the integer part of "retries" will be used.')
    }
  }

  if ("new_name" %in% names(argg_list)) {
    assertthat::assert_that(
      is.character(argg_list$new_name),
      msg='"new_name" must be of type character.')
  }

  if ("reselect" %in% names(argg_list)) {
    assertthat::assert_that(
      is.logical(argg_list$reselect),
      msg='"reselect" must be a logical.')
  }

  # SEARCH

  if ("negate" %in% names(argg_list)) {
    assertthat::assert_that(
      is.logical(argg_list$negate),
      msg='"negate" must be a logical.')
  }

  if ("use_uid" %in% names(argg_list)) {
    assertthat::assert_that(
      is.logical(argg_list$use_uid),
      msg='"use_uid" must be a logical.')
  }

  if ("esearch" %in% names(argg_list)) {
    assertthat::assert_that(
      is.logical(argg_list$esearch),
      msg='"esearch" must be a logical.')
  }

  # if ("string" %in% names(argg_list)) { # string_in_header
  #   assertthat::assert_that(
  #     is.character(argg_list$string),
  #     msg='"string" argument must be of type character.')
  # }
  #
  # if ("field" %in% names(argg_list)) { # string_in_header
  #   assertthat::assert_that(
  #     is.character(argg_list$field),
  #     msg='"field" argument must be of type character. See field_options().')
  # }

  if ("size" %in% names(argg_list)) {
    assertthat::assert_that(
      is.numeric(argg_list$size),
      msg='"size" must be of type numeric.')
    # can it be double? e.g. 3600.5
  }

  if ("flag" %in% names(argg_list)) {
    assertthat::assert_that(
      any(
        is.null(argg_list$flag),
        is.character(argg_list$flag)
      ),
      msg='"flag" argument must be NULL or of type character.')
  }

  if ("date_char" %in% names(argg_list)) {
    assertthat::assert_that(
      grepl(pattern = "[\\d]{2}-[a-zA-Z]{3}-[\\d]{4}", argg_list$date_char, perl = TRUE),
      # stringr::str_detect(string = date_char,
      #                     pattern = "[\\d]{2}-[a-zA-Z]{3}-[\\d]{4}"),
      msg='"date_char" must be of type character with format DD-Mon-YYYY", e.g. "02-Jan-2020".')
  }

  if ("since_date_char" %in% names(argg_list)) {
    assertthat::assert_that(
      grepl(pattern = "[\\d]{2}-[a-zA-Z]{3}-[\\d]{4}", argg_list$since_date_char, perl = TRUE),
      # stringr::str_detect(string = date_char,
      #                     pattern = "[\\d]{2}-[a-zA-Z]{3}-[\\d]{4}"),
      msg='"since_date_char" must be of type character with format DD-Mon-YYYY", e.g. "02-Jan-2020".')
  }

  if ("before_date_char" %in% names(argg_list)) {
    assertthat::assert_that(
      grepl(pattern = "[\\d]{2}-[a-zA-Z]{3}-[\\d]{4}", argg_list$before_date_char, perl = TRUE),
      # stringr::str_detect(string = date_char,
      #                     pattern = "[\\d]{2}-[a-zA-Z]{3}-[\\d]{4}"),
      msg='"before_date_char" must be of type character with format DD-Mon-YYYY", e.g. "02-Jan-2020".')
  }

  if ("seconds" %in% names(argg_list)) {
    assertthat::assert_that(
      is.numeric(argg_list$seconds),
      msg='"seconds" must be of type numeric.')
    # can it be double? e.g. 3600.5
  }

  # search_string
  if ("expr" %in% names(argg_list)) {
    assertthat::assert_that(
      is.character(argg_list$expr),
      msg='"expr" must be of type character.')
  }

  if ("in_headerfield" %in% names(argg_list)) { # if there is a "in_headerfield", there is a "in_section", because they are NULL for starter
    if ( (is.null(argg_list$in_headerfield) && is.null(argg_list$in_section)) ||
        (!is.null(argg_list$in_headerfield) && !is.null(argg_list$in_section)) ) {
      stop('Chosse one argument: "in_headerfield" or "in_section".')
    } else if (!is.null(argg_list$in_headerfield)) {
      assertthat::assert_that(
        is.character(argg_list$in_headerfield),
        msg='"in_headerfield" argument must be of type character. See headerfield_options().')
    } else {
      assertthat::assert_that(
        is.character(argg_list$in_section),
        msg='"in_section" argument must be of type character.')
    }
  }

  # ----------------- FETCH ARGG CHECKING

  # fetch_body
  if ("msg" %in% names(argg_list)) {
    assertthat::assert_that(
      is.numeric(argg_list$msg),
      msg='"msg" must be a numeric vector of length equal or greater than 1.')

    assertthat::assert_that(
      !any(is.na(argg_list$msg)),
      msg='"msg" cannot have NAs.')

    assertthat::assert_that(
      !any(argg_list$msg == 0),
      msg='"msg" cannot have id 0.')
  }

  if ("peek" %in% names(argg_list)) {
    assertthat::assert_that(
      is.logical(argg_list$peek),
      msg='"peek" must be a logical.')
  }

  if ("write" %in% names(argg_list)) {
    assertthat::assert_that(
      is.logical(argg_list$write),
      msg='"write" must be a logical.')
  }

  # if ("keep" %in% names(argg_list)) { # changed to silent
  #   assertthat::assert_that(
  #     is.logical(argg_list$keep),
  #     msg='"keep" must be a logical.')
  #
  #   assertthat::assert_that(
  #     isTRUE(argg_list$write),
  #     msg='"keep" can only be set as FALSE when "write" = TRUE.')
  #
  #   if (isFALSE(keep)) {
  #     warning('Setting "keep = FALSE" will not alow you to use get_attachments()')
  #   }
  # }

  if ("partial" %in% names(argg_list)) {

    # print(argg_list$partial)
    assertthat::assert_that(
      any(
        is.character(argg_list$partial),
        is.null(argg_list$partial)
      ), msg='"partial" must be NULL or a character with format "startchar.endchar", e.g. "0.255".')


    if (is.character(argg_list$partial))
      assertthat::assert_that(
        stringr::str_detect(string = argg_list$partial,
                          pattern = '[0-9]+\\.[0-9]+'),
        msg='"partial" must be NULL or a character with format "startchar.stopchar", e.g. "0.255".')

  }

  # fetch_header
  if ("fields" %in% names(argg_list)) {
    assertthat::assert_that(
      any(
        is.null(argg_list$fields),
        is.character(argg_list$fields)
      ),
      msg='"fields" must be NULL or a character vector. See fields_options().')

  }


  if ("negate_fields" %in% names(argg_list)) {
    assertthat::assert_that(
      is.logical(argg_list$negate_fields),
      msg='"negate_fields" must be a logical.')

    if (is.null(argg_list$fields)) {
      assertthat::assert_that(
        isFALSE(argg_list$negate_fields),
        msg='"negate_fields" can only be set as TRUE when "fields" is not NULL.')
    }
  }

  # fetch_metadata
  if ("metadata" %in% names(argg_list)) {
    assertthat::assert_that(
      any(
        is.null(argg_list$metadata),
        is.character(argg_list$metadata)
      ),
      msg='"metadata" must be NULL or a character vector. See metadata_options().')
  }

  if ("base64_decode" %in% names(argg_list)) {
    assertthat::assert_that(
      is.logical(argg_list$base64_decode),
      msg='"base64_decode" must be a logical.')
  }


  # ACESSORIAL FUNCTIONS

  if ("to_folder" %in% names(argg_list)) {
    assertthat::assert_that(
      is.character(argg_list$to_folder),
      msg='"to_folder" must be of type character. See list_mail_folders().')
  }


  if ("uid" %in% names(argg_list)) {
    assertthat::assert_that(
      any(
        is.null(argg_list$uid),
        is.numeric(argg_list$uid)
      ), msg='"uid" can be NULL or a numeric vector of size equal or greater than 1.')
  }

  # if (!missing(negate)) {}

  # if (!missing(negate)) {}

  # if (!missing(negate)) {}


  invisible(NULL)
}
