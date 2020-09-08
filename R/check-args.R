#' Internal function for checking function arguments
#' @param ... Any argument passed to any function (and method) of the package.
#' @noRd
check_args <- function(...) {

  # v0.0.9
  # check all arguments of all functions, but the config_con_handle_and_params() ones,
  # and.. for deep internal functions

  # argg_list <- c(as.list(environment()), list(...))
  argg_list <- list(...)

  # print(argg_list)

  # mailbox operations
  if ("name" %in% names(argg_list)) {
    # if (!is.character(argg_list$name)) {
    #   stop('"name" must be of type character.')
    # }
    assertthat::assert_that(
      is.character(argg_list$name),
      msg='"name" must be of type character.')
  }

  # if ("silent" %in% names(argg_list)) {
  #   # if (!is.logical(argg_list$silent)) {
  #   #   stop('"silent" must be a logical.')
  #   # }
  #   assertthat::assert_that(
  #     is.logical(argg_list$silent),
  #     msg='"silent" must be a logical.')
  # }

  if ("mute" %in% names(argg_list)) {
    # if (!is.logical(argg_list$mute)) {
    #   stop('"mute" must be a logical.')
    # }
    assertthat::assert_that(
      is.logical(argg_list$mute),
      msg='"mute" must be a logical.')
  }

  if ("retries" %in% names(argg_list)) {
    # if (!(is.numeric(argg_list$retries) && argg_list$retries >= 0)) {
    #   stop('"retries" must be an integer equal or greater than 0.')
    # }
    assertthat::assert_that(
      is.numeric(argg_list$retries),
      isTRUE(argg_list$retries >= 0),
      msg='"retries" must be an integer equal or greater than 0.')

    if (argg_list$retries%%1 != 0) {
      warning('only the integer part of "retries" will be used.')
    }
  }

  if ("new_name" %in% names(argg_list)) {
    # if (!is.character(argg_list$new_name)) {
    #   stop('"new_name" must be of type character.')
    # }
    assertthat::assert_that(
      is.character(argg_list$new_name),
      msg='"new_name" must be of type character.')
  }

  if ("reselect" %in% names(argg_list)) {
    # if (!is.logical(argg_list$reselect)) {
    #   stop('"reselect" must be a logical.')
    # }
    assertthat::assert_that(
      is.logical(argg_list$reselect),
      msg='"reselect" must be a logical.')
  }

  # SEARCH

  if ("negate" %in% names(argg_list)) {
    # if (!is.logical(argg_list$negate)) {
    #   stop('"negate" must be a logical.')
    # }
    assertthat::assert_that(
      is.logical(argg_list$negate),
      msg='"negate" must be a logical.')
  }

  if ("use_uid" %in% names(argg_list)) {
    # if (!is.logical(argg_list$use_uid)) {
    #   stop('"use_uid" must be a logical.')
    # }
    assertthat::assert_that(
      is.logical(argg_list$use_uid),
      msg='"use_uid" must be a logical.')
  }

  if ("esearch" %in% names(argg_list)) {
    # if (!is.logical(argg_list$esearch)) {
    #   stop('"esearch" must be a logical.')
    # }
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
    # if (!is.numeric(argg_list$size)) {
    #   stop('"size" must be of type numeric.')
    # }
    assertthat::assert_that(
      is.numeric(argg_list$size),
      msg='"size" must be of type numeric.')
    # can it be double? e.g. 3600.5
  }

  if ("flag" %in% names(argg_list)) {
    # if (!any(is.null(argg_list$flag), is.character(argg_list$flag))) {
    #   stop('"flag" argument must be NULL or of type character.')
    # }
    assertthat::assert_that(
      any(
        is.null(argg_list$flag),
        is.character(argg_list$flag)
      ),
      msg='"flag" argument must be NULL or of type character.')
  }

  if ("date_char" %in% names(argg_list)) {
    # if (!grepl(pattern = "[\\d]{2}-[a-zA-Z]{3}-[\\d]{4}", argg_list$date_char, perl = TRUE)) {
    #   stop('"date_char" must be of type character with format DD-Mon-YYYY", e.g. "02-Jan-2020".')
    # }
    assertthat::assert_that(
      grepl(pattern = "[\\d]{2}-[a-zA-Z]{3}-[\\d]{4}", argg_list$date_char, perl = TRUE),
      # stringr::str_detect(string = date_char,
      #                     pattern = "[\\d]{2}-[a-zA-Z]{3}-[\\d]{4}"),
      msg='"date_char" must be of type character with format DD-Mon-YYYY", e.g. "02-Jan-2020".')
  }

  if ("since_date_char" %in% names(argg_list)) {
    # if (!grepl(pattern = "[\\d]{2}-[a-zA-Z]{3}-[\\d]{4}", argg_list$since_date_char, perl = TRUE)) {
    #   stop('"since_date_char" must be of type character with format DD-Mon-YYYY", e.g. "02-Jan-2020".')
    # }
    assertthat::assert_that(
      grepl(pattern = "[\\d]{2}-[a-zA-Z]{3}-[\\d]{4}", argg_list$since_date_char, perl = TRUE),
      # stringr::str_detect(string = date_char,
      #                     pattern = "[\\d]{2}-[a-zA-Z]{3}-[\\d]{4}"),
      msg='"since_date_char" must be of type character with format DD-Mon-YYYY", e.g. "02-Jan-2020".')
  }

  if ("before_date_char" %in% names(argg_list)) {
    # if (!grepl(pattern = "[\\d]{2}-[a-zA-Z]{3}-[\\d]{4}", argg_list$before_date_char, perl = TRUE)) {
    #   stop('"before_date_char" must be of type character with format DD-Mon-YYYY", e.g. "02-Jan-2020".')
    # }
    assertthat::assert_that(
      grepl(pattern = "[\\d]{2}-[a-zA-Z]{3}-[\\d]{4}", argg_list$before_date_char, perl = TRUE),
      # stringr::str_detect(string = date_char,
      #                     pattern = "[\\d]{2}-[a-zA-Z]{3}-[\\d]{4}"),
      msg='"before_date_char" must be of type character with format DD-Mon-YYYY", e.g. "02-Jan-2020".')
  }

  if ("seconds" %in% names(argg_list)) {
    # if (!is.numeric(argg_list$seconds)) {
    #   stop('"seconds" must be of type numeric.')
    # }
    assertthat::assert_that(
      is.numeric(argg_list$seconds),
      msg='"seconds" must be of type numeric.')
    # can it be double? e.g. 3600.5
  }

  # search_string
  if ("expr" %in% names(argg_list)) {
    # if (!is.character(argg_list$expr)) {
    #   stop('"expr" must be of type character.')
    # }
    assertthat::assert_that(
      is.character(argg_list$expr),
      msg='"expr" must be of type character.')
  }

  if ("where" %in% names(argg_list)) {
    # if (!is.character(argg_list$expr)) {
    #   stop('"expr" must be of type character.')
    # }
    assertthat::assert_that(
      is.character(argg_list$where),
      msg='"where" must be of type character.')
  }

  # if ("in_headerfield" %in% names(argg_list)) { # if there is a "in_headerfield", there is a "in_section", because they are NULL for starter
  #   if ( (is.null(argg_list$in_headerfield) && is.null(argg_list$in_section)) ||
  #       (!is.null(argg_list$in_headerfield) && !is.null(argg_list$in_section)) ) {
  #     stop('Choose one argument: "in_headerfield" or "in_section".')
  #   } else if (!is.null(argg_list$in_headerfield)) {
  #     # if (!is.character(argg_list$in_headerfield)) {
  #     #   stop('"in_headerfield" argument must be of type character. See headerfield_options().')
  #     # }
  #     assertthat::assert_that(
  #       is.character(argg_list$in_headerfield),
  #       msg='"in_headerfield" argument must be of type character. See headerfield_options().')
  #   } else {
  #     # if (!is.character(argg_list$in_section)) {
  #     #   stop('"in_section" argument must be of type character.')
  #     # }
  #     assertthat::assert_that(
  #       is.character(argg_list$in_section),
  #       msg='"in_section" argument must be of type character.')
  #   }
  # }

  # ----------------- FETCH ARGG CHECKING

  # fetch_body
  if ("msg_id" %in% names(argg_list)) {
    # if (!is.numeric(argg_list$msg)) {
    #   stop('"msg" must be a numeric vector of length equal or greater than 1.')
    # }
    assertthat::assert_that(
      is.numeric(argg_list$msg_id),
      msg='"msg_id" must be a numeric vector of length equal or greater than 1.')

    # if (any(is.na(argg_list$msg))) {
    #   stop('"msg" cannot have NAs as ids.')
    # }
    assertthat::assert_that(
      !any(is.na(argg_list$msg_id)),
      msg='"msg_id" cannot have NAs as ids.')

    # if (any(argg_list$msg == 0)) {
    #   stop('"msg" cannot have id 0.')
    # }
    assertthat::assert_that(
      !any(argg_list$msg_id == 0),
      msg='"msg_id" cannot have id 0.')
  }

  if ("peek" %in% names(argg_list)) {
    # if (!is.logical(argg_list$peek)) {
    #   stop('"peek" must be a logical.')
    # }
    assertthat::assert_that(
      is.logical(argg_list$peek),
      msg='"peek" must be a logical.')
  }

  if ("write_to_disk" %in% names(argg_list)) {
    # if (!is.logical(argg_list$write)) {
    #   stop('"write_to_disk" must be a logical.')
    # }
    assertthat::assert_that(
      is.logical(argg_list$write_to_disk),
      msg='"write_to_disk" must be a logical.')
  }

  if ("keep_in_mem" %in% names(argg_list)) { # changed to silent
    assertthat::assert_that(
      is.logical(argg_list$keep_in_mem),
      msg='"keep_in_mem" must be a logical.')

    if (isFALSE(argg_list$keep_in_mem)) {
      assertthat::assert_that(
        isTRUE(argg_list$write_to_disk),
        msg='"keep_in_mem" can only be set as FALSE when "write_to_disk = TRUE".')
    }


    # if (isFALSE(keep_in_mem)) { # only for the fetch_body e fetch_text
    #   warning('Setting "keep_in_mem = FALSE" will not alow you to use list_attachments() or get_attachments() after fetching')
    # }
  }

  if ("partial" %in% names(argg_list)) {
    # if (!any(is.character(argg_list$partial), is.null(argg_list$partial))) {
    #   stop('"partial" must be NULL or a character with format "startchar.endchar", e.g. "0.255".')
    # }
    # print(argg_list$partial)
    assertthat::assert_that(
      any(
        is.character(argg_list$partial),
        is.null(argg_list$partial)
      ), msg='"partial" must be NULL or a character with format "startchar.endchar", e.g. "0.255".')


    if (is.character(argg_list$partial)) {
      # if (!stringr::str_detect(string = argg_list$partial, pattern = '[0-9]+\\.[0-9]+')) {
      #   stop('"partial" must be NULL or a character with format "startchar.stopchar", e.g. "0.255".')
      # }
      assertthat::assert_that(
        stringr::str_detect(string = argg_list$partial,
                          pattern = '[0-9]+\\.[0-9]+'),
        msg='"partial" must be NULL or a character with format "startchar.stopchar", e.g. "0.255".')
    }

  }

  # fetch_header
  if ("fields" %in% names(argg_list)) {
    # if (!any(is.null(argg_list$fields), is.character(argg_list$fields))) {
    #   stop('"fields" must be NULL or a character vector. See fields_options().')
    # }
    assertthat::assert_that(
      any(
        is.null(argg_list$fields),
        is.character(argg_list$fields)
      ),
      msg='"fields" must be NULL or a character vector. See fields_options().')

  }

  #fetch header
  if ("negate_fields" %in% names(argg_list)) {
    # if (!is.logical(argg_list$negate_fields)) {
    #   stop('"negate_fields" must be a logical.')
    # }
    assertthat::assert_that(
      is.logical(argg_list$negate_fields),
      msg='"negate_fields" must be a logical.')

    if (is.null(argg_list$fields)) {
      # if (isTRUE(argg_list$negate_fields)) {
      #   stop('"negate_fields" can only be set as TRUE when "fields" is not NULL.')
      # }
      assertthat::assert_that(
        isFALSE(argg_list$negate_fields),
        msg='"negate_fields" can only be set as TRUE when "fields" is not NULL.')
    }
  }

  # fetch_metadata
  if ("metadata" %in% names(argg_list)) {
    # if (!any(is.null(argg_list$metadata), is.character(argg_list$metadata))) {
    #   stop('"metadata" must be NULL or a character vector. See metadata_options().')
    # }
    assertthat::assert_that(
      any(
        is.null(argg_list$metadata),
        is.character(argg_list$metadata)
      ),
      msg='"metadata" must be NULL or a character vector. See metadata_options().')
  }

  if ("base64_decode" %in% names(argg_list)) {
    # if (!is.logical(argg_list$base64_decode)) {
    #   stop('"base64_decode" must be a logical.')
    # }
    assertthat::assert_that(
      is.logical(argg_list$base64_decode),
      msg='"base64_decode" must be a logical.')
  }


  # ACESSORIAL FUNCTIONS

  if ("to_folder" %in% names(argg_list)) {
    # if (!is.character(argg_list$to_folder)) {
    #   stop('"to_folder" must be of type character. See list_mail_folders().')
    # }
    assertthat::assert_that(
      is.character(argg_list$to_folder),
      msg='"to_folder" must be of type character. See list_mail_folders().')
  }


  if ("msg_uid" %in% names(argg_list)) {
    # if (!any(is.null(argg_list$uid), is.numeric(argg_list$uid))) {
    #   stop('"uid" can only be NULL or a numeric vector of size equal or greater than 1.')
    # }
    assertthat::assert_that(
      any(
        is.null(argg_list$msg_uid),
        is.numeric(argg_list$msg_uid)
      ), msg='"msg_uid" can be NULL or a numeric vector of size equal or greater than 1.')
  }

  if ("flags_to_set" %in% names(argg_list)) {
    # if (!is.character(argg_list$flags_to_set)) {
    #   stop('"flags_to_set" argument must be a character.')
    # }
    assertthat::assert_that(
      is.character(argg_list$flags_to_set),
      msg='"flags_to_set" argument must be a character.')
  }

  if ("flags_to_unset" %in% names(argg_list)) {
    # if (!is.character(argg_list$flags_to_unset)) {
    #   stop('"flags_to_unset" argument must be a character.')
    # }
    assertthat::assert_that(
      is.character(argg_list$flags_to_unset),
      msg='"flags_to_unset" argument must be a character.')
  }

  # ATTACHMENTS OPERATIONS

  if ("msg_list" %in% names(argg_list)) {

    # if (!is.list(argg_list$msg_list)) {
    #   stop('"msg_list" must be a list returned by fetch_body() ou fetch_text().')
    # }

    assertthat::assert_that(
      is.list(argg_list$msg_list),
      msg='"msg_list" must be a list returned by fetch_full_msg().')

    # if (length(argg_list$msg_list) == 0) {
    #   stop('"msg_list" has length 0. There is no message in the list.')
    # }
    assertthat::assert_that(
      length(argg_list$msg_list) != 0,
      msg='"msg_list" has length 0. There is no message in the list.')

  }

  if ("content_disposition" %in% names(argg_list)) {

    # if (!any(argg_list$content_disposition == "both",
    #          argg_list$content_disposition == "attachment",
    #          argg_list$content_disposition == "inline")) {
    #   stop('"content_disposition" must be set as "both", "attachment" or "inline".')
    # }

    assertthat::assert_that(
      any(
        argg_list$content_disposition == "both",
        argg_list$content_disposition == "attachment",
        argg_list$content_disposition == "inline"
      ),
      msg='"content_disposition" must be set as "both", "attachment" or "inline".')
  }

  if ("override" %in% names(argg_list)) {
    assertthat::assert_that(
      is.logical(argg_list$override),
      msg='"override" must be a logical.')
  }

  # if (!missing(negate)) {}

  # if (!missing(negate)) {}

  # if (!missing(negate)) {}


  invisible(NULL)
}
