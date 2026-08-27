#' Append a message to a mail folder (INTERNAL HELPER)
#'
#' Uploads a full RFC 822 message to a mail folder (IMAP \code{APPEND}). Unlike
#' the other operations, \code{APPEND} is performed by an \strong{upload}
#' (\code{CURLOPT_UPLOAD}) to the folder URL rather than a \code{customrequest},
#' following the same read-callback pattern as \code{curl::send_mail()}. The
#' shared connection handle is reused (the credentials are not stored in
#' \code{con_params}, so a fresh handle cannot be built) and restored out of
#' upload mode on exit.
#' @param message A \code{character} string or \code{raw} vector with the full
#'   RFC 822 message (headers + body).
#' @param folder A string with the destination folder. If \code{NULL}, uses the
#'   previously selected folder.
#' @param flags \code{NULL} (default) or a character vector with the flags to
#'   store with the message: any of \code{"Seen"}, \code{"Flagged"},
#'   \code{"Answered"}, \code{"Draft"}, and \code{"Deleted"}. Sent through
#'   libcurl's \code{CURLOPT_UPLOAD_FLAGS} (libcurl >= 8.13); earlier
#'   libcurl versions ignore it and always store the message with
#'   \code{\\Seen}.
#' @param mute A \code{logical}. Provides a confirmation message if the command
#'   is successfully executed. Default is \code{FALSE}.
#' @param retries Number of attempts to connect and execute the command.
#' @return Invisibly, the UID assigned to the appended message when the server
#'   reports it through the \code{APPENDUID} response code (UIDPLUS, RFC
#'   4315), or \code{NA} otherwise.
#' @noRd
append_int <- function(self, message, folder, flags, mute, retries) {

  # flags -> CURLOPT_UPLOAD_FLAGS bitmask (curl.h: ANSWERED 1, DELETED 2,
  # DRAFT 4, FLAGGED 8, SEEN 16)
  flag_bits <- c(answered = 1L, deleted = 2L, draft = 4L, flagged = 8L,
                 seen = 16L)
  if (!is.null(flags)) {
    assertthat::assert_that(
      is.character(flags),
      msg='"flags" must be NULL or a character vector.')
    flags_key <- tolower(gsub("^\\\\", "", flags))
    assertthat::assert_that(
      all(flags_key %in% names(flag_bits)),
      msg='"flags" must be a subset of: Seen, Flagged, Answered, Draft, Deleted.')
    upload_flags <- sum(flag_bits[unique(flags_key)])
  } else {
    upload_flags <- 0L
  }

  assertthat::assert_that(
    any(is.character(message), is.raw(message)),
    msg='"message" must be a character string or a raw vector (a full RFC 822 message).')

  if (!is.null(folder)) {
    assertthat::assert_that(
      is.character(folder),
      msg='"folder" must be of type character or NULL.')
  } else {
    assertthat::assert_that(
      !is.na(self$con_params$folder),
      msg='No folder previously selected. Provide "folder" or select one first.')
    folder <- self$con_params$folder
  }

  check_args(mute = mute, retries = retries)

  # forcing retries as an integer
  retries <- as.integer(retries)

  if (is.character(message)) {
    message <- charToRaw(paste(message, collapse = "\r\n"))
  }

  # the destination folder goes in the URL path (percent-encoded)
  url_append <- paste0(self$con_params$url, "/",
                       utils::URLencode(imap_utf7_encode(folder), reserved = TRUE))

  # isolating the handle
  h <- self$con_handle

  # always restore the handle out of upload mode, even on error
  on.exit(
    tryCatch(curl::handle_setopt(h, upload = FALSE), error = function(e) NULL),
    add = TRUE)

  do_append <- function() {
    con <- rawConnection(message)
    on.exit(close(con))
    # clear any leftover CURLOPT_CUSTOMREQUEST from a prior operation on the
    # shared handle (e.g. a previous STATUS/CREATE). With upload = TRUE, a stale
    # customrequest makes libcurl hang right after the server's APPEND "+ go
    # ahead" continuation, so we reset it to the default before uploading.
    curl::handle_setopt(handle = h, customrequest = NULL)
    # store the message with exactly the requested flags (none by default);
    # libcurl < 8.13 has no such option and hardcodes \Seen in APPEND
    if ("upload_flags" %in% names(curl::curl_options())) {
      curl::handle_setopt(handle = h, upload_flags = as.integer(upload_flags))
    }
    curl::handle_setopt(
      handle = h,
      upload = TRUE,
      infilesize_large = length(message),
      readfunction = function(nbytes, ...) readBin(con, raw(), nbytes)
    )
    tryCatch({
      curl::curl_fetch_memory(url_append, handle = h)
    }, error = function(e){
      # print(e$message)
      response_error_handling(e$message[1])
    })
  }

  response <- tryCatch({
    do_append()
  }, error = function(e){
    stop("The connection handle is dead. Please, configure a new IMAP connection with configure_imap().")
  })

  if (is.null(response)) {
    count_retries = 0 #the first try doesnt count

    while (is.null(response) && count_retries < retries) {
      count_retries = count_retries + 1
      response <- do_append()
    }

    if (is.null(response)) {
      stop('Request error: the server returned an error.')
    }
  }

  # UIDPLUS (RFC 4315): servers that advertise it report the UID assigned to
  # the appended message in the tagged "OK [APPENDUID <uidvalidity> <uid>]"
  resp_char <- paste(rawToChar(response$headers), rawToChar(response$content))
  appenduid <- parse_appenduid(resp_char)
  uid <- if (is.null(appenduid)) NA_integer_ else unname(appenduid[["uid"]])

  if (!mute) {
    if (self$con_params$verbose) {
      Sys.sleep(0.01)  # wait for the end of the client-server conversation
    }
    cat(paste0("\n::mRpostman: message appended to ", '"', folder, '"',
               if (!is.na(uid)) paste0(" (UID ", uid, ")") else "", ".\n"))
  }

  invisible(uid)

}
