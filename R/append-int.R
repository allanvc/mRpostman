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
#' @param mute A \code{logical}. Provides a confirmation message if the command
#'   is successfully executed. Default is \code{FALSE}.
#' @param retries Number of attempts to connect and execute the command.
#' @noRd
append_int <- function(self, message, folder, mute, retries) {

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
                       utils::URLencode(folder, reserved = TRUE))

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

  if (!mute) {
    if (self$con_params$verbose) {
      Sys.sleep(0.01)  # wait for the end of the client-server conversation
    }
    cat(paste0("\n::mRpostman: message appended to ", '"', folder, '"', ".\n"))
  }

  invisible(TRUE)

}
