#' @title Text Fetch Arguments Check
#'
#' @inherit check_args_fetch_full_msg description return
#'
#' @param msg A \code{numeric vector} containing one or more messages ids.
#' @param use_uid Default is \code{FALSE}. In this case, results will be
#'     presented as message's sequence numbers. A message sequence number is a
#'     message's relative position to the oldest message in the mailbox.
#'     It may change after deleting or moving messages. If a message
#'     is deleted, sequence numbers are reordered to fill the gap.
#'     If \code{TRUE}, the command will be performed using the \code{"UID"} or
#'     unique identifier, and results are presented as such. UIDs are always the
#'     same during the life cycle of a message.
#' @param peek If \code{TRUE}, it does not mark messages as "read" after
#'     fetching. Default is \code{TRUE}.
#' @param partial \code{NULL} or a character string with format
#'     "startchar.endchar" indicating the size (in characters) of a message slice
#'     to fetch. Default is \code{NULL}, which fetchs the full specified content.
#' @param write_to_disk If \code{TRUE} writes fetch content of each message
#'     to the disk as a text file in the working directory. Default is \code{FALSE}.
#' @param keep_in_mem If \code{TRUE} keeps a copy of fetch results as an
#'     list in the R session when \code{write_to_disk = TRUE}. Default is
#'     \code{FALSE}. It can only be set \code{TRUE} when
#'     \code{write_to_disk = TRUE}.
#' @param base64_decode If \code{TRUE}, tries to guess and decode the fetched
#'     text from base64 format to \code{character}. Default is \code{FALSE}.
#' @param retries Number of attempts to connect and execute the command. Default
#'     is \code{2}.
#'
#' @family fetch helper
#' @family check args
#'
#' @keywords internal
#'
check_args_fetch_msg_text <- function(msg_id, by, peek, partial,
                                      write_to_disk, keep_in_mem, try_b64decode,
                                      retries){
  # checks
  assertthat::assert_that(
    is.numeric(msg_id),
    msg='"msg_id" must be a numeric vector of length equal or greater than 1.')

  assertthat::assert_that(
    msg_id != 0,
    msg='"msg_id" must not be 0')
  # it will be important when dealing with pipes and receiving info from search___() functions
  #... when no msg was found

  assertthat::assert_that(
    any(
      by == "MSN",
      by == "UID"
    ),
    msg='"by" must be set as "MSN" or "UID".')

  assertthat::assert_that(
    is.logical(peek),
    msg='"peek" must be a logical.')

  assertthat::assert_that(
    is.logical(write_to_disk),
    msg='"write_to_disk" must be a logical.')

  assertthat::assert_that(
    is.logical(keep_in_mem),
    msg='"keep_in_mem" must be a logical.')

  if (isFALSE(keep_in_mem)) {
    assertthat::assert_that(
      isTRUE(write_to_disk),
      msg='"keep_in_mem" can only be set as FALSE when "write_to_disk" = TRUE.')
  }


  # partial = "0.255"
  assertthat::assert_that(
    any(
      is.character(partial),
      is.null(partial)
    ), msg='"partial" must be NULL or a character with format "startchar.endchar", e.g. "0.255".')

  if(!is.null(partial)){
    assertthat::assert_that(
      stringr::str_detect(string = partial,
                          pattern = '[0-9]+\\.[0-9]+'),
      msg='"partial" must be NULL or a character with format "startchar.endchar", e.g. "0.255".'
    )
  }

  assertthat::assert_that(
    is.logical(try_b64decode),
    msg='"try_b64decode" must be a logical.')

  assertthat::assert_that(
    is.numeric(retries),
    assertthat::validate_that(retries >= 1),
    msg='"retries" must be an integer equal or greater than 1.')

  if (retries%%1 != 0) {
    warning('only the integer part of "retries" will be used.')
  }

  return(NULL)
}
