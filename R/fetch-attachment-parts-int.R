#' Decode a quoted-printable body into raw bytes (INTERNAL HELPER)
#' @noRd
qp_decode_raw <- function(txt) {
  txt <- gsub("=\r?\n", "", txt)                    # soft line breaks
  b <- charToRaw(txt)
  out <- raw(length(b)); n <- 0L; i <- 1L; L <- length(b)
  while (i <= L) {
    if (b[i] == as.raw(0x3d) && i + 2L <= L) {       # "=XX"
      hex <- rawToChar(b[(i + 1L):(i + 2L)])
      v <- suppressWarnings(strtoi(hex, 16L))
      if (!is.na(v)) { n <- n + 1L; out[n] <- as.raw(v); i <- i + 3L; next }
    }
    n <- n + 1L; out[n] <- b[i]; i <- i + 1L
  }
  out[seq_len(n)]
}

#' Decode a fetched MIME part according to its transfer encoding (INTERNAL HELPER)
#' @noRd
decode_part_raw <- function(txt, encoding) {
  encoding <- tolower(if (is.na(encoding)) "" else encoding)
  if (encoding == "base64") {
    base64enc::base64decode(gsub("[^A-Za-z0-9+/=]", "", txt))
  } else if (encoding == "quoted-printable") {
    qp_decode_raw(txt)
  } else {
    charToRaw(txt)
  }
}

#' Fetch attachments by MIME part, guided by BODYSTRUCTURE (INTERNAL HELPER)
#'
#' Retrieves the MIME structure of each message, selects the attachment parts
#' (or the parts given in \code{parts}), fetches each with
#' \code{BODY.PEEK[<part>]}, decodes the transfer encoding, and either writes
#' the payloads to disk (same layout as \code{fetch_attachments()}) or
#' returns them as raw vectors.
#' @noRd
fetch_attachment_parts_int <- function(self, msg_id, use_uid, parts, local_dir,
                                       override, mute, retries,
                                       content_disposition = "both",
                                       as_is = FALSE) {

  check_args(msg_id = msg_id, use_uid = use_uid, override = override,
             mute = mute, retries = retries)
  assertthat::assert_that(
    is.character(content_disposition), length(content_disposition) == 1,
    content_disposition %in% c("both", "attachment", "inline"),
    msg='"content_disposition" must be one of "both", "attachment", or "inline".')
  if (!is.null(parts)) {
    assertthat::assert_that(is.character(parts),
                            msg='"parts" must be NULL or a character vector of section numbers, e.g. c("2", "3.1").')
  }
  if (!is.null(local_dir)) {
    assertthat::assert_that(is.character(local_dir), length(local_dir) == 1,
                            msg='"local_dir" must be NULL or a single directory path.')
  }

  bs <- fetch_bodystructure_int(self, msg_id, use_uid, retries)
  idcol <- names(bs)[1]
  # part selection, mirroring fetch_attachments(): "attachment" and "inline"
  # follow the Content-Disposition declared by the server; "both" takes every
  # part with either disposition, plus non-text parts that carry a filename
  # (attachments some senders declare without a disposition)
  sel <- if (!is.null(parts)) {
    bs$part %in% parts
  } else if (content_disposition == "both") {
    !is.na(bs$part) & (bs$disposition %in% c("attachment", "inline") | bs$is_attachment)
  } else {
    !is.na(bs$part) & bs$disposition %in% content_disposition
  }
  bs <- bs[sel, , drop = FALSE]
  if (nrow(bs) == 0) {
    out <- data.frame(id = integer(0), part = character(0), filename = character(0),
                      type = character(0), size = numeric(0), stringsAsFactors = FALSE)
    names(out)[1] <- idcol
    if (!is.null(local_dir)) out$path <- character(0) else out$content <- list()
    return(out)
  }

  forbidden_chars <- "[\\\\/:*?\"<>|]"
  user_folder <- gsub(forbidden_chars, "", self$con_params$username)
  folder_clean <- gsub(forbidden_chars, "", gsub("%20", "_", self$con_params$folder))
  uid_string <- if (isTRUE(use_uid)) "UID " else ""

  paths <- character(nrow(bs)); sizes <- numeric(nrow(bs)); payloads <- vector("list", nrow(bs))
  for (i in seq_len(nrow(bs))) {
    id <- bs[[idcol]][i]
    fetch_request <- paste0(uid_string, "FETCH # (BODY.PEEK[", bs$part[i], "])")
    msg_list <- execute_fetch_loop(self = self, msg_id = id, fetch_request = fetch_request,
                                   use_uid = use_uid, write_to_disk = FALSE,
                                   keep_in_mem = FALSE, retries = retries,
                                   fetch_type = "part")
    payload <- if (isTRUE(as_is)) charToRaw(msg_list[[1]]) else
      decode_part_raw(msg_list[[1]], bs$encoding[i])
    sizes[i] <- length(payload)
    filename <- bs$filename[i]
    if (is.na(filename) || !nzchar(filename)) {
      filename <- paste0("part_", gsub(".", "_", bs$part[i], fixed = TRUE), ".",
                         if (is.na(bs$subtype[i])) "bin" else bs$subtype[i])
    }
    filename <- gsub(forbidden_chars, "_", filename)
    if (!is.null(local_dir)) {
      id_folder <- if (isTRUE(use_uid)) paste0("UID", id) else id
      complete_path <- paste0(sub("/+$", "", local_dir), "/", user_folder, "/",
                              folder_clean, "/", id_folder)
      dir.create(complete_path, showWarnings = FALSE, recursive = TRUE)
      path <- if (isTRUE(override)) paste0(complete_path, "/", filename) else
        serialize_filename(sufix = filename, complete_path = complete_path)
      writeBin(payload, path)
      paths[i] <- path
    } else {
      payloads[[i]] <- payload
    }
    bs$filename[i] <- filename
  }

  out <- data.frame(id = bs[[idcol]], part = bs$part, filename = bs$filename,
                    type = paste0(bs$type, "/", bs$subtype), size = sizes,
                    stringsAsFactors = FALSE)
  names(out)[1] <- idcol
  if (!is.null(local_dir)) {
    out$path <- paths
    if (!mute) {
      cat(paste0("\n::mRpostman: ", nrow(out), " attachment part(s) saved under ",
                 sub("/+$", "", local_dir), "/", user_folder, "/", folder_clean, ".\n"))
    }
  } else {
    out$content <- payloads
  }
  rownames(out) <- NULL
  out
}
