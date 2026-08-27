#' Fetch and parse the ENVELOPE of messages (INTERNAL HELPER)
#' @noRd
fetch_envelope_int <- function(self, msg_id, use_uid, retries) {
  msg_list <- fetch_metadata_int(self, msg_id, use_uid, attribute = "ENVELOPE",
                                 write_to_disk = FALSE, keep_in_mem = TRUE,
                                 mute = TRUE, retries = retries)
  rows <- lapply(msg_list, parse_envelope)
  out <- do.call(rbind, rows)
  ids <- sub("^metadata(UID)?", "", names(msg_list))
  out <- cbind(data.frame(id = suppressWarnings(as.integer(ids)), stringsAsFactors = FALSE),
               out)
  names(out)[1] <- if (isTRUE(use_uid)) "uid" else "id"
  rownames(out) <- NULL
  out
}

#' Fetch and parse the BODYSTRUCTURE of messages (INTERNAL HELPER)
#' @noRd
fetch_bodystructure_int <- function(self, msg_id, use_uid, retries) {
  msg_list <- fetch_metadata_int(self, msg_id, use_uid, attribute = "BODYSTRUCTURE",
                                 write_to_disk = FALSE, keep_in_mem = TRUE,
                                 mute = TRUE, retries = retries)
  ids <- sub("^metadata(UID)?", "", names(msg_list))
  rows <- Map(function(x, id) {
    df <- parse_bodystructure(x)
    cbind(data.frame(id = rep(suppressWarnings(as.integer(id)), nrow(df))), df)
  }, msg_list, ids)
  out <- do.call(rbind, rows)
  names(out)[1] <- if (isTRUE(use_uid)) "uid" else "id"
  rownames(out) <- NULL
  out
}
