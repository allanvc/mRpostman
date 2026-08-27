# 2.2.0: the remaining registered IMAP capabilities (offline parsers,
# capability gates, and a live round trip for the ones the sandbox serves).

test_that("parse_esort_partial() reads the PARTIAL item of an ESEARCH response", {
  p <- mRpostman:::parse_esort_partial('* ESEARCH (TAG "R001") UID PARTIAL (1:10 3,5,9:11)\r\n')
  expect_identical(as.integer(p), c(3L, 5L, 9L, 10L, 11L))
  expect_identical(attr(p, "range"), "1:10")
  expect_identical(mRpostman:::parse_esort_partial('* ESEARCH (TAG "R001") UID PARTIAL (-1:-100 NIL)'),
                   structure(integer(0), range = "-1:-100"))
  expect_identical(mRpostman:::parse_esort_partial("R001 OK done"), integer(0))
  # a SORT result keeps the server order, including descending ranges
  expect_identical(as.integer(mRpostman:::parse_esort_partial('PARTIAL (1:5 9:7,2)')),
                   c(9L, 8L, 7L, 2L))
})

test_that("parse_uid_batches() reads the UIDBATCHES response", {
  b <- mRpostman:::parse_uid_batches('* UIDBATCHES (TAG "A143") 215295:99696,99695:20351,20350:1\r\nA143 OK done')
  expect_identical(b$from, c(215295L, 99695L, 20350L))
  expect_identical(b$to, c(99696L, 20351L, 1L))
  expect_identical(nrow(mRpostman:::parse_uid_batches('* UIDBATCHES (TAG "A1")\r\n')), 0L)
})

test_that("parse_esearch_multi() reads multimailbox ESEARCH responses", {
  df <- mRpostman:::parse_esearch_multi(c(
    '* ESEARCH (TAG "t1" MAILBOX "folder1" UIDVALIDITY 1) UID ALL 4001,4003',
    '* ESEARCH (TAG "t1" MAILBOX folder2/banana UIDVALIDITY 503) UID ALL 3002:3004',
    '* ESEARCH (TAG "t1" MAILBOX "empty" UIDVALIDITY 9)'))
  expect_identical(df$mailbox, c("folder1", "folder1", rep("folder2/banana", 3)))
  expect_identical(df$uid, c(4001L, 4003L, 3002L, 3003L, 3004L))
  expect_identical(df$uidvalidity[3], 503L)
  expect_identical(nrow(mRpostman:::parse_esearch_multi(character(0))), 0L)
})

test_that("parse_objectid() reads EMAILID/THREADID items", {
  txt <- paste0('* 1 FETCH (EMAILID (M6d99ac3275bb4e) THREADID (T64b478a75b7ea9))\r\n',
                '* 2 FETCH (UID 25 EMAILID (M5fdc09b49ea703) THREADID NIL)\r\n')
  df <- mRpostman:::parse_objectid(txt, use_uid = TRUE)
  expect_identical(df$id, c(1L, 25L))
  expect_identical(df$emailid[1], "M6d99ac3275bb4e")
  expect_true(is.na(df$threadid[2]))
})

test_that("parse_annotation() reads ANNOTATION items", {
  txt <- '* 1 FETCH (ANNOTATION (/comment (value.priv "My comment" value.shared NIL)))\r\n'
  df <- mRpostman:::parse_annotation(txt)
  expect_identical(df$entry, rep("/comment", 2))
  expect_identical(df$attribute, c("value.priv", "value.shared"))
  expect_identical(df$value, c("My comment", NA_character_))
  txt2 <- '* 3 FETCH (UID 30 ANNOTATION (/comment (size.priv 10)))\r\n'
  df2 <- mRpostman:::parse_annotation(txt2, use_uid = TRUE)
  expect_identical(df2$id, 30L)
  expect_identical(df2$value, "10")
})

test_that("parse_status_counts() carries MAILBOXID through", {
  out <- mRpostman:::parse_status_counts('* STATUS "INBOX" (MESSAGES 5 MAILBOXID (F2212ea87-6097))\r\n')
  expect_identical(out[["MESSAGES"]], "5")
  expect_identical(out[["MAILBOXID"]], "F2212ea87-6097")
  # numeric-only responses keep returning numbers
  out2 <- mRpostman:::parse_status_counts('* STATUS "INBOX" (MESSAGES 5 UNSEEN 2)\r\n')
  expect_identical(out2, c(MESSAGES = 5, UNSEEN = 2))
})

test_that("split_fetch_responses() accepts UIDONLY's UIDFETCH blocks", {
  raw_text <- paste0("* 25 UIDFETCH (FLAGS (\\Seen) BODY[] {5}\r\nhello)\r\n",
                     "a1 OK done\r\n")
  out <- mRpostman:::split_fetch_responses(raw_text, "text", use_uid = TRUE)
  expect_identical(names(out), "textUID25")
})

test_that("fuzzy() and filter_stored() build RFC 6203/5466 criteria", {
  expect_identical(fuzzy(string(expr = "jump", where = "SUBJECT")),
                   '(FUZZY SUBJECT "jump")')
  expect_identical(fuzzy("TEXT \"bar\""), "FUZZY TEXT \"bar\"")
  expect_identical(filter_stored("on-the-road"), "(FILTER on-the-road)")
})

test_that("assert_within_appendlimit() enforces the advertised limit", {
  con <- configure_imap(url = "imap://localhost:1430", username = "x",
                        password = "x", use_ssl = FALSE)
  con$server_capabilities <- c("IMAP4REV1", "APPENDLIMIT=10")
  expect_error(mRpostman:::assert_within_appendlimit(con, 100), "APPENDLIMIT")
  expect_true(mRpostman:::assert_within_appendlimit(con, c(3, 10)))
  con$server_capabilities <- c("IMAP4REV1", "APPENDLIMIT")
  expect_true(mRpostman:::assert_within_appendlimit(con, 1e9))
})

test_that("the new extension methods are capability-gated", {
  con <- configure_imap(url = "imap://localhost:1430", username = "x",
                        password = "x", use_ssl = FALSE)
  con$server_capabilities <- c("IMAP4REV1")
  con$con_params$folder <- "INBOX"
  expect_error(con$esearch_partial("1:10"), "PARTIAL")
  expect_error(con$esort_partial("1:10"), "SORT")
  expect_error(con$replace_msg(1, "Subject: x\r\n\r\nb\r\n"), "REPLACE")
  expect_error(con$fetch_objectid(1), "OBJECTID")
  expect_error(con$uid_batches(100), "UIDBATCHES")
  expect_error(con$esearch_multi("personal"), "MULTISEARCH")
  expect_error(con$unauthenticate(), "UNAUTHENTICATE")
  expect_error(con$language(), "LANGUAGE")
  expect_error(con$comparator(), "I18NLEVEL=2")
  expect_error(con$genurlauth(imap_url("INBOX", 1)), "URLAUTH")
  expect_error(con$urlfetch("imap://x@y/INBOX/;UID=1"), "URLAUTH")
  expect_error(con$fetch_convert(1, "application/pdf"), "CONVERT")
  expect_error(con$fetch_annotation(1), "ANNOTATE-EXPERIMENT-1")
  expect_error(con$store_annotation(1, "/comment", c("value.priv" = "x")),
               "ANNOTATE-EXPERIMENT-1")
  expect_error(con$sort(by = "DISPLAYFROM"), "SORT")
  expect_error(con$thread(algorithm = "REFS"), "THREAD=REFS")
  expect_error(con$status(items = "MAILBOXID"), "OBJECTID")
  expect_error(con$search(request = fuzzy(string("x", "SUBJECT"))), "SEARCH=FUZZY")
  expect_error(con$search(request = filter_stored("f")), "FILTERS")
})

rare_sandbox_up <- function() {
  tryCatch({
    s <- mRpostman:::imap_socket_open("imap://localhost:1430", 2000, FALSE)
    mRpostman:::imap_socket_close(s); TRUE
  }, error = function(e) FALSE)
}

test_that("SORT=DISPLAY, THREAD=REFS, PARTIAL, and LITERAL+ work on the sandbox", {
  skip_if_not(rare_sandbox_up(), "Docker sandbox not reachable on localhost:1430")
  con <- configure_imap(url = "imap://localhost:1430", username = "rawtest3",
                        password = "sandbox", use_ssl = FALSE)
  from <- c("Zed", "Ann", "Mid")
  msgs <- vapply(1:3, function(i) {
    paste0("From: \"", from[i], "\" <", letters[i], "@example.com>\r\n",
           "Subject: display ", i, "\r\n\r\nbody\r\n")
  }, "")
  # LITERAL+ is advertised by Dovecot: this MULTIAPPEND goes out with
  # non-synchronizing "{n+}" literals in a single write
  uids <- con$append_msgs(msgs, folder = "INBOX", mute = TRUE)
  expect_length(uids, 3); expect_false(anyNA(uids))
  con$select_folder("INBOX", mute = TRUE)
  s <- con$sort(by = "DISPLAYFROM", use_uid = TRUE)
  expect_true(all(uids %in% s))
  expect_true(which(s == uids[2]) < which(s == uids[1]))  # "Ann" before "Zed"
  th <- con$thread(algorithm = "REFS")
  expect_true(length(th) >= 1)
  p <- con$esearch_partial("1:2", use_uid = TRUE)   # via CONTEXT=SEARCH
  expect_true(length(p) == 2)
  expect_identical(attr(p, "range"), "1:2")
  con$disconnect()
})
