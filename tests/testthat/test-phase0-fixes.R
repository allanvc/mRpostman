# Regression tests for the phase-0 hardening round: parser fixes, the
# rejected-STORE detector, and the search-stripping guard.

test_that("parse_search_ids reads every untagged * SEARCH line", {
  one <- "* SEARCH 3 5 8\r\nA5 OK Search completed\r\n"
  expect_identical(mRpostman:::parse_search_ids(one), c(3, 5, 8))
  split <- "* SEARCH 3 5 8\r\n* SEARCH 11 13\r\nA5 OK Search completed\r\n"
  expect_identical(mRpostman:::parse_search_ids(split), c(3, 5, 8, 11, 13))
  modseq <- "* SEARCH 2 4 (MODSEQ 917162) \r\nA5 OK completed\r\n"
  expect_identical(mRpostman:::parse_search_ids(modseq), c(2, 4))
  expect_identical(mRpostman:::parse_search_ids("A5 OK done\r\n"), numeric(0))
})

test_that("parse_esearch_all only reads ALL from an untagged ESEARCH line", {
  resp <- '* ESEARCH (TAG "A2") ALL 1:3,5\r\nA2 OK done\r\n'
  expect_identical(mRpostman:::parse_esearch_all(resp), c(1L, 2L, 3L, 5L))
  # an "ALL n:m" inside message content must never be parsed as a result
  trap <- 'Subject: totals ALL 1:5 due\r\nA2 OK done\r\n'
  expect_identical(mRpostman:::parse_esearch_all(trap), integer(0))
  both <- 'body says ALL 7:9\r\n* ESEARCH (TAG "A2") UID ALL 2,4\r\nA2 OK\r\n'
  expect_identical(mRpostman:::parse_esearch_all(both), c(2L, 4L))
})

test_that("fix_search_stripping is a no-op when the mismatch is at the end", {
  clean <- c(1L, 5L, 9L)
  expect_identical(mRpostman:::fix_search_stripping(clean), clean)
  # unsortable tail: nothing to the right to infer from - must not error
  tail_mismatch <- c(5L, 3L)
  expect_identical(mRpostman:::fix_search_stripping(tail_mismatch), tail_mismatch)
})

test_that("find_no_reply sees tagged and untagged NO lines, ignores greetings", {
  ok <- "* OK Gimap ready\r\nA7 OK STORE completed\r\n"
  expect_identical(mRpostman:::find_no_reply(ok), NA_character_)
  untagged <- "* OK ready\r\n* NO [CANNOT] Invalid system flag \\Foo\r\nA7 OK done\r\n"
  expect_identical(mRpostman:::find_no_reply(untagged),
                   "[CANNOT] Invalid system flag \\Foo")
  tagged <- "* OK ready\r\nA7 NO STORE attempt on read-only mailbox\r\n"
  expect_identical(mRpostman:::find_no_reply(tagged),
                   "STORE attempt on read-only mailbox")
  # "NOOP"/"NOT" tokens must not be mistaken for a NO reply
  noop <- "* OK ready\r\nA7 OK NOOP completed\r\nA8 NOT-A-REAL-LINE\r\n"
  expect_identical(mRpostman:::find_no_reply(noop), NA_character_)
})

test_that("the fetch id placeholder only replaces the id slot", {
  # sub(fixed = TRUE) semantics used by execute_fetch_loop
  req <- "UID FETCH # (BODY.PEEK[HEADER.FIELDS (X-Note#7)])"
  out <- sub("#", "42", req, fixed = TRUE)
  expect_identical(out, "UID FETCH 42 (BODY.PEEK[HEADER.FIELDS (X-Note#7)])")
})
