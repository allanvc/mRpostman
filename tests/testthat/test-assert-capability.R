# Capability sets captured from real servers (offline simulation -- no network).
# Gmail advertises NAMESPACE/QUOTA/ID/UNSELECT/SPECIAL-USE but NOT SORT/THREAD.
gmail_caps <- c("IMAP4rev1", "UNSELECT", "IDLE", "NAMESPACE", "QUOTA", "ID",
                "XLIST", "CHILDREN", "X-GM-EXT-1", "UIDPLUS", "COMPRESS=DEFLATE",
                "ENABLE", "MOVE", "CONDSTORE", "ESEARCH", "UTF8=ACCEPT",
                "LIST-EXTENDED", "LIST-STATUS", "LITERAL-", "SPECIAL-USE",
                "APPENDLIMIT=35651584")

# A Dovecot-like server that DOES support SORT and THREAD.
dovecot_caps <- c("IMAP4rev1", "SORT", "SORT=DISPLAY", "THREAD=REFERENCES",
                  "THREAD=REFS", "THREAD=ORDEREDSUBJECT", "QUOTA", "NAMESPACE",
                  "ID", "UNSELECT", "SPECIAL-USE", "ESEARCH")

# Minimal fake connection object: a pre-populated cache means get_server_
# capabilities() returns it without touching the network.
fake_con <- function(caps) {
  e <- new.env()
  e$server_capabilities <- caps
  e
}

test_that("get_server_capabilities returns the cache without a network call", {
  con <- fake_con(gmail_caps)
  expect_identical(get_server_capabilities(con), gmail_caps)
})

test_that("assert_capability passes when the capability is present", {
  con <- fake_con(gmail_caps)
  expect_true(assert_capability(con, "QUOTA", command = "get_quota"))
  expect_true(assert_capability(con, "NAMESPACE", command = "namespace"))
  expect_true(assert_capability(con, "SPECIAL-USE",
                                command = "list_special_use_folders"))
})

test_that("assert_capability errors when the capability is absent (Gmail: no SORT/THREAD)", {
  con <- fake_con(gmail_caps)
  expect_error(assert_capability(con, "SORT", command = "sort"),
               "does not advertise")
  expect_error(
    assert_capability(con, paste0("THREAD=", toupper("REFERENCES")),
                      command = "thread", rfc = "RFC 5256"),
    "THREAD=REFERENCES")
})

test_that("assert_capability matching is case-insensitive", {
  con <- fake_con(gmail_caps)
  expect_true(assert_capability(con, "quota", command = "get_quota"))
  # advertised token in mixed case still matches an upper-case request
  con2 <- fake_con(c("IMAP4rev1", "Quota"))
  expect_true(assert_capability(con2, "QUOTA", command = "get_quota"))
})

test_that("exact THREAD=<algorithm> matching (as the thread() gate uses it)", {
  con <- fake_con(dovecot_caps)
  expect_true(assert_capability(con, "THREAD=REFERENCES", command = "thread"))
  expect_true(assert_capability(con, "THREAD=ORDEREDSUBJECT", command = "thread"))
  # an algorithm the server does NOT list must fail even on a THREAD-capable server
  expect_error(assert_capability(con, "THREAD=FOOBAR", command = "thread"),
               "does not advertise")
})

test_that("prefix = TRUE matches any capability starting with the token", {
  con <- fake_con(dovecot_caps)
  expect_true(assert_capability(con, "THREAD=", command = "thread",
                                prefix = TRUE))
  con_gmail <- fake_con(gmail_caps)   # Gmail has no THREAD= token at all
  expect_error(assert_capability(con_gmail, "THREAD=", command = "thread",
                                 prefix = TRUE),
               "does not advertise")
})

test_that("SORT/THREAD succeed on a capable (Dovecot-like) server", {
  con <- fake_con(dovecot_caps)
  expect_true(assert_capability(con, "SORT", command = "sort", rfc = "RFC 5256"))
  expect_true(assert_capability(con, paste0("THREAD=", toupper("REFERENCES")),
                                command = "thread", rfc = "RFC 5256"))
})

test_that("ESEARCH and MOVE gates (Gmail supports both; a minimal server does not)", {
  con <- fake_con(gmail_caps)     # Gmail advertises ESEARCH and MOVE
  expect_true(assert_capability(con, "ESEARCH", command = "search (esearch = TRUE)"))
  expect_true(assert_capability(con, "MOVE", command = "move_msg"))

  minimal <- fake_con(c("IMAP4rev1", "UNSELECT"))
  expect_error(assert_capability(minimal, "ESEARCH", command = "esearch_count_msg"),
               "does not advertise")
  expect_error(assert_capability(minimal, "MOVE", command = "move_msg"),
               "does not advertise")
})

test_that("the error message names the command and points to list_server_capabilities()", {
  con <- fake_con(gmail_caps)
  err <- tryCatch(assert_capability(con, "SORT", command = "sort",
                                    rfc = "RFC 5256"),
                  error = function(e) conditionMessage(e))
  expect_match(err, "sort")
  expect_match(err, "RFC 5256", fixed = TRUE)
  expect_match(err, "list_server_capabilities", fixed = TRUE)
})
