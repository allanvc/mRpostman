# 2.3.0: the expression-based search language (pure translator, offline)
# and the Ops combinators on the criterion constructors.

q <- function(x) unclass(x)
translate_query <- mRpostman:::translate_query

test_that("translate_query() translates string-field comparisons", {
  expect_identical(q(translate_query(subject == "budget")), '(SUBJECT "budget")')
  expect_identical(q(translate_query(subject != "budget")), '(NOT (SUBJECT "budget"))')
  expect_identical(q(translate_query(from == "kaminski")), '(FROM "kaminski")')
  expect_identical(q(translate_query(text == 'say "hi"')), '(TEXT "say \\"hi\\"")')
  expect_error(translate_query(subject > "a"), "== \\(contains\\) and !=")
})

test_that("translate_query() translates flags", {
  expect_identical(q(translate_query(flag == "SEEN")), "(SEEN)")
  expect_identical(q(translate_query(flag != "SEEN")), "(UNSEEN)")
  expect_identical(q(translate_query(flag != "RECENT")), "(OLD)")
  expect_identical(q(translate_query(flag == "\\Draft")), "(DRAFT)")
  expect_identical(q(translate_query(flag == "todo")), "(KEYWORD todo)")
  expect_identical(q(translate_query(flag != "todo")), "(UNKEYWORD todo)")
})

test_that("translate_query() translates size and age", {
  expect_identical(q(translate_query(size > 5e6)), "(LARGER 5000000)")
  expect_identical(q(translate_query(size >= 5e6)), "(LARGER 4999999)")
  expect_identical(q(translate_query(size < 1400)), "(SMALLER 1400)")
  expect_identical(q(translate_query(size <= 1400)), "(SMALLER 1401)")
  expect_identical(q(translate_query(size == 100)), "(LARGER 99 SMALLER 101)")
  expect_identical(q(translate_query(age < 3600)), "(YOUNGER 3600)")
  expect_identical(q(translate_query(age >= 3600)), "(OLDER 3599)")
  expect_error(translate_query(age == 10), "seconds")
  expect_identical(q(translate_query(modseq >= 205)), "(MODSEQ 205)")
  expect_identical(q(translate_query(modseq > 205)), "(MODSEQ 206)")
})

test_that("translate_query() translates the three date fields exactly", {
  expect_identical(q(translate_query(sent >= "2001-10-01")), "(SENTSINCE 1-Oct-2001)")
  expect_identical(q(translate_query(sent > "2001-10-01")), "(SENTSINCE 2-Oct-2001)")
  expect_identical(q(translate_query(sent < "2002-01-01")), "(SENTBEFORE 1-Jan-2002)")
  expect_identical(q(translate_query(sent <= "2001-12-31")), "(SENTBEFORE 1-Jan-2002)")
  expect_identical(q(translate_query(sent == "2001-10-16")), "(SENTON 16-Oct-2001)")
  expect_identical(q(translate_query(date >= "01-Oct-2001")), "(SINCE 1-Oct-2001)")
  expect_identical(q(translate_query(saved >= as.Date("2026-01-02"))), "(SAVEDSINCE 2-Jan-2026)")
  expect_error(translate_query(sent >= "soon"), "not a date")
})

test_that("translate_query() combines with R's operators and precedence", {
  expect_identical(q(translate_query(subject == "a" & flag != "SEEN")),
                   '((SUBJECT "a") (UNSEEN))')
  expect_identical(q(translate_query(subject == "a" | text == "b")),
                   '(OR (SUBJECT "a") (TEXT "b"))')
  expect_identical(q(translate_query(!(subject == "a"))), '(NOT (SUBJECT "a"))')
  # & binds tighter than |, as in R
  expect_identical(q(translate_query(subject == "a" & flag != "SEEN" | text == "b")),
                   '(OR ((SUBJECT "a") (UNSEEN)) (TEXT "b"))')
  expect_identical(q(translate_query(subject == "a" & (flag != "SEEN" | text == "b"))),
                   '((SUBJECT "a") (OR (UNSEEN) (TEXT "b")))')
})

test_that("bare values inherit the preceding comparison (implicit OR)", {
  expect_identical(q(translate_query(subject == "budget" | "budget 3")),
                   '(OR (SUBJECT "budget") (SUBJECT "budget 3"))')
  expect_identical(q(translate_query((subject == "budget" | "budget 3") & flag != "SEEN")),
                   '((OR (SUBJECT "budget") (SUBJECT "budget 3")) (UNSEEN))')
  expect_error(translate_query(subject %in% list("a")), "vector of values")
  expect_error(translate_query("orphan"), "bare value")
})

test_that("%in% expands into OR chains over one field", {
  expect_identical(q(translate_query(subject %in% c("a", "b"))),
                   '(OR (SUBJECT "a") (SUBJECT "b"))')
  expect_identical(q(translate_query(subject %in% c("a", "b", "c"))),
                   '(OR (OR (SUBJECT "a") (SUBJECT "b")) (SUBJECT "c"))')
  expect_identical(q(translate_query(flag %in% c("SEEN", "todo"))),
                   "(OR (SEEN) (KEYWORD todo))")
})

test_that("variables and helper calls are evaluated in the caller frame", {
  needle <- "budget"
  n <- 5e6
  expect_identical(q(translate_query(subject == needle & size > n)),
                   '((SUBJECT "budget") (LARGER 5000000))')
  expect_identical(q(translate_query(string("x", where = "SUBJECT") & flag != "SEEN")),
                   '((SUBJECT "x") (UNSEEN))')
  expect_identical(q(translate_query(header("X-Mailer") == "Outlook")),
                   '(HEADER X-Mailer "Outlook")')
  expect_error(translate_query(subject), "must appear in a comparison")
  expect_error(translate_query(header("X-Mailer")), "must appear in a comparison")
})

test_that("Ops combinators work on the criterion constructors", {
  expect_identical(q(string("budget", where = "SUBJECT") & !flag("SEEN")),
                   '((SUBJECT "budget") (NOT (SEEN)))')
  expect_identical(q(string("a", where = "SUBJECT") |
                       (sent_since(date_char = "01-Oct-2001") & flag("FLAGGED"))),
                   paste0('(OR (SUBJECT "a") ((SENTSINCE 01-Oct-2001) (FLAGGED)))'))
  expect_error(string("a", where = "SUBJECT") + flag("SEEN"), "not defined")
})

rare_sandbox_up2 <- function() {
  tryCatch({
    s <- mRpostman:::imap_socket_open("imap://localhost:1430", 2000, FALSE)
    mRpostman:::imap_socket_close(s); TRUE
  }, error = function(e) FALSE)
}

test_that("query() returns the same ids as the classic search on the sandbox", {
  skip_if_not(rare_sandbox_up2(), "Docker sandbox not reachable on localhost:1430")
  con <- configure_imap(url = "imap://localhost:1430",
                        username = paste0("qt", format(Sys.time(), "%H%M%S")),
                        password = "sandbox", use_ssl = FALSE)
  invisible(populate_sandbox(con, n = 50, seed = 3501, mute = TRUE))
  con$select_folder("INBOX", mute = TRUE)
  a <- con$query(subject == "Relat" | flag != "SEEN")
  b <- con$search(request = OR(string("Relat", where = "SUBJECT"), flag("UNSEEN")))
  expect_identical(sort(a), sort(b))
  big <- con$query(size > 8000)
  expect_identical(sort(big), sort(con$search_larger_than(8000)))
  d <- con$query(sent >= "2020-01-01" & sent < "2020-04-01")
  expect_true(length(d) > 0)
  expect_identical(sort(d), sort(con$search(request = AND(
    sent_since(date_char = "01-Jan-2020"), sent_before(date_char = "01-Apr-2020")))))
  expect_identical(sort(con$query(size < 1400)), sort(con$search_smaller_than(1400)))
  expect_identical(sort(con$query(flag == "FLAGGED")), sort(con$search_flag("FLAGGED")))
  expect_identical(con$query(subject == "zzz-no-such-subject", esearch = TRUE), integer(0))
  con$disconnect()
})

# ---- verbatim(): verbatim fragments inside the query language -----------

test_that("verbatim() wraps fragments and composes with the language", {
  expect_s3_class(verbatim("UID 100:200"), "imap_search")
  expect_identical(unclass(verbatim("UID 100:200")), "(UID 100:200)")
  # already-parenthesized fragments are kept as is
  expect_identical(unclass(verbatim('(X-GM-RAW "has:attachment")')),
                   '(X-GM-RAW "has:attachment")')
  # two adjacent groups are not a single group: wrapped
  expect_identical(unclass(verbatim("(SEEN) (DRAFT)")), "((SEEN) (DRAFT))")
  # a quoted parenthesis must not fool the balance check
  expect_identical(unclass(verbatim('HEADER X-Note "a)b"')),
                   '(HEADER X-Note "a)b")')
  translate_query <- mRpostman:::translate_query
  expect_identical(
    unclass(translate_query(verbatim("UID 1:10") & flag != "SEEN")),
    "((UID 1:10) (UNSEEN))")
  expect_error(verbatim(c("a", "b")))
  expect_error(verbatim("   "))
  expect_error(verbatim(NA_character_))
})

test_that("query(verbatim()) matches the classic custom search on the sandbox", {
  skip_if_not(rare_sandbox_up2(), "Docker sandbox not reachable on localhost:1430")
  con <- configure_imap(url = "imap://localhost:1430",
                        username = paste0("qr", format(Sys.time(), "%H%M%S")),
                        password = "sandbox", use_ssl = FALSE)
  invisible(populate_sandbox(con, n = 50, seed = 3501, mute = TRUE))
  con$select_folder("INBOX", mute = TRUE)
  a <- con$query(verbatim("LARGER 8000"), use_uid = TRUE)
  b <- con$search(request = "(LARGER 8000)", use_uid = TRUE)
  expect_identical(sort(a), sort(b))
  # a raw sequence set combined with a translated field
  uids <- sort(con$search_larger_than(1, use_uid = TRUE))
  rng <- paste0("UID ", min(uids), ":", max(uids))
  d <- con$query(verbatim(rng) & size > 8000, use_uid = TRUE)
  expect_identical(sort(d), sort(b))
  con$disconnect()
})
