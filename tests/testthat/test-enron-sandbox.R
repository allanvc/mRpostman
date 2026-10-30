# enron_sandbox() is exercised offline: a tiny fake corpus is laid out under a
# temporary cache_dir (so nothing is downloaded) and ingest_maildir() is
# mocked, so no server is needed. What is under test is the subset selection
# and the manifest that ties every selected file to the server (folder + uid).

fake_corpus <- function(custodians = c("kaminski-v", "lay-k")) {
  cache_dir <- tempfile("enron-cache-")
  maildir <- file.path(cache_dir, "maildir")
  write_msg <- function(cust, folder, name, date) {
    d <- file.path(maildir, cust, folder)
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
    writeLines(c(paste0("Message-ID: <", name, "@", cust, ">"),
                 paste0("Date: ", date),
                 "From: someone@enron.com",
                 "",
                 "Body."), file.path(d, name))
  }
  for (cust in custodians) {
    write_msg(cust, "sent", "1.", "Mon, 14 May 2001 16:39:00 -0700 (PDT)")
    write_msg(cust, "sent", "2.", "Tue, 15 May 2001 09:00:00 -0700 (PDT)")
    write_msg(cust, "inbox", "3.", "Wed, 16 May 2001 09:00:00 -0700 (PDT)")
    write_msg(cust, "_sent_mail", "4.", "Thu, 2 Aug 2001 09:00:00 -0700 (PDT)")
    # the corpus has a few files directly under the custodian, in no folder
    write_msg(cust, "", "5.", "Fri, 3 Aug 2001 09:00:00 -0700 (PDT)")
  }
  cache_dir
}

fake_imapcon <- function() structure(list(), class = "ImapCon")

# the corpus names its files "1.", "2.", ...; Windows drops a trailing dot
# when a file is created, so there the fixture (and a corpus extracted there)
# ends up with "1", "2", ...
corpus_name <- function(x) {
  if (.Platform$OS.type == "windows") sub("\\.$", "", x) else x
}

# a stand-in for ingest_maildir(): records the calls and hands out UIDs
local_fake_ingest <- function(.env = parent.frame()) {
  rec <- new.env(parent = emptyenv())
  rec$calls <- list()
  testthat::local_mocked_bindings(
    ingest_maildir = function(con, path = NULL, folder = basename(path),
                              create = TRUE, recursive = TRUE, files = NULL,
                              mute = FALSE, retries = 1) {
      rec$calls[[length(rec$calls) + 1L]] <- list(folder = folder,
                                                  files = files)
      n <- length(files)
      appended <- rep(TRUE, n)
      if (n > 1) appended[n] <- FALSE   # simulate one rejected upload
      invisible(data.frame(path = files,
                           size = file.size(files),
                           appended = appended,
                           uid = ifelse(appended, seq_len(n) + 100L,
                                        NA_integer_),
                           stringsAsFactors = FALSE))
    },
    .package = "mRpostman", .env = .env)
  rec
}

test_that("enron_sandbox() manifest carries folder and uid aligned to path", {
  cache_dir <- fake_corpus()
  on.exit(unlink(cache_dir, recursive = TRUE))
  rec <- local_fake_ingest()

  man <- enron_sandbox(fake_imapcon(), custodians = "kaminski-v",
                       folder_pattern = ".*", cache_dir = cache_dir,
                       ask = FALSE, mute = TRUE)

  expect_s3_class(man, "data.frame")
  expect_named(man, c("custodian", "folder", "path", "size", "date",
                      "appended", "uid"))
  expect_equal(nrow(man), 5L)
  expect_true(all(man$custodian == "kaminski-v"))
  expect_true(all(man$folder == "kaminski-v"))
  # path is relative to the corpus root: custodian/folder/file, or
  # custodian/file for a file that sits in no folder (".*" matches "")
  expect_identical(man$path, corpus_name(c("kaminski-v/5.",
                                           "kaminski-v/_sent_mail/4.",
                                           "kaminski-v/inbox/3.",
                                           "kaminski-v/sent/1.",
                                           "kaminski-v/sent/2.")))
  # uid is the one ingest_maildir() reported, row by row; NA on failure
  expect_identical(man$uid, c(101L, 102L, 103L, 104L, NA_integer_))
  expect_identical(man$appended, c(TRUE, TRUE, TRUE, TRUE, FALSE))
  expect_s3_class(man$date, "Date")
  expect_identical(as.character(man$date),
                   c("2001-08-03", "2001-08-02", "2001-05-16", "2001-05-14",
                     "2001-05-15"))
  # one ingest_maildir() call, into a folder named after the custodian
  expect_length(rec$calls, 1L)
  expect_identical(rec$calls[[1]]$folder, "kaminski-v")
  expect_identical(basename(rec$calls[[1]]$files),
                   corpus_name(c("5.", "4.", "3.", "1.", "2.")))
})

test_that("enron_sandbox() default folder_pattern keeps only sent folders", {
  cache_dir <- fake_corpus()
  on.exit(unlink(cache_dir, recursive = TRUE))
  local_fake_ingest()

  man <- enron_sandbox(fake_imapcon(), custodians = c("kaminski-v", "lay-k"),
                       cache_dir = cache_dir, ask = FALSE, mute = TRUE)

  expect_equal(nrow(man), 6L)
  expect_identical(unique(man$custodian), c("kaminski-v", "lay-k"))
  expect_identical(man$folder, man$custodian)
  expect_true(all(dirname(man$path) %in% c("kaminski-v/_sent_mail",
                                           "kaminski-v/sent",
                                           "lay-k/_sent_mail", "lay-k/sent")))
  expect_false(any(grepl("/inbox/", man$path)))
  # the default pattern does not match "", so the folderless files stay out
  expect_false(any(basename(man$path) == corpus_name("5.")))
})

test_that("enron_sandbox() applies the Date: window and max_msgs", {
  cache_dir <- fake_corpus()
  on.exit(unlink(cache_dir, recursive = TRUE))
  local_fake_ingest()

  man <- enron_sandbox(fake_imapcon(), custodians = "lay-k",
                       folder_pattern = ".*",
                       sent_since = "2001-05-15", sent_before = "2001-08-01",
                       cache_dir = cache_dir, ask = FALSE, mute = TRUE)
  expect_identical(man$path, corpus_name(c("lay-k/inbox/3.", "lay-k/sent/2.")))
  # a pattern that matches "" but no real folder name selects only the root files
  man <- enron_sandbox(fake_imapcon(), custodians = "lay-k",
                       folder_pattern = "^$",
                       cache_dir = cache_dir, ask = FALSE, mute = TRUE)
  expect_identical(man$path, corpus_name("lay-k/5."))

  man <- enron_sandbox(fake_imapcon(), custodians = "lay-k",
                       folder_pattern = ".*", max_msgs = 1,
                       cache_dir = cache_dir, ask = FALSE, mute = TRUE)
  expect_equal(nrow(man), 1L)
  expect_identical(man$uid, 101L)
})

test_that("enron_sandbox() rejects an unknown custodian", {
  cache_dir <- fake_corpus()
  on.exit(unlink(cache_dir, recursive = TRUE))
  local_fake_ingest()

  expect_error(
    enron_sandbox(fake_imapcon(), custodians = "nobody-x",
                  cache_dir = cache_dir, ask = FALSE, mute = TRUE),
    "not found in the corpus")
})
