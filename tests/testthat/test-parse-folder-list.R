# parse_folder_list(): shared LIST/LSUB parser. Splits folder names into root
# vs children using the server-declared hierarchy separator and drops \Noselect
# entries. Also acts as regression coverage for the refactored
# list_mail_folders_int(), which now delegates to this helper.

test_that("a LIST response splits root vs children and drops \\Noselect", {
  content <- paste0(
    '* LIST (\\HasNoChildren) "/" "INBOX"\r\n',
    '* LIST (\\HasChildren) "/" "Work"\r\n',
    '* LIST (\\HasNoChildren) "/" "Work/Reports"\r\n',
    '* LIST (\\Noselect \\HasChildren) "/" "[Gmail]"\r\n')
  out <- mRpostman:::parse_folder_list(content, command = "LIST")
  expect_identical(out$root, c("INBOX", "Work"))
  expect_identical(out$children, "Work/Reports")
})

test_that("an LSUB response is parsed the same way, keyed on the LSUB keyword", {
  content <- paste0(
    '* LSUB (\\HasNoChildren) "/" "INBOX"\r\n',
    '* LSUB (\\HasNoChildren) "/" "Work/Reports"\r\n')
  out <- mRpostman:::parse_folder_list(content, command = "LSUB")
  expect_identical(out$root, "INBOX")
  expect_identical(out$children, "Work/Reports")
})

test_that("a Yandex-style '|' hierarchy separator is handled", {
  content <- paste0(
    '* LIST (\\HasChildren) "|" "Root"\r\n',
    '* LIST (\\HasNoChildren) "|" "Root|Child"\r\n')
  out <- mRpostman:::parse_folder_list(content, command = "LIST")
  expect_identical(out$root, "Root")
  expect_identical(out$children, "Root|Child")
})
