# ACL extension (RFC 4314): GETACL, SETACL, DELETEACL, LISTRIGHTS, MYRIGHTS.
# None of these commands needs a selected folder.

#' Resolve the folder name argument of the ACL helpers (INTERNAL HELPER)
#' @noRd
acl_folder <- function(self, name) {
  if (!is.null(name)) {
    assertthat::assert_that(is.character(name),
                            msg='"name" must be of type character or NULL.')
    return(adjust_folder_name(name))
  }
  assertthat::assert_that(!is.na(self$con_params$folder),
                          msg='No folder previously selected.')
  adjust_folder_name(self$con_params$folder)
}

#' @noRd
get_acl_int <- function(self, name, retries) {
  folder <- acl_folder(self, name)
  check_args(retries = retries)
  assert_capability(self, "ACL", command = "get_acl", rfc = "RFC 4314",
                    retries = retries)
  parse_acl(execute_simple_command(self, paste0("GETACL ", folder), retries))
}

#' @noRd
set_acl_int <- function(self, name, identifier, rights, retries) {
  folder <- acl_folder(self, name)
  assertthat::assert_that(is.character(identifier), length(identifier) == 1,
                          msg='"identifier" must be a single character string.')
  assertthat::assert_that(is.character(rights), length(rights) == 1,
                          grepl("^[+-]?[a-z]+$", rights),
                          msg='"rights" must be a string of right letters, optionally prefixed with "+" or "-" (e.g. "lrs", "+w", "-d").')
  check_args(retries = retries)
  assert_capability(self, "ACL", command = "set_acl", rfc = "RFC 4314",
                    retries = retries)
  execute_simple_command(self, paste("SETACL", folder, identifier, rights),
                         retries)
  invisible(TRUE)
}

#' @noRd
delete_acl_int <- function(self, name, identifier, retries) {
  folder <- acl_folder(self, name)
  assertthat::assert_that(is.character(identifier), length(identifier) == 1,
                          msg='"identifier" must be a single character string.')
  check_args(retries = retries)
  assert_capability(self, "ACL", command = "delete_acl", rfc = "RFC 4314",
                    retries = retries)
  execute_simple_command(self, paste("DELETEACL", folder, identifier), retries)
  invisible(TRUE)
}

#' @noRd
list_rights_int <- function(self, name, identifier, retries) {
  folder <- acl_folder(self, name)
  assertthat::assert_that(is.character(identifier), length(identifier) == 1,
                          msg='"identifier" must be a single character string.')
  check_args(retries = retries)
  assert_capability(self, "ACL", command = "list_rights", rfc = "RFC 4314",
                    retries = retries)
  parse_listrights(execute_simple_command(
    self, paste("LISTRIGHTS", folder, identifier), retries))
}

#' @noRd
my_rights_int <- function(self, name, retries) {
  folder <- acl_folder(self, name)
  check_args(retries = retries)
  assert_capability(self, "ACL", command = "my_rights", rfc = "RFC 4314",
                    retries = retries)
  parse_myrights(execute_simple_command(self, paste("MYRIGHTS", folder),
                                        retries))
}
