#' @title An IMAP Connection Class
#' @description Configure an IMAP connection using the \code{ImapCon} \code{R6}
#'   class.
#' @importFrom R6 R6Class
#' @export
#' @examples
#' \dontrun{
#' # w/ Plain authentication
#' con <- configure_imap(
#'   url="imaps://outlook.office365.com",
#'   username="user@agency.gov.br",
#'   password=rstudioapi::askForPassword(),
#'   verbose = TRUE)
#'
#' # OR
#' con <- ImapCon$new(
#'   url="imaps://outlook.office365.com",
#'   username="user@agency.gov.br",
#'   password=rstudioapi::askForPassword(),
#'   verbose = TRUE)
#'
#' # w/ OAuth2.0 authentication
#' con <- configure_imap(
#'   url="imaps://outlook.office365.com",
#'   username="user@agency.gov.br",
#'   verbose = TRUE,
#'   xoauth2_bearer = "XX.Ya9...")
#'
#' # OR
#' con <- ImapCon$new(
#'   url="imaps://outlook.office365.com",
#'   username="user@agency.gov.br",
#'   verbose = TRUE,
#'   xoauth2_bearer = "XX.Ya9...")
#'
#' }
#'
#'
ImapCon <- R6::R6Class("ImapCon",
  portable = FALSE,
  lock_objects = FALSE,
  # private = list(
  #   # password = character(0)#,
  #   # xoauth2_bearer = character(0),
  # ),
  public = list(

    #' @description Configure and create a new IMAP connection.
    #' @param url A character string containing the IMAP server address
    #' @param username A character string containing the username.
    #' @param password A character string containing the user's password.
    #' @param xoauth2_bearer A character string containing the oauth2 bearer token.
    #' @param use_ssl A logical indicating the use or not of Secure Sockets Layer
    #'   encryption when connecting to the IMAP server. Default is \code{TRUE}.
    #' @param verbose If \code{FALSE}, mutes the flow of information between the
    #'   server and the client. Default is \code{FALSE}.
    #' @param buffersize The size in bytes for the receive buffer. Default is
    #'   16000 bytes or 16kb, which means it will use the libcurl's default value.
    #'   According to the libcurl's documentation, the maximum buffersize is 512kb
    #'   (or 512000 bytes), but any number passed to \code{buffersize} is treated
    #'   as a request, not an order.
    #' @param timeout_ms Time in milliseconds (ms) to wait for the execution or
    #'   re-execution of a command. Default is 0, which means that no timeout limit is
    #'   set.
    #' @param ... Further curl parameters (see \code{curl::curl_options}) that
    #'   can be used with the IMAP protocol. Only for advanced users.
    #' @note \href{#method-new}{\code{ImapCon$new()}}: The \code{\link{configure_imap}}
    #'   should be preferred instead of \code{ImapCon$new()}.
    #' @return A new `ImapCon` object.
    initialize = function(url,
                          username,
                          password = NULL,
                          xoauth2_bearer = NULL,
                          use_ssl = TRUE,
                          verbose = FALSE,
                          buffersize = 16000,
                          timeout_ms = 0,
                          ...) {

      out <- config_con_handle_and_params(url = url, username = username,
                                   password = password, xoauth2_bearer = xoauth2_bearer,
                                   use_ssl = use_ssl, verbose = verbose,
                                   buffersize = buffersize, timeout_ms = timeout_ms,
                                   ...)

      # print(out$con_params)

      self$con_params <- out$con_params
      self$con_handle <- out$con_handle

      self$con_params$folder <- NA



    },
    # R6 methods

    ## RESET methods

    #' @description Reset the previously informed url
    #' @param x A character string containing a new url to be set.
    reset_url = function(x) {

      url = x

      assertthat::assert_that(
        is.character(url),
        msg='Argument "x" must be a string, e.g. "imaps://imap.servername.com".')

      url <- utils::URLencode(gsub("/+$", "", url))
      check_url <- grepl("^(imap|imaps)://\\w", url)

      assertthat::assert_that(
        isTRUE(check_url),
        msg='Invalid url! Try the following format: "imaps://imap.servername.com".')

      self$con_params$url <- utils::URLencode(gsub("/+$", "", url))
    },

    #' @description Reset the previously informed username
    #' @param x A character string containing a new username to be set.
    reset_username = function(x) {

      username = x

      modify_con_handle(self, username = username) # same strategy from check_args() to keep a named list
      self$con_params$username <- username

    },

    #' @description Reset the previously informed use_ssl parameter
    #' @param x A logical indicating the use or not of Secure Sockets Layer
    #'   encryption when connecting to the IMAP server. Default is \code{TRUE}.
    reset_use_ssl = function(x) {

      use_ssl = x

      modify_con_handle(self, use_ssl = use_ssl)
      self$con_params$use_ssl <- use_ssl

    },

    #' @description Reset the previously informed verbose parameter
    #' @param x If \code{FALSE}, mutes the flow of information between the
    #'   server and the client.
    reset_verbose = function(x) {

      verbose = x

      # verbose = to
      modify_con_handle(self, verbose = verbose)
      self$con_params$verbose <- verbose

    },

    #' @description Reset the previously informed buffersize parameter
    #' @param x The size in bytes for the receive buffer. Default is
    #'   16000 bytes or 16kb, which means it will use the libcurl's default value.
    #'   According to the libcurl's documentation, the maximum buffersize is 512kb
    #'   (or 512000 bytes), but any number passed to \code{buffersize} is treated
    #'   as a request, not an order.
    reset_buffersize = function(x) {

      buffersize = x

      modify_con_handle(self, buffersize = buffersize)
      self$con_params$buffersize <- buffersize

    },

    #' @description Reset the previously informed buffersize parameter
    #' @param x Time in milliseconds (ms) to wait for the execution or
    #'   re-execution of a command. Default is 0, which means that no timeout limit is
    #'   set.
    reset_timeout_ms = function(x) {

      timeout_ms = x

      modify_con_handle(self, timeout_ms = timeout_ms)
      self$con_params$timeout_ms <- timeout_ms

    },

    #' @description Reset the previously informed password
    #' @param x A character string containing the user's password.
    reset_password = function(x) {

      password = x

      modify_con_handle(self, password = password)

    },

    #' @description Reset the previously informed oauth2 bearer token
    #' @param x A character string containing the oauth2 bearer token.
    reset_xoauth2_bearer = function(x) {

      xoauth2_bearer = x

      modify_con_handle(self, xoauth2_bearer = xoauth2_bearer)

    },

    # List elements
    # access = function() {
    #   list(
    #     url = self$url,
    #     user = self$user
    #   )
    # },

    # MAIN METHODS:

    ## server capabalities
    #' @description List the server's IMAP capabilities.
    #' @param retries Number of attempts to connect and execute the command.
    #'   Default is \code{1}.
    #' @return A character \code{vector} containing the server's IMAP capabilities.
    #' @examples
    #' \dontrun{
    #' cap <- con$list_server_capabilities()
    #' cap
    #' }
    list_server_capabilities = function(retries = 1) {
      out <- list_server_capabilities_int(self, retries)
      return(out)
    },

    ## mailbox operations
    #' @description List mail folders in a mailbox.
    #' @param retries Number of attempts to connect and execute the command.
    #'   Default is \code{1}.
    #' @return A \code{list} containing the mail folder names and their inherent
    #'   structure.
    #' @examples
    #' \dontrun{
    #' folders <- con$list_mail_folders()
    #' folders
    #' }
    list_mail_folders = function(retries = 1) {
      out <- list_mail_folders_int(self, retries)
      return(out)
    },

    #' @description Select a mail folder.
    #' @param name A string containing the name of an existing mail folder on the
    #'   user's mailbox.
    #' @param mute A \code{logical}. If \code{TRUE}, mutes the confirmation message
    #'   when the command is successfully executed. Default is \code{FALSE}.
    #' @param retries Number of attempts to connect and execute the command.
    #'   Default is \code{1}.
    #' @return A \code{list} containing the mail folder names and their inherent
    #'   structure.
    #' @examples
    #' \dontrun{
    #' con$select_mail_folder(name = "INBOX")
    #' }
    select_folder = function(name, mute = FALSE, retries = 1) {
      self$con_params$folder <- select_folder_int(self, name, mute, retries)
      invisible(TRUE)
    },

    #' @description Examine the number of messages in a mail folder.
    #' @param name A \code{character} string containing the name of an existing
    #'   mail folder on the user's mailbox. If no name is passed, the command
    #'   will be executed using the previously selected mail folder name.
    #' @param retries Number of attempts to connect and execute the command.
    #'   Default is \code{1}.
    #' @return A \code{vector} (with names \code{"EXISTS"} and \code{"RECENT"})
    #'   containing the number of messages in each category.
    #' @examples
    #' \dontrun{
    #' con$select_folder(name = "INBOX")
    #' con$examine_folder()
    #'
    #' # or directly:
    #' con$examine_folder("Sent")
    #' }
    examine_folder = function(name = NULL, retries = 1) {
      out <- examine_folder_int(self, name, retries)
      return(out)
    },

    #' @description Create a new mail folder.
    #' @param name A string containing the name of the new mail folder to be
    #'   created.
    #' @param mute A \code{logical}. If \code{TRUE}, mutes the confirmation message
    #'   when the command is successfully executed. Default is \code{FALSE}.
    #' @param retries Number of attempts to connect and execute the command.
    #'   Default is \code{1}.
    #' @return \code{TRUE} in case the operation is successful.
    #' @examples
    #' \dontrun{
    #' con$create_folder(name = "New Folder Name")
    #' }
    create_folder = function(name, mute = FALSE, retries = 1) {
      invisible(create_folder_int(self, name, mute, retries))
    },

    #' @description Rename a mail folder.
    #' @param name A string containing the name of the new mail folder to be
    #'   renamed. If no name is passed, the command will be executed using the
    #'   previously selected mail folder name.
    #' @param new_name A string containing the new name to be assigned.
    #' @param reselect A logical. If \code{TRUE}, calls
    #'   \code{select_folder(name = to_folder)} under the hood before returning
    #'   the output. Default is \code{TRUE}.
    #' @param mute A \code{logical}. If \code{TRUE}, mutes the confirmation message
    #'   when the command is successfully executed. Default is \code{FALSE}.
    #' @param retries Number of attempts to connect and execute the command.
    #'   Default is \code{1}.
    #' @return \code{TRUE} in case the operation is successful.
    #' @examples
    #' \dontrun{
    #' con$select_folder(name = "Folder A")
    #' con$rename_folder(new_name = "Folder B")
    #' # or directly:
    #' con$rename_folder(name = "Folder A", new_name = "Folder B")
    #' }
    rename_folder = function(name = NULL, new_name, reselect = TRUE,
                             mute = FALSE, retries = 1) {
      self$con_params$folder <- rename_folder_int(self, name, new_name, reselect, mute,
                                       retries)
      invisible(TRUE)
    },

    #' @description List flags in a selected mail folder
    #' @param retries Number of attempts to connect and execute the command.
    #'   Default is \code{1}.
    #' @return \code{TRUE} in case the operation is successful.
    #' @examples
    #' \dontrun{
    #' con$select_folder(name = "INBOX")
    #' con$list_flags()
    #' }
    list_flags = function(retries = 1) {
      out <- list_flags_int(self, retries)
      return(out)
    },

    ## SEARCH
    ### custom search
    #' @description Execute a custom search
    #' @param request A string directly specifying what to search or
    #'   constructed by a combination of relational-operator-helper-functions \code{\link{OR}}
    #'   and \code{\link{AND}}, and criteria helper functions such as
    #'   \code{\link{before}}, \code{\link{since}}, \code{\link{on}},
    #'   \code{\link{sent_before}}, \code{\link{sent_since}}, \code{\link{sent_on}},
    #'   \code{\link{flag}}, \code{\link{string}}, \code{\link{smaller_than}},
    #'   \code{\link{larger_than}}, \code{\link{younger_than}}, or
    #'   \code{\link{younger_than}}.
    #' @param negate If \code{TRUE}, negates the search and seeks for "NOT SEARCH
    #'   CRITERIA". Default is \code{FALSE}.
    #' @param use_uid Default is \code{FALSE}. In this case, results will be
    #'   presented as message sequence numbers. A message sequence number is a
    #'   message's relative position to the oldest message in a mail folder. It may
    #'   change after deleting or moving messages. If a message is deleted,
    #'   sequence numbers are reordered to fill the gap. If \code{TRUE}, the
    #'   command will be performed using the \code{"UID"} or unique identifier,
    #'   and results are presented as such. UIDs are always the same during the
    #'   life cycle of a message in a mail folder.
    #' @param esearch A logical. Default is \code{FALSE}. If the IMAP server has
    #'   \code{ESEARCH} capability, it can be used to optimize search results. It
    #'   will condense the results: instead of writing down the whole sequences of messages'
    #'   ids, such as \code{\{1 2 3 4 5\}}, it will be presented as \code{\{1:5\}},
    #'   which decreases transmission costs. This argument can be used along with
    #'   \code{buffersize} to avoid results stripping. Check if your IMAP server
    #'   supports \code{ESEARCH} with
    #'   \href{#method-list_server_capabilities}{\code{ImapCon$list_server_capabilities()}}.
    #' @param retries Number of attempts to connect and execute the command.
    #'   Default is \code{1}.
    #' @note \href{#method-search}{\code{ImapCon$search()}}: IMAP queries follows
    #'   Polish notation, i.e. operators such as \code{OR} come before arguments,
    #'   e.g. "OR argument1 argument2". Therefore, the relational-operator-helper-functions
    #'   in this package should be used like the following examples:
    #'   \code{OR(before("17-Apr-2015"), string("FROM", "John"))}. Even though there
    #'   is no "AND" operator in IMAP, this package adds a helper function
    #'   \code{\link{AND}} to indicate multiples arguments that must be searched
    #'   together, e.g. \code{AND(since("01-Jul-2018"), smaller_than(16000))}.
    #' @return A \code{list} containing the flags (\code{character vector}),
    #'   the permanent flags (\code{character vector}), and an indication if custom
    #'   flags are allowed by the server (\code{logical vector}).
    #' @family custom search
    #' @examples
    #' \dontrun{
    #' con$select_folder(name = "INBOX")
    #' # ex1
    #' con$search(OR(before(date_char = "17-Apr-2015"),
    #'               string(expr = "John", where = "FROM")))
    #'
    #' # ex2
    #' con$search(AND(smaller_than(size = "512000"),
    #'                string(expr = "John", where = "FROM"),
    #'                string(expr = "@ksu.edu", where = "CC")))
    #' }
    search = function(request, negate = FALSE, use_uid = FALSE,
                      esearch = FALSE, retries = 1) {
      out <- search_int(self, request, negate, use_uid, esearch, retries)
      return(out)
    },

    #OBS: helper methods for custom search -- internal helpers are not methods,
    # but functions!

    ### size search
    #' @description Search by size (LARGER)
    #' @param size An integer specifying the size in bytes to be used as the
    #'   search criterion.
    #' @param negate If \code{TRUE}, negates the search and seeks for "NOT SEARCH
    #'   CRITERION". Default is \code{FALSE}.
    #' @param use_uid Default is \code{FALSE}. In this case, results will be
    #'   presented as message sequence numbers. A message sequence number is a
    #'   message's relative position to the oldest message in a mail folder. It may
    #'   change after deleting or moving messages. If a message is deleted,
    #'   sequence numbers are reordered to fill the gap. If \code{TRUE}, the
    #'   command will be performed using the \code{"UID"} or unique identifier,
    #'   and results are presented as such. UIDs are always the same during the
    #'   life cycle of a message in a mail folder.
    #' @param flag An optional argument that sets one or more flags as an additional
    #'   filter to the search. Use \href{#method-list_flags}{\code{ImapCon$list_flags()}}
    #'   to list the flags in a selected mail folder. Default is \code{NULL}.
    #' @param esearch A logical. Default is \code{FALSE}. If the IMAP server has
    #'   \code{ESEARCH} capability, it can be used to optimize search results. It
    #'   will condense the results: instead of writing down the whole sequences of messages'
    #'   ids, such as \code{\{1 2 3 4 5\}}, it will be presented as \code{\{1:5\}},
    #'   which decreases transmission costs. This argument can be used along with
    #'   \code{buffersize} to avoid results stripping. Check if your IMAP server
    #'   supports \code{ESEARCH} with
    #'   \href{#method-list_server_capabilities}{\code{ImapCon$list_server_capabilities()}}.
    #' @param retries Number of attempts to connect and execute the command.
    #'   Default is \code{1}.
    #' @return A \code{numeric vector} containing the message ids.
    #' @family search by size
    #' @examples
    #' \dontrun{
    #' # search for messages with size larger than 512Kb
    #' con$search_larger_than(size = 512000))
    #' }
    search_larger_than = function(size, negate = FALSE, use_uid = FALSE,
                                  flag = NULL, esearch = FALSE, retries = 1) {
      out <- search_larger_than_int(self, size, negate, use_uid, flag, esearch,
                                    retries)
      return(out)
    },

    #' @description Search by size (SMALLER)
    #' @param size An integer specifying the size in bytes to be used as the
    #'   search criterion.
    #' @param negate If \code{TRUE}, negates the search and seeks for "NOT SEARCH
    #'   CRITERION". Default is \code{FALSE}.
    #' @param use_uid Default is \code{FALSE}. In this case, results will be
    #'   presented as message sequence numbers. A message sequence number is a
    #'   message's relative position to the oldest message in a mail folder. It may
    #'   change after deleting or moving messages. If a message is deleted,
    #'   sequence numbers are reordered to fill the gap. If \code{TRUE}, the
    #'   command will be performed using the \code{"UID"} or unique identifier,
    #'   and results are presented as such. UIDs are always the same during the
    #'   life cycle of a message in a mail folder.
    #' @param flag An optional argument that sets one or more flags as an additional
    #'   filter to the search. Use \href{#method-list_flags}{\code{ImapCon$list_flags()}}
    #'   to list the flags in a selected mail folder. Default is \code{NULL}.
    #' @param esearch A logical. Default is \code{FALSE}. If the IMAP server has
    #'   \code{ESEARCH} capability, it can be used to optimize search results. It
    #'   will condense the results: instead of writing down the whole sequences of messages'
    #'   ids, such as \code{\{1 2 3 4 5\}}, it will be presented as \code{\{1:5\}},
    #'   which decreases transmission costs. This argument can be used along with
    #'   \code{buffersize} to avoid results stripping. Check if your IMAP server
    #'   supports \code{ESEARCH} with
    #'   \href{#method-list_server_capabilities}{\code{ImapCon$list_server_capabilities()}}.
    #' @param retries Number of attempts to connect and execute the command.
    #' Default is \code{1}.
    #' @return A \code{numeric vector} containing the message ids.
    #' @family search by size
    #' @examples
    #' \dontrun{
    #' con$select_folder(name = "INBOX")
    #' # search for messages with size smaller than 512Kb
    #' con$search_smaller_than(size = 512000))
    #' }
    search_smaller_than = function(size, negate = FALSE, use_uid = FALSE,
                                  flag = NULL, esearch = FALSE, retries = 1) {
      out <- search_smaller_than_int(self, size, negate, use_uid, flag, esearch,
                                     retries)
      return(out)
    },

    ### search by date
    #' @description Search by internal date (BEFORE)
    #' @param date_char A \code{character string} with format "DD-Mon-YYYY", e.g.
    #'   "01-Apr-2019". We opt not to use \code{Date} or \code{POSIX*} like
    #'   objects, since IMAP servers use this uncommon date format.
    #' @param negate If \code{TRUE}, negates the search and seeks for "NOT SEARCH
    #'   CRITERION". Default is \code{FALSE}.
    #' @param use_uid Default is \code{FALSE}. In this case, results will be
    #'   presented as message sequence numbers. A message sequence number is a
    #'   message's relative position to the oldest message in a mail folder. It may
    #'   change after deleting or moving messages. If a message is deleted,
    #'   sequence numbers are reordered to fill the gap. If \code{TRUE}, the
    #'   command will be performed using the \code{"UID"} or unique identifier,
    #'   and results are presented as such. UIDs are always the same during the
    #'   life cycle of a message in a mail folder.
    #' @param flag An optional argument that sets one or more flags as an additional
    #'   filter to the search. Use \href{#method-list_flags}{\code{ImapCon$list_flags()}}
    #'   to list the flags in a selected mail folder. Default is \code{NULL}.
    #' @param esearch A logical. Default is \code{FALSE}. If the IMAP server has
    #'   \code{ESEARCH} capability, it can be used to optimize search results. It
    #'   will condense the results: instead of writing down the whole sequences of messages'
    #'   ids, such as \code{\{1 2 3 4 5\}}, it will be presented as \code{\{1:5\}},
    #'   which decreases transmission costs. This argument can be used along with
    #'   \code{buffersize} to avoid results stripping. Check if your IMAP server
    #'   supports \code{ESEARCH} with
    #'   \href{#method-list_server_capabilities}{\code{ImapCon$list_server_capabilities()}}.
    #' @param retries Number of attempts to connect and execute the command.
    #'   Default is \code{1}.
    #' @return A \code{numeric vector} containing the message ids.
    #' @family search by date
    #' @examples
    #' \dontrun{
    #' con$select_folder(name = "INBOX")
    #' # search for messages with date before "02-Jan-2020", presenting the
    #' # .. results as unique identifiers (UID)
    #' con$search_before(date = "02-Jan-2020", use_uid = TRUE)
    #' }
    search_before = function(date_char, negate = FALSE, use_uid = FALSE,
                             flag = NULL, esearch = FALSE, retries = 1) {
      out <- search_before_int(self, date_char, negate, use_uid,
                                     flag, esearch, retries)
      return(out)
    },

    #' @description Search by internal date (SINCE)
    #' @param date_char A \code{character string} with format "DD-Mon-YYYY", e.g.
    #'   "01-Apr-2019". We opt not to use \code{Date} or \code{POSIX*} like
    #'   objects, since IMAP servers use this uncommon date format.
    #'   \code{POSIX*} like objects, since IMAP servers use this uncommon date format.
    #'   \code{POSIX*} like, since IMAP servers like this not so common date format.
    #' @param negate If \code{TRUE}, negates the search and seeks for "NOT SEARCH
    #'   CRITERION". Default is \code{FALSE}.
    #' @param use_uid Default is \code{FALSE}. In this case, results will be
    #'   presented as message sequence numbers. A message sequence number is a
    #'   message's relative position to the oldest message in a mail folder. It may
    #'   change after deleting or moving messages. If a message is deleted,
    #'   sequence numbers are reordered to fill the gap. If \code{TRUE}, the
    #'   command will be performed using the \code{"UID"} or unique identifier,
    #'   and results are presented as such. UIDs are always the same during the
    #'   life cycle of a message in a mail folder.
    #' @param flag An optional argument that sets one or more flags as an additional
    #'   filter to the search. Use \href{#method-list_flags}{\code{ImapCon$list_flags()}}
    #'   to list the flags in a selected mail folder. Default is \code{NULL}.
    #' @param esearch A logical. Default is \code{FALSE}. If the IMAP server has
    #'   \code{ESEARCH} capability, it can be used to optimize search results. It
    #'   will condense the results: instead of writing down the whole sequences of messages'
    #'   ids, such as \code{\{1 2 3 4 5\}}, it will be presented as \code{\{1:5\}},
    #'   which decreases transmission costs. This argument can be used along with
    #'   \code{buffersize} to avoid results stripping. Check if your IMAP server
    #'   supports \code{ESEARCH} with
    #'   \href{#method-list_server_capabilities}{\code{ImapCon$list_server_capabilities()}}.
    #' @param retries Number of attempts to connect and execute the command.
    #'   Default is \code{1}.
    #' @return A \code{numeric vector} containing the message ids.
    #' @family search by date
    #' @examples
    #' \dontrun{
    #' con$select_folder(name = "INBOX")
    #' # search for messages with date since "02-Jan-2020", presenting the
    #' # .. results as unique identifiers (UID)
    #' con$search_since(date = "02-Jan-2020", use_uid = TRUE)
    #' }
    search_since = function(date_char, negate = FALSE, use_uid = FALSE,
                            flag = NULL, esearch = FALSE, retries = 1) {
      out <- search_since_int(self, date_char, negate, use_uid,
                              flag, esearch, retries)
      return(out)
    },

    #' @description Search by internal date (ON)
    #' @param date_char A \code{character string} with format "DD-Mon-YYYY", e.g.
    #'   "01-Apr-2019". We opt not to use \code{Date} or \code{POSIX*} like
    #'   objects, since IMAP servers use this uncommon date format.
    #' @param negate If \code{TRUE}, negates the search and seeks for "NOT SEARCH
    #'   CRITERION". Default is \code{FALSE}.
    #' @param use_uid Default is \code{FALSE}. In this case, results will be
    #'   presented as message sequence numbers. A message sequence number is a
    #'   message's relative position to the oldest message in a mail folder. It may
    #'   change after deleting or moving messages. If a message is deleted,
    #'   sequence numbers are reordered to fill the gap. If \code{TRUE}, the
    #'   command will be performed using the \code{"UID"} or unique identifier,
    #'   and results are presented as such. UIDs are always the same during the
    #'   life cycle of a message in a mail folder.
    #' @param flag An optional argument that sets one or more flags as an additional
    #'   filter to the search. Use \href{#method-list_flags}{\code{ImapCon$list_flags()}}
    #'   to list the flags in a selected mail folder. Default is \code{NULL}.
    #' @param esearch A logical. Default is \code{FALSE}. If the IMAP server has
    #'   \code{ESEARCH} capability, it can be used to optimize search results. It
    #'   will condense the results: instead of writing down the whole sequences of messages'
    #'   ids, such as \code{\{1 2 3 4 5\}}, it will be presented as \code{\{1:5\}},
    #'   which decreases transmission costs. This argument can be used along with
    #'   \code{buffersize} to avoid results stripping. Check if your IMAP server
    #'   supports \code{ESEARCH} with
    #'   \href{#method-list_server_capabilities}{\code{ImapCon$list_server_capabilities()}}.
    #' @param retries Number of attempts to connect and execute the command.
    #'   Default is \code{1}.
    #' @return A \code{numeric vector} containing the message ids.
    #' @family search by date
    #' @examples
    #' \dontrun{
    #' con$select_folder(name = "INBOX")
    #' # search for messages received on date "02-Jan-2020", presenting the
    #' #... results as unique identifiers (UID)
    #' con$search_on(date = "02-Jan-2020", use_uid = TRUE)
    #' }
    search_on = function(date_char, negate = FALSE, use_uid = FALSE,
                             flag = NULL, esearch = FALSE, retries = 1) {
      out <- search_on_int(self, date_char, negate, use_uid,
                               flag, esearch, retries)
      return(out)
    },

    #' @description Search by internal date (Period)
    #' @param since_date_char A \code{character string} with format "DD-Mon-YYYY", e.g.
    #'   "01-Apr-2019". We opt not to use \code{Date} or \code{POSIX*} like
    #'   objects, since IMAP servers use this uncommon date format.
    #' @param before_date_char A \code{character string} with format "DD-Mon-YYYY", e.g.
    #'   "01-Apr-2019". We opt not to use \code{Date} or \code{POSIX*} like
    #'   objects, since IMAP servers use this uncommon date format.
    #' @param negate If \code{TRUE}, negates the search and seeks for "NOT SEARCH
    #'   CRITERION". Default is \code{FALSE}.
    #' @param use_uid Default is \code{FALSE}. In this case, results will be
    #'   presented as message sequence numbers. A message sequence number is a
    #'   message's relative position to the oldest message in a mail folder. It may
    #'   change after deleting or moving messages. If a message is deleted,
    #'   sequence numbers are reordered to fill the gap. If \code{TRUE}, the
    #'   command will be performed using the \code{"UID"} or unique identifier,
    #'   and results are presented as such. UIDs are always the same during the
    #'   life cycle of a message in a mail folder.
    #' @param flag An optional argument that sets one or more flags as an additional
    #'   filter to the search. Use \href{#method-list_flags}{\code{ImapCon$list_flags()}}
    #'   to list the flags in a selected mail folder. Default is \code{NULL}.
    #' @param esearch A logical. Default is \code{FALSE}. If the IMAP server has
    #'   \code{ESEARCH} capability, it can be used to optimize search results. It
    #'   will condense the results: instead of writing down the whole sequences of messages'
    #'   ids, such as \code{\{1 2 3 4 5\}}, it will be presented as \code{\{1:5\}},
    #'   which decreases transmission costs. This argument can be used along with
    #'   \code{buffersize} to avoid results stripping. Check if your IMAP server
    #'   supports \code{ESEARCH} with
    #'   \href{#method-list_server_capabilities}{\code{ImapCon$list_server_capabilities()}}.
    #' @param retries Number of attempts to connect and execute the command.
    #'   Default is \code{1}.
    #' @return A \code{numeric vector} containing the message ids.
    #' @family search by date
    #' @examples
    #' \dontrun{
    #' con$select_folder(name = "INBOX")
    #' # search for all messages in the mail folder, EXCEPT (negate = TRUE) by
    #' #... those received between the dates "02-Jan-2020" and "22-Mar-2020"
    #' con$search_period(since_date_char = "02-Jan-2020",
    #'                   before_date_char = "22-Mar-2020",
    #'                   negate = TRUE))
    #' }
    search_period = function(since_date_char, before_date_char, negate = FALSE,
                             use_uid = FALSE, flag = NULL, esearch = FALSE,
                             retries = 1) {
      out <- search_period_int(self, since_date_char, before_date_char, negate,
                               use_uid, flag, esearch, retries)
      return(out)
    },

    #' @description Search by origination date  (RFC 2822 Header - SENT BEFORE)
    #' @param date_char A \code{character string} with format "DD-Mon-YYYY", e.g.
    #'   "01-Apr-2019". We opt not to use \code{Date} or \code{POSIX*} like
    #'   objects, since IMAP servers use this uncommon date format.
    #' @param negate If \code{TRUE}, negates the search and seeks for "NOT SEARCH
    #'   CRITERION". Default is \code{FALSE}.
    #' @param use_uid Default is \code{FALSE}. In this case, results will be
    #'   presented as message sequence numbers. A message sequence number is a
    #'   message's relative position to the oldest message in a mail folder. It may
    #'   change after deleting or moving messages. If a message is deleted,
    #'   sequence numbers are reordered to fill the gap. If \code{TRUE}, the
    #'   command will be performed using the \code{"UID"} or unique identifier,
    #'   and results are presented as such. UIDs are always the same during the
    #'   life cycle of a message in a mail folder.
    #' @param flag An optional argument that sets one or more flags as an additional
    #'   filter to the search. Use \href{#method-list_flags}{\code{ImapCon$list_flags()}}
    #'   to list the flags in a selected mail folder. Default is \code{NULL}.
    #' @param esearch A logical. Default is \code{FALSE}. If the IMAP server has
    #'   \code{ESEARCH} capability, it can be used to optimize search results. It
    #'   will condense the results: instead of writing down the whole sequences of messages'
    #'   ids, such as \code{\{1 2 3 4 5\}}, it will be presented as \code{\{1:5\}},
    #'   which decreases transmission costs. This argument can be used along with
    #'   \code{buffersize} to avoid results stripping. Check if your IMAP server
    #'   supports \code{ESEARCH} with
    #'   \href{#method-list_server_capabilities}{\code{ImapCon$list_server_capabilities()}}.
    #' @param retries Number of attempts to connect and execute the command.
    #'   Default is \code{1}.
    #' @note \href{#method-sent_before}{\code{ImapCon$sent_before()}}: Search
    #'   operations that use the origination/RFC-2822 Header date
    #'   tend to be "slower" than those that use the internal date. Although the
    #'   overhead is minimum, the difference is due to the fact that the internal date
    #'   is kept on a database, while the origination date has to be retrieved from
    #'   inside the message. Therefore, the server needs to access each message when
    #'   executing this type of search. Despite this fact, both dates tend to be the
    #'   same.
    #' @return A \code{numeric vector} containing the message ids.
    #' @family search by date
    #' @examples
    #' \dontrun{
    #' # search for messages with date before "02-Jan-2020", presenting the
    #' # .. results as unique identifiers (UID)
    #' con$search_sent_before(date = "02-Jan-2020", use_uid = TRUE)
    #' }
    search_sent_before = function(date_char, negate = FALSE, use_uid = FALSE,
                                  flag = NULL, esearch = FALSE, retries = 1) {
      out <- search_sent_before_int(self, date_char, negate, use_uid,
                               flag, esearch, retries)
      return(out)
    },

    #' @description Search by origination date (RFC 2822 Header - SENT SINCE)
    #' @param date_char A \code{character string} with format "DD-Mon-YYYY", e.g.
    #'   "01-Apr-2019". We opt not to use \code{Date} or \code{POSIX*} like
    #'   objects, since IMAP servers use this uncommon date format.
    #' @param negate If \code{TRUE}, negates the search and seeks for "NOT SEARCH
    #'   CRITERION". Default is \code{FALSE}.
    #' @param use_uid Default is \code{FALSE}. In this case, results will be
    #'   presented as message sequence numbers. A message sequence number is a
    #'   message's relative position to the oldest message in a mail folder. It may
    #'   change after deleting or moving messages. If a message is deleted,
    #'   sequence numbers are reordered to fill the gap. If \code{TRUE}, the
    #'   command will be performed using the \code{"UID"} or unique identifier,
    #'   and results are presented as such. UIDs are always the same during the
    #'   life cycle of a message in a mail folder.
    #' @param flag An optional argument that sets one or more flags as an additional
    #'   filter to the search. Use \href{#method-list_flags}{\code{ImapCon$list_flags()}}
    #'   to list the flags in a selected mail folder. Default is \code{NULL}.
    #' @param esearch A logical. Default is \code{FALSE}. If the IMAP server has
    #'   \code{ESEARCH} capability, it can be used to optimize search results. It
    #'   will condense the results: instead of writing down the whole sequences of messages'
    #'   ids, such as \code{\{1 2 3 4 5\}}, it will be presented as \code{\{1:5\}},
    #'   which decreases transmission costs. This argument can be used along with
    #'   \code{buffersize} to avoid results stripping. Check if your IMAP server
    #'   supports \code{ESEARCH} with
    #'   \href{#method-list_server_capabilities}{\code{ImapCon$list_server_capabilities()}}.
    #' @param retries Number of attempts to connect and execute the command.
    #'   Default is \code{1}.
    #' @note \href{#method-search_sent_since}{\code{ImapCon$search_sent_since()}}: Search
    #'   operations that use the origination/RFC-2822 Header date
    #'   tend to be "slower" than those that use the internal date. Although the
    #'   overhead is minimum, the difference is due to the fact that the internal date
    #'   is kept on a database, while the origination date has to be retrieved from
    #'   inside the message. Therefore, the server needs to access each message when
    #'   executing this type of search. Despite this fact, both dates tend to be the
    #'   same.
    #' @return A \code{numeric vector} containing the message ids.
    #' @family search by date
    #' @examples
    #' \dontrun{
    #' # search for messages with date before "02-Jan-2020", presenting the
    #' # .. results as unique identifiers (UID)
    #' con$search_sent_since(date = "02-Jan-2020", use_uid = TRUE)
    #' }
    search_sent_since = function(date_char, negate = FALSE, use_uid = FALSE,
                                 flag = NULL, esearch = FALSE, retries = 1) {
      out <- search_sent_since_int(self, date_char, negate, use_uid,
                                   flag, esearch, retries)
      return(out)
    },

    #' @description Search by origination date (RFC 2822 Header - SENT ON)
    #' @param date_char A \code{character string} with format "DD-Mon-YYYY", e.g.
    #'   "01-Apr-2019". We opt not to use \code{Date} or \code{POSIX*} like
    #'   objects, since IMAP servers use this uncommon date format.
    #' @param negate If \code{TRUE}, negates the search and seeks for "NOT SEARCH
    #'   CRITERION". Default is \code{FALSE}.
    #' @param use_uid Default is \code{FALSE}. In this case, results will be
    #'   presented as message sequence numbers. A message sequence number is a
    #'   message's relative position to the oldest message in a mail folder. It may
    #'   change after deleting or moving messages. If a message is deleted,
    #'   sequence numbers are reordered to fill the gap. If \code{TRUE}, the
    #'   command will be performed using the \code{"UID"} or unique identifier,
    #'   and results are presented as such. UIDs are always the same during the
    #'   life cycle of a message in a mail folder.
    #' @param flag An optional argument that sets one or more flags as an additional
    #'   filter to the search. Use \href{#method-list_flags}{\code{ImapCon$list_flags()}}
    #'   to list the flags in a selected mail folder. Default is \code{NULL}.
    #' @param esearch A logical. Default is \code{FALSE}. If the IMAP server has
    #'   \code{ESEARCH} capability, it can be used to optimize search results. It
    #'   will condense the results: instead of writing down the whole sequences of messages'
    #'   ids, such as \code{\{1 2 3 4 5\}}, it will be presented as \code{\{1:5\}},
    #'   which decreases transmission costs. This argument can be used along with
    #'   \code{buffersize} to avoid results stripping. Check if your IMAP server
    #'   supports \code{ESEARCH} with
    #'   \href{#method-list_server_capabilities}{\code{ImapCon$list_server_capabilities()}}.
    #' @param retries Number of attempts to connect and execute the command.
    #'   Default is \code{1}.
    #' @note \href{#method-search_sent_on}{\code{ImapCon$search_sent_on()}}: Search
    #'   operations that use the origination/RFC-2822 Header date
    #'   tend to be "slower" than those that use the internal date. Although the
    #'   overhead is minimum, the difference is due to the fact that the internal date
    #'   is kept on a database, while the origination date has to be retrieved from
    #'   inside the message. Therefore, the server needs to access each message when
    #'   executing this type of search. Despite this fact, both dates tend to be the
    #'   same.
    #' @return A \code{numeric vector} containing the message ids.
    #' @family search by size
    #' @examples
    #' \dontrun{
    #' con$select_folder(name = "INBOX")
    #' # search for messages received on date "02-Jan-2020", presenting the
    #' #... results as unique identifiers (UID)
    #' con$search_sent_on(date = "02-Jan-2020", use_uid = TRUE)
    #' }
    search_sent_on = function(date_char, negate = FALSE, use_uid = FALSE,
                              flag = NULL, esearch = FALSE, retries = 1) {
      out <- search_sent_on_int(self, date_char, negate, use_uid,
                                flag, esearch, retries)
      return(out)
    },

    #' @description Search by origination date (RFC 2822 Header - SENT Period)
    #' @param since_date_char A \code{character string} with format "DD-Mon-YYYY", e.g.
    #'   "01-Apr-2019". We opt not to use \code{Date} or \code{POSIX*} like
    #'   objects, since IMAP servers use this uncommon date format.
    #' @param before_date_char A \code{character string} with format "DD-Mon-YYYY", e.g.
    #'   "01-Apr-2019". We opt not to use \code{Date} or \code{POSIX*} like
    #'   objects, since IMAP servers use this uncommon date format.
    #' @param negate If \code{TRUE}, negates the search and seeks for "NOT SEARCH
    #'   CRITERION". Default is \code{FALSE}.
    #' @param use_uid Default is \code{FALSE}. In this case, results will be
    #'   presented as message sequence numbers. A message sequence number is a
    #'   message's relative position to the oldest message in a mail folder. It may
    #'   change after deleting or moving messages. If a message is deleted,
    #'   sequence numbers are reordered to fill the gap. If \code{TRUE}, the
    #'   command will be performed using the \code{"UID"} or unique identifier,
    #'   and results are presented as such. UIDs are always the same during the
    #'   life cycle of a message in a mail folder.
    #' @param flag An optional argument that sets one or more flags as an additional
    #'   filter to the search. Use \href{#method-list_flags}{\code{ImapCon$list_flags()}}
    #'   to list the flags in a selected mail folder. Default is \code{NULL}.
    #' @param esearch A logical. Default is \code{FALSE}. If the IMAP server has
    #'   \code{ESEARCH} capability, it can be used to optimize search results. It
    #'   will condense the results: instead of writing down the whole sequences of messages'
    #'   ids, such as \code{\{1 2 3 4 5\}}, it will be presented as \code{\{1:5\}},
    #'   which decreases transmission costs. This argument can be used along with
    #'   \code{buffersize} to avoid results stripping. Check if your IMAP server
    #'   supports \code{ESEARCH} with
    #'   \href{#method-list_server_capabilities}{\code{ImapCon$list_server_capabilities()}}.
    #' @param retries Number of attempts to connect and execute the command.
    #'   Default is \code{1}.
    #' @note \href{#method-search_sent_period}{\code{ImapCon$search_sent_period()}}: Search
    #'   operations that use the origination/RFC-2822 Header date
    #'   tend to be "slower" than those that use the internal date. Although the
    #'   overhead is minimum, the difference is due to the fact that the internal date
    #'   is kept on a database, while the origination date has to be retrieved from
    #'   inside the message. Therefore, the server needs to access each message when
    #'   executing this type of search. Despite this fact, both dates tend to be the
    #'   same.
    #' @return A \code{numeric vector} containing the message ids.
    #' @family search by date
    #' @examples
    #' \dontrun{
    #' con$select_folder(name = "INBOX")
    #' # search for all messages in the mail folder, EXCEPT (negate = TRUE) by
    #' #... those received between the dates "02-Jan-2020" and "22-Mar-2020"
    #' con$search_sent_period(since_date_char = "02-Jan-2020",
    #'                   before_date_char = "22-Mar-2020",
    #'                   negate = TRUE))
    #' }
    search_sent_period = function(since_date_char, before_date_char, negate = FALSE,
                                  use_uid = FALSE, flag = NULL, esearch = FALSE,
                                  retries = 1) {
      out <- search_sent_period_int(self, since_date_char, before_date_char,
                                    negate, use_uid, flag, esearch, retries)
      return(out)
    },

    ### OTHER SEARCH

    ### flag
    #' @description Search by flag(s)
    #' @param name A string containing one or more flags to search for. Use
    #'   \href{#method-list_flags}{\code{ImapCon$list_flags()}} to list the flags
    #'   in a selected mail folder.
    #' @param negate If \code{TRUE}, negates the search and seeks for "NOT SEARCH
    #'   CRITERION". Default is \code{FALSE}.
    #' @param use_uid Default is \code{FALSE}. In this case, results will be
    #'   presented as message sequence numbers. A message sequence number is a
    #'   message's relative position to the oldest message in a mail folder. It may
    #'   change after deleting or moving messages. If a message is deleted,
    #'   sequence numbers are reordered to fill the gap. If \code{TRUE}, the
    #'   command will be performed using the \code{"UID"} or unique identifier,
    #'   and results are presented as such. UIDs are always the same during the
    #'   life cycle of a message in a mail folder.
    #' @param esearch A logical. Default is \code{FALSE}. If the IMAP server has
    #'   \code{ESEARCH} capability, it can be used to optimize search results. It
    #'   will condense the results: instead of writing down the whole sequences of messages'
    #'   ids, such as \code{\{1 2 3 4 5\}}, it will be presented as \code{\{1:5\}},
    #'   which decreases transmission costs. This argument can be used along with
    #'   \code{buffersize} to avoid results stripping. Check if your IMAP server
    #'   supports \code{ESEARCH} with
    #'   \href{#method-list_server_capabilities}{\code{ImapCon$list_server_capabilities()}}.
    #' @param retries Number of attempts to connect and execute the command.
    #'   Default is \code{1}.
    #' @return A \code{numeric vector} containing the message ids.
    #' @family search by flag
    #' @examples
    #' \dontrun{
    #' con$select_folder(name = "INBOX")
    #' # search for all messages in the mail folder that are marked as "SEEN" AND
    #' #.. "ANSWERED"
    #' con$search_flag(name = c("SEEN", "ANSWERED"))
    #' }
    search_flag = function(name, negate = FALSE, use_uid = FALSE, esearch = FALSE,
                           retries = 1) {
      out <- search_flag_int(self, name, negate, use_uid, esearch, retries)
      return(out)
    },

    ### WITHIN

    #' @description Search WITHIN a specific time (OLDER)
    #' @param seconds An integer specifying the number of seconds to be used as
    #'   the search criterion.
    #' @param negate If \code{TRUE}, negates the search and seeks for "NOT SEARCH
    #'   CRITERION". Default is \code{FALSE}.
    #' @param use_uid Default is \code{FALSE}. In this case, results will be
    #'   presented as message sequence numbers. A message sequence number is a
    #'   message's relative position to the oldest message in a mail folder. It may
    #'   change after deleting or moving messages. If a message is deleted,
    #'   sequence numbers are reordered to fill the gap. If \code{TRUE}, the
    #'   command will be performed using the \code{"UID"} or unique identifier,
    #'   and results are presented as such. UIDs are always the same during the
    #'   life cycle of a message in a mail folder.
    #' @param flag An optional argument that sets one or more flags as an additional
    #'   filter to the search. Use \href{#method-list_flags}{\code{ImapCon$list_flags()}}
    #'   to list the flags in a selected mail folder. Default is \code{NULL}.
    #' @param esearch A logical. Default is \code{FALSE}. If the IMAP server has
    #'   \code{ESEARCH} capability, it can be used to optimize search results. It
    #'   will condense the results: instead of writing down the whole sequences of messages'
    #'   ids, such as \code{\{1 2 3 4 5\}}, it will be presented as \code{\{1:5\}},
    #'   which decreases transmission costs. This argument can be used along with
    #'   \code{buffersize} to avoid results stripping. Check if your IMAP server
    #'   supports \code{ESEARCH} with
    #'   \href{#method-list_server_capabilities}{\code{ImapCon$list_server_capabilities()}}.
    #' @param retries Number of attempts to connect and execute the command.
    #'   Default is \code{1}.
    #' @note \href{#method-search_older_than}{\code{ImapCon$search_older_than()}}:
    #'   To be able to use this functionality, the server must support the
    #'   \code{WITHIN} capability. You can check it by running
    #'   \href{#method-list_server_capabilities}{\code{ImapCon$list_server_capabilities()}}.
    #' @return A \code{numeric vector} containing the message ids.
    #' @family search within
    #' @examples
    #' \dontrun{
    #' con$select_folder(name = "INBOX")
    #' # search for all messages received in the last hour (not older than 3600 seconds)
    #' con$search_older_than(seconds = 3600, negate = TRUE)
    #' }
    search_older_than = function(seconds, negate = FALSE, use_uid = FALSE,
                                 flag = NULL, esearch = FALSE, retries = 1) {
      out <- search_older_than_int(self, seconds, negate, use_uid, flag,
                                   esearch, retries)
      return(out)
    },

    #' @description Search WITHIN a specific time (YOUNGER)
    #' @param seconds An integer specifying the number of seconds to be used as
    #'   the search criterion.
    #' @param negate If \code{TRUE}, negates the search and seeks for "NOT SEARCH
    #'   CRITERION". Default is \code{FALSE}.
    #' @param use_uid Default is \code{FALSE}. In this case, results will be
    #'   presented as message sequence numbers. A message sequence number is a
    #'   message's relative position to the oldest message in a mail folder. It may
    #'   change after deleting or moving messages. If a message is deleted,
    #'   sequence numbers are reordered to fill the gap. If \code{TRUE}, the
    #'   command will be performed using the \code{"UID"} or unique identifier,
    #'   and results are presented as such. UIDs are always the same during the
    #'   life cycle of a message in a mail folder.
    #' @param flag An optional argument that sets one or more flags as an additional
    #'   filter to the search. Use \href{#method-list_flags}{\code{ImapCon$list_flags()}}
    #'   to list the flags in a selected mail folder. Default is \code{NULL}.
    #' @param esearch A logical. Default is \code{FALSE}. If the IMAP server has
    #'   \code{ESEARCH} capability, it can be used to optimize search results. It
    #'   will condense the results: instead of writing down the whole sequences of messages'
    #'   ids, such as \code{\{1 2 3 4 5\}}, it will be presented as \code{\{1:5\}},
    #'   which decreases transmission costs. This argument can be used along with
    #'   \code{buffersize} to avoid results stripping. Check if your IMAP server
    #'   supports \code{ESEARCH} with
    #'   \href{#method-list_server_capabilities}{\code{ImapCon$list_server_capabilities()}}.
    #' @param retries Number of attempts to connect and execute the command.
    #'   Default is \code{1}.
    #' @note \href{#method-search_older_than}{\code{ImapCon$search_older_than()}}:
    #'   To be able to use this functionality, the server must support the
    #'   \code{WITHIN} capability. You can check it by running
    #'   \href{#method-list_server_capabilities}{\code{ImapCon$list_server_capabilities()}}.
    #' @return A \code{numeric vector} containing the message ids.
    #' @family search within
    #' @examples
    #' \dontrun{
    #' con$select_folder(name = "INBOX")
    #' # search for all messages received in the last hour (younger than 3600 seconds)
    #' con$search_younger_than(seconds = 3600)
    #' }
    search_younger_than = function(seconds, negate = FALSE, use_uid = FALSE,
                                   flag = NULL, esearch = FALSE, retries = 1) {
      out <- search_younger_than_int(self, seconds, negate, use_uid, flag,
                                   esearch, retries)
      return(out)
    },


    #' @description Search by string or expression
    #' @param expr A character string specifying the word or expression to search
    #'   for in messages.
    #' @param where A mandatory character string specifying in which
    #'   message's Section or Header Field to search for the provided string.
    #' @param negate If \code{TRUE}, negates the search and seeks for "NOT SEARCH
    #'   CRITERION". Default is \code{FALSE}.
    #' @param use_uid Default is \code{FALSE}. In this case, results will be
    #'   presented as message sequence numbers. A message sequence number is a
    #'   message's relative position to the oldest message in a mail folder. It may
    #'   change after deleting or moving messages. If a message is deleted,
    #'   sequence numbers are reordered to fill the gap. If \code{TRUE}, the
    #'   command will be performed using the \code{"UID"} or unique identifier,
    #'   and results are presented as such. UIDs are always the same during the
    #'   life cycle of a message in a mail folder.
    #' @param flag An optional argument that sets one or more flags as an additional
    #'   filter to the search. Use \href{#method-list_flags}{\code{ImapCon$list_flags()}}
    #'   to list the flags in a selected mail folder. Default is \code{NULL}.
    #' @param esearch A logical. Default is \code{FALSE}. If the IMAP server has
    #'   \code{ESEARCH} capability, it can be used to optimize search results. It
    #'   will condense the results: instead of writing down the whole sequences of messages'
    #'   ids, such as \code{\{1 2 3 4 5\}}, it will be presented as \code{\{1:5\}},
    #'   which decreases transmission costs. This argument can be used along with
    #'   \code{buffersize} to avoid results stripping. Check if your IMAP server
    #'   supports \code{ESEARCH} with
    #'   \href{#method-list_server_capabilities}{\code{ImapCon$list_server_capabilities()}}.
    #' @param retries Number of attempts to connect and execute the command.
    #'   Default is \code{1}.
    #' @note \href{#method-search_string}{\code{ImapCon$search_string()}}: Using
    #'   \code{where = "TEXT"}, may produce unexpected results since it
    #'   will perform the search on raw data, i.e. the searched expression may be
    #'   truncated by special formating characters such as \code{\\r\\n} for example.
    #'   It is recommended to perform this type of search using \code{where = "BODY"},
    #'   instead of \code{"TEXT"} (\cite{Heinlein, P. and Hartleben, P. (2008)}).
    #' @references \href{#method-search_string}{\code{ImapCon$search_string()}}:
    #'   Heinlein, P. and Hartleben, P. (2008). The Book of IMAP: Building a
    #'   Mail Server with Courier and Cyrus. No Starch Press. ISBN 978-1-59327-177-0.
    #' @return A \code{numeric vector} containing the message ids.
    #' @family search by date
    #' @examples
    #' \dontrun{
    #' con$select_folder(name = "INBOX")
    #' # search for all messages received in the last hour (younger than 3600 seconds)
    #' con$search_string(expr = "@k-state.edu", where = "FROM")
    #' }
    search_string = function(expr, where, negate = FALSE, use_uid = FALSE,
                             flag = NULL, esearch = FALSE, retries = 1) {
      out <- search_string_int(self, expr, where, negate, use_uid, flag, esearch,
                               retries)
      return(out)
    },

    ## FETCH

    #' @description Fetch message body (message's full content)
    #' @param msg_id A \code{numeric vector} containing one or more message ids.
    #' @param use_uid Default is \code{FALSE}. In this case, the operation will
    #'   be performed using message sequence numbers. A message sequence number
    #'   is a message's relative position to the oldest message in a mail folder.
    #'   It may change after deleting or moving messages. If a message is deleted,
    #'   sequence numbers are reordered to fill the gap. If \code{TRUE}, the
    #'   command will be performed using the \code{"UID"} or unique identifier.
    #'   UIDs are always the same during the life cycle of a message in a mail folder.
    #' @param mime_level An \code{integer} specifying MIME multipart to fetch from
    #'   the message's body. Default is \code{NULL}, which retrieves the full body content.
    #' @param peek If \code{TRUE}, it does not mark messages as "read" after
    #'   fetching. Default is \code{TRUE}.
    #' @param partial \code{NULL} or a character string with format
    #'   "startchar.endchar" indicating the size (in characters) of a message slice
    #'   to fetch. Default is \code{NULL}, which will fetch the full specified content.
    #' @param write_to_disk If \code{TRUE}, writes the fetched content of each message
    #'   to a text file in a local folder inside the working directory, also
    #'   returning the results with \code{invisible()}. Default is \code{FALSE}.
    #' @param keep_in_mem If \code{TRUE}, keeps a copy of each fetch result while
    #'   the operation is being performed with \code{write_to_disk = TRUE}. Default
    #'   is \code{FALSE}, and it can only be set \code{TRUE} when
    #'   \code{write_to_disk = TRUE}.
    #' @param mute A \code{logical}. It provides a confirmation message if the
    #'   command is successfully executed. It is only effective when \code{write_to_disk = TRUE}
    #'   and \code{keep_in_mem = FALSE}. Default is \code{FALSE}.
    #' @param retries Number of attempts to connect and execute the command. Default
    #'   is \code{1}.
    #' @return A \code{list} with the fetch contents or a logical if
    #'   \code{write_to_disk = TRUE} and \code{keep_in_mem = FALSE}.
    #' @family fetch
    #' @examples
    #' \dontrun{
    #' con$select_folder(name = "INBOX")
    #' # do a search and fetch the results (saving to disk) using the pipe
    #' con$search_string(expr = "@k-state.edu", where = "FROM") %>%
    #'   con$fetch_body(write_to_disk = TRUE, keep_in_mem = FALSE)
    #'
    #' # or using a traditional approach
    #' res <- con$search_string(expr = "@k-state.edu", where = "FROM")
    #'
    #' con$fetch_body(msg = res, write_to_disk = TRUE, keep_in_mem = FALSE)
    #'
    #' }
    fetch_body = function(msg_id, use_uid = FALSE, mime_level = NULL, peek = TRUE,
                          partial = NULL, write_to_disk = FALSE,
                          keep_in_mem = TRUE, mute = FALSE, retries = 1) {
      out <- fetch_body_int(self, msg_id, use_uid, mime_level, peek, partial, write_to_disk,
                            keep_in_mem, mute, retries)

      if (isTRUE(write_to_disk)) {
        invisible(out)
      } else {
        return(out)
      }

    },

    #' @description Fetch message header
    #' @param msg_id A \code{numeric vector} containing one or more message ids.
    #' @param use_uid Default is \code{FALSE}. In this case, the operation will
    #'   be performed using message sequence numbers. A message sequence number
    #'   is a message's relative position to the oldest message in a mail folder.
    #'   It may change after deleting or moving messages. If a message is deleted,
    #'   sequence numbers are reordered to fill the gap. If \code{TRUE}, the
    #'   command will be performed using the \code{"UID"} or unique identifier.
    #'   UIDs are always the same during the life cycle of a message in a mail folder.
    #' @param fields An optional \code{character vector} specifying which field(s)
    #'   will be fetched from the message's header. If none is specified, it will
    #'   fetch the full header.
    #' @param negate_fields If \code{TRUE}, negates the operation and seeks for
    #'   "NOT in the field". Default is \code{FALSE}.
    #' @param peek If \code{TRUE}, it does not mark messages as "read" after
    #'   fetching. Default is \code{TRUE}.
    #' @param partial \code{NULL} or a character string with format
    #'   "startchar.endchar" indicating the size (in characters) of a message slice
    #'   to fetch. Default is \code{NULL}, which will fetch the full specified content.
    #' @param write_to_disk If \code{TRUE}, writes the fetched content of each message
    #'   to a text file in a local folder inside the working directory, also
    #'   returning the results with \code{invisible()}. Default is \code{FALSE}.
    #' @param keep_in_mem If \code{TRUE}, keeps a copy of each fetch result while
    #'   the operation is being performed with \code{write_to_disk = TRUE}. Default
    #'   is \code{FALSE}, and it can only be set \code{TRUE} when
    #'   \code{write_to_disk = TRUE}.
    #' @param mute A \code{logical}. It provides a confirmation message if the
    #'   command is successfully executed. It is only effective when \code{write_to_disk = TRUE}
    #'   and \code{keep_in_mem = FALSE}. Default is \code{FALSE}.
    #' @param retries Number of attempts to connect and execute the command. Default
    #'   is \code{1}.
    #' @return A \code{list} with the fetch contents or a logical if
    #'   \code{write_to_disk = TRUE} and \code{keep_in_mem = FALSE}.
    #' @family fetch
    #' @examples
    #' \dontrun{
    #' con$select_folder(name = "INBOX")
    #' # do a search and fetch the results (also saving to disk) using the pipe
    #' out <- con$search_string(expr = "@k-state.edu", where = "CC") %>%
    #'   con$fetch_header()
    #'
    #' # or using a traditional approach
    #' res <- con$search_string(expr = "@k-state.edu", where = "CC")
    #' out <- con$fetch_header()
    #'
    #' }
    fetch_header = function(msg_id, use_uid = FALSE, fields = NULL,
                            negate_fields = FALSE, peek = TRUE, partial = NULL,
                            write_to_disk = FALSE, keep_in_mem = TRUE,
                            mute = FALSE, retries = 1) {
      out <- fetch_header_int(self, msg_id, use_uid, fields, negate_fields, peek,
                              partial, write_to_disk, keep_in_mem, mute, retries)

      if (isTRUE(write_to_disk)) {
        invisible(out)
      } else {
        return(out)
      }

    },

    #' @description Fetch message metadata
    #' @param msg_id A \code{numeric vector} containing one or more message ids.
    #' @param use_uid Default is \code{FALSE}. In this case, the operation will
    #'   be performed using message sequence numbers. A message sequence number
    #'   is a message's relative position to the oldest message in a mail folder.
    #'   It may change after deleting or moving messages. If a message is deleted,
    #'   sequence numbers are reordered to fill the gap. If \code{TRUE}, the
    #'   command will be performed using the \code{"UID"} or unique identifier.
    #'   UIDs are always the same during the life cycle of a message in a mail folder.
    #' @param attribute An optional \code{character vector} specifying one or more
    #'   attributes of the metadata of a message to fetch. See \link{metadata_options}.
    #' @param peek If \code{TRUE}, it does not mark messages as "read" after
    #'   fetching. Default is \code{TRUE}.
    #' @param partial \code{NULL} or a character string with format
    #'   "startchar.endchar" indicating the size (in characters) of a message slice
    #'   to fetch. Default is \code{NULL}, which will fetch the full specified content.
    #' @param write_to_disk If \code{TRUE}, writes the fetched content of each message
    #'   to a text file in a local folder inside the working directory, also
    #'   returning the results with \code{invisible()}. Default is \code{FALSE}.
    #' @param keep_in_mem If \code{TRUE}, keeps a copy of each fetch result while
    #'   the operation is being performed with \code{write_to_disk = TRUE}. Default
    #'   is \code{FALSE}, and it can only be set \code{TRUE} when
    #'   \code{write_to_disk = TRUE}.
    #' @param mute A \code{logical}. It provides a confirmation message if the
    #'   command is successfully executed. It is only effective when \code{write_to_disk = TRUE}
    #'   and \code{keep_in_mem = FALSE}. Default is \code{FALSE}.
    #' @param retries Number of attempts to connect and execute the command. Default
    #'   is \code{1}.
    #' @return A \code{list} with the fetch contents or a logical if
    #'   \code{write_to_disk = TRUE} and \code{keep_in_mem = FALSE}.
    #' @family fetch
    #' @examples
    #' \dontrun{
    #' con$select_folder(name = "INBOX")
    #' # do a search and fetch the results using the pipe
    #' out <- con$search_string(expr = "@k-state.edu", where = "FROM") %>%
    #'   con$fetch_metadata()
    #'
    #' # or using a traditional approach
    #' res <- con$search_string(expr = "@k-state.edu", where = "FROM")
    #' out <- con$fetch_metadata(msg = res)
    #'
    #' }
    fetch_metadata = function(msg_id, use_uid = FALSE, attribute = NULL,
                              write_to_disk = FALSE, keep_in_mem = TRUE,
                              mute = FALSE, retries = 1) {
      out <- fetch_metadata_int(self, msg_id, use_uid, attribute, write_to_disk,
                                keep_in_mem, mute, retries)

      if (isTRUE(write_to_disk)) {
        invisible(out)
      } else {
        return(out)
      }

    },

    #' @description Fetch message text
    #' @param msg_id A \code{numeric vector} containing one or more message ids.
    #' @param use_uid Default is \code{FALSE}. In this case, the operation will
    #'   be performed using message sequence numbers. A message sequence number
    #'   is a message's relative position to the oldest message in a mail folder.
    #'   It may change after deleting or moving messages. If a message is deleted,
    #'   sequence numbers are reordered to fill the gap. If \code{TRUE}, the
    #'   command will be performed using the \code{"UID"} or unique identifier.
    #'   UIDs are always the same during the life cycle of a message in a mail folder.
    #' @param peek If \code{TRUE}, it does not mark messages as "read" after
    #'   fetching. Default is \code{TRUE}.
    #' @param partial \code{NULL} or a character string with format
    #'   "startchar.endchar" indicating the size (in characters) of a message slice
    #'   to fetch. Default is \code{NULL}, which will fetch the full specified content.
    #' @param write_to_disk If \code{TRUE}, writes the fetched content of each message
    #'   to a text file in a local folder inside the working directory, also
    #'   returning the results with \code{invisible()}. Default is \code{FALSE}.
    #' @param keep_in_mem If \code{TRUE}, keeps a copy of each fetch result while
    #'   the operation is being performed with \code{write_to_disk = TRUE}. Default
    #'   is \code{FALSE}, and it can only be set \code{TRUE} when
    #'   \code{write_to_disk = TRUE}.
    #' @param mute A \code{logical}. It provides a confirmation message if the
    #'   command is successfully executed. It is only effective when \code{write_to_disk = TRUE}
    #'   and \code{keep_in_mem = FALSE}. Default is \code{FALSE}.
    #' @param base64_decode If \code{TRUE}, tries to guess and decode the fetched
    #'   text from base64 format to \code{character}. Default is \code{FALSE}.
    #' @param retries Number of attempts to connect and execute the command. Default
    #'   is \code{1}.
    #' @return A \code{list} with the fetch contents or a logical if
    #'   \code{write_to_disk = TRUE} and \code{keep_in_mem = FALSE}.
    #' @family fetch
    #' @examples
    #' \dontrun{
    #' con$select_folder(name = "INBOX")
    #' # do a search and partially fetch the results using the pipe
    #' # first 200 characters, writing to disk, silence results in the console
    #'con$search_string(expr = "@k-state.edu", where = "FROM") %>%
    #'   con$fetch_text(partial = "0.200",
    #'                  write_to_disk = TRUE,
    #'                  keep_in_mem = FALSE)
    #'
    #' # or using a traditional approach
    #' res <- con$search_string(expr = "@k-state.edu", where = "FROM")
    #' con$fetch_text(msg = res,
    #'                partial = "0.200",
    #'                write_to_disk = TRUE,
    #'                keep_in_mem = FALSE)
    #'
    #' }
    fetch_text = function(msg_id, use_uid = FALSE, peek = TRUE, partial = NULL,
                          write_to_disk = FALSE, keep_in_mem = TRUE, mute = FALSE,
                          base64_decode = FALSE, retries = 1) {
      out <- fetch_text_int(self, msg_id, use_uid, peek, partial, write_to_disk,
                            keep_in_mem, mute, base64_decode, retries)

      if (isTRUE(write_to_disk)) {
        invisible(out)
      } else {
        return(out)
      }

    },

    ## COMPLEMENTARY OPERATIONS

    #' @description Copy message(s) between the selected folder and another one
    #' @param msg_id A \code{numeric vector} containing one or more message ids.
    #' @param use_uid Default is \code{FALSE}. In this case, the operation will
    #'   be performed using message sequence numbers. A message sequence number
    #'   is a message's relative position to the oldest message in a mail folder.
    #'   It may change after deleting or moving messages. If a message is deleted,
    #'   sequence numbers are reordered to fill the gap. If \code{TRUE}, the
    #'   command will be performed using the \code{"UID"} or unique identifier.
    #'   UIDs are always the same during the life cycle of a message in a mail folder.
    #' @param to_folder A \code{character} string specifying the folder to which
    #'   the messages will be copied.
    #' @param reselect A logical. If \code{TRUE}, calls
    #'   \href{#method-select_folder}{\code{ImapCon$select_folder(name = to_folder)}}
    #'   under the hood before returning the output. Default is \code{TRUE}.
    #' @param mute A \code{logical}. If \code{TRUE}, mutes the confirmation message
    #'   when the command is successfully executed. Default is \code{FALSE}.
    #' @param retries Number of attempts to connect and execute the command.
    #'   Default is \code{1}.
    #' @return An invisible \code{numeric vector} containing the message ids.
    #' @family complementary operations
    #' @examples
    #' \dontrun{
    #' con$select_folder(name = "INBOX")
    #' # do a search and copy the results to another folder
    #' con$search_string(expr = "@k-state.edu", where = "FROM") %>%
    #'   con$copy(to_folder = "Sent")
    #'
    #' # or using a traditional approach
    #' res <- con$search_string(expr = "@k-state.edu", where = "FROM")
    #' con$copy(msg = res, to_folder = "Sent")
    #'
    #' }
    copy_msg = function(msg_id, use_uid = FALSE, to_folder, reselect = TRUE,
                        mute = FALSE, retries = 1) {
      out <- copy_msg_int(self, msg_id, use_uid, to_folder, reselect, mute, retries)

      if (!is.null(out$folder)) {
        self$con_params$folder <- out$folder
      }

      invisible(out$msg_id)

    },

    #' @description Move message(s) between the selected folder and another one
    #' @param msg_id A \code{numeric vector} containing one or more message ids.
    #' @param use_uid Default is \code{FALSE}. In this case, the operation will
    #'   be performed using message sequence numbers. A message sequence number
    #'   is a message's relative position to the oldest message in a mail folder.
    #'   It may change after deleting or moving messages. If a message is deleted,
    #'   sequence numbers are reordered to fill the gap. If \code{TRUE}, the
    #'   command will be performed using the \code{"UID"} or unique identifier.
    #'   UIDs are always the same during the life cycle of a message in a mail folder.
    #' @param to_folder A \code{character} string specifying the folder to which
    #'   the messages will be copied.
    #' @param reselect A logical. If \code{TRUE}, calls
    #'   \href{#method-select_folder}{\code{ImapCon$select_folder(name = to_folder)}}
    #'   under the hood before returning the output. Default is \code{TRUE}.
    #' @param mute A \code{logical}. If \code{TRUE}, mutes the confirmation message
    #'   when the command is successfully executed. Default is \code{FALSE}.
    #' @param retries Number of attempts to connect and execute the command.
    #'   Default is \code{1}.
    #' @return An invisible \code{numeric vector} containing the message ids.
    #' @family complementary operations
    #' @examples
    #' \dontrun{
    #' con$select_folder(name = "INBOX")
    #' # do a search and copy the results to another folder
    #' con$search_string(expr = "@k-state.edu", where = "FROM") %>%
    #'   con$move(to_folder = "Sent")
    #'
    #' # or using a traditional approach
    #' res <- con$search_string(expr = "@k-state.edu", where = "FROM")
    #' con$move(msg = res, to_folder = "Sent")
    #'
    #' }
    move_msg = function(msg_id, use_uid = FALSE, to_folder, reselect = TRUE,
                        mute = FALSE, retries = 1) {
      out <- move_msg_int(self, msg_id, use_uid, to_folder, reselect, mute, retries)

      if (!is.null(out$folder)) {
        self$con_params$folder <- out$folder
      }

      invisible(out$msg_id)

    },

    #' @description Count the number of messages with a specific flag(s) in a
    #'   folder (depend on ESEARCH capability)
    #' @param flag A mandatory parameter that specifies one or more flags as a
    #'   filter to the counting operation. Use \href{#method-list_flags}{\code{ImapCon$list_flags()}}
    #'   to list the flags in a selected mail folder.
    #' @param use_uid Default is \code{FALSE}. In this case, results will be
    #'   presented as message sequence numbers. A message sequence number is a
    #'   message's relative position to the oldest message in a mail folder. It may
    #'   change after deleting or moving messages. If a message is deleted,
    #'   sequence numbers are reordered to fill the gap. If \code{TRUE}, the
    #'   command will be performed using the \code{"UID"} or unique identifier,
    #'   and results are presented as such. UIDs are always the same during the
    #'   life cycle of a message in a mail folder.
    #' @param retries Number of attempts to connect and execute the command.
    #'   Default is \code{1}.
    #' @note \href{#method-esearch_count}{\code{ImapCon$esearch_count()}}: This
    #'   operation depends on the \code{ESEARCH} extension.
    #' @return A numeric \code{vector} of length \code{1} containing the number
    #'   of messages in the folder that meet the specified criteria.
    #' @family complementary operations
    #' @examples
    #' \dontrun{
    #' con$select_folder(name = "INBOX")
    #' # count the number of messages marked as "Flagged" and "Answered"
    #' con$esearch_count(flag = c("Flagged", "Answered"))
    #' }
    esearch_count = function(flag, use_uid = FALSE, retries = 1) {
      out <- esearch_count_int(self, flag, use_uid, retries)

      return(out)

    },

    #' @description Delete message(s) in the selected mail folder
    #' @param msg_id A \code{numeric vector} containing one or more message ids.
    #' @param use_uid Default is \code{FALSE}. In this case, the operation will
    #'   be performed using message sequence numbers. A message sequence number
    #'   is a message's relative position to the oldest message in a mail folder.
    #'   It may change after deleting or moving messages. If a message is deleted,
    #'   sequence numbers are reordered to fill the gap. If \code{TRUE}, the
    #'   command will be performed using the \code{"UID"} or unique identifier.
    #'   UIDs are always the same during the life cycle of a message in a mail folder.
    #' @param mute A \code{logical}. If \code{TRUE}, mutes the confirmation message
    #'   when the command is successfully executed. Default is \code{FALSE}.
    #' @param retries Number of attempts to connect and execute the command.
    #'   Default is \code{1}.
    #' @return An invisible \code{numeric vector} containing the message ids.
    #' @family complementary operations
    #' @examples
    #' \dontrun{
    #' con$select_folder(name = "INBOX")
    #' # delete
    #' con$delete_msg(flag = c("Flagged", "Answered"))
    #' }
    delete_msg = function(msg_id, use_uid = FALSE, mute = FALSE, retries = 1) {
      out <- delete_msg_int(self, msg_id, use_uid, mute, retries)

      invisible(out)

    },


    #' @description Permanently removes all or specific messages marked as deleted from the selected folder
    #' @param msg_uid A \code{numeric vector} containing one or more messages UIDs.
    #'   Only UIDs are allowed in this operation (note the "u" in msg_\emph{u}id).
    #' @param mute A \code{logical}. If \code{TRUE}, mutes the confirmation message
    #'   when the command is successfully executed. Default is \code{FALSE}.
    #' @param retries Number of attempts to connect and execute the command.
    #'   Default is \code{1}.
    #' @return \code{TRUE} if the operation is successful.
    #' @family complementary operations
    #' @examples
    #' \dontrun{
    #' con$select_folder(name = "INBOX")
    #' # count the number of messages marked as "Flagged" and "Answered"
    #' con$esearch_count(flag = c("Flagged", "Answered"))
    #' }
    expunge = function(msg_uid = NULL, mute = FALSE, retries = 1) {
      out <- expunge_int(self, msg_uid, mute, retries)

      invisible(out)

    },

    #' @description Search the minimum message id in the selected mail folder
    #'   (depend on ESEARCH capability)
    #' @param flag A mandatory parameter that specifies one or more flags as a
    #'   filter to the searching operation. Use \href{#method-list_flags}{\code{ImapCon$list_flags()}}
    #'   to list the flags in a selected mail folder.
    #' @param use_uid Default is \code{FALSE}. In this case, results will be
    #'   presented as message sequence numbers. A message sequence number is a
    #'   message's relative position to the oldest message in a mail folder. It may
    #'   change after deleting or moving messages. If a message is deleted,
    #'   sequence numbers are reordered to fill the gap. If \code{TRUE}, the
    #'   command will be performed using the \code{"UID"} or unique identifier,
    #'   and results are presented as such. UIDs are always the same during the
    #'   life cycle of a message in a mail folder.
    #' @param retries Number of attempts to connect and execute the command.
    #'   Default is \code{1}.
    #' @note \href{#method-esearch_min_id}{\code{ImapCon$esearch_min_id()}}: This
    #'   operation depends on the \code{ESEARCH} extension.
    #' @return A numeric \code{vector} of length \code{1} containing the minimum
    #'   message id in the folder.
    #' @family complementary operations
    #' @examples
    #' \dontrun{
    #' con$select_folder(name = "INBOX")
    #' # Search the minimum id of messages marked as "Answered"
    #' con$esearch_min_id(flag = "Answered")
    #' }
    esearch_min_id = function(flag, use_uid = FALSE, retries = 1) {
      out <- esearch_min_id_int(self, flag, use_uid, retries)

      return(out)

    },

    #' @description Search the maximum message id in the selected mail folder
    #'   (depend on ESEARCH capability)
    #' @param flag A mandatory parameter that specifies one or more flags as a
    #'   filter to the searching operation. Use \href{#method-list_flags}{\code{ImapCon$list_flags()}}
    #'   to list the flags in a selected mail folder.
    #' @param use_uid Default is \code{FALSE}. In this case, results will be
    #'   presented as message sequence numbers. A message sequence number is a
    #'   message's relative position to the oldest message in a mail folder. It may
    #'   change after deleting or moving messages. If a message is deleted,
    #'   sequence numbers are reordered to fill the gap. If \code{TRUE}, the
    #'   command will be performed using the \code{"UID"} or unique identifier,
    #'   and results are presented as such. UIDs are always the same during the
    #'   life cycle of a message in a mail folder.
    #' @param retries Number of attempts to connect and execute the command.
    #'   Default is \code{1}.
    #' @note \href{#method-esearch_max_id}{\code{ImapCon$esearch_max_id()}}: This
    #'   operation depends on the \code{ESEARCH} extension.
    #' @return A numeric \code{vector} of length \code{1} containing the maximum
    #'   message id in the folder.
    #' @family complementary operations
    #' @examples
    #' \dontrun{
    #' con$select_folder(name = "INBOX")
    #' # Search the minimum id of messages marked as "Seen"
    #' con$esearch_max_id(flag = "Seen")
    #' }
    esearch_max_id = function(flag, use_uid = FALSE, retries = 1) {
      out <- esearch_max_id_int(self, flag, use_uid, retries)

      return(out)

    },

    # FLAG operations

    #' @description Add flags to one or more messages
    #' @param msg_id A \code{numeric vector} containing one or more message ids.
    #' @param use_uid Default is \code{FALSE}. In this case, the operation will
    #'   be performed using message sequence numbers. A message sequence number
    #'   is a message's relative position to the oldest message in a mail folder.
    #'   It may change after deleting or moving messages. If a message is deleted,
    #'   sequence numbers are reordered to fill the gap. If \code{TRUE}, the
    #'   command will be performed using the \code{"UID"} or unique identifier.
    #'   UIDs are always the same during the life cycle of a message in a mail folder.
    #' @param flags_to_set A \code{character vector} containing one or more flag
    #'   names to add to the specified message ids. If the flag to be set is a
    #'   system flag, such as \code{\\SEEN}, \code{\\ANSWERED}, the name should be
    #'   preceded by two backslashes \code{\\}.
    #' @param mute A \code{logical}. If \code{TRUE}, mutes the confirmation message
    #'   when the command is successfully executed. Default is \code{FALSE}.
    #' @param retries Number of attempts to connect and execute the command.
    #'   Default is \code{1}.
    #' @note \href{#method-add_flags}{\code{ImapCon$add_flags()}}: Unlike the
    #'   search operations, the add/replace/delete flags operations
    #'   demand system flag names to be preceded by two backslashes \code{"\\\\"}.
    #' @note \href{#method-add_flags}{\code{ImapCon$add_flags()}}: \code{add_flags},
    #'   \code{remove_flags}, and \code{replace_flags} accept not only flags but
    #'   also keywords (any word not beginning with two backslashes) which are
    #'   custom flags defined by the user.
    #' @return An invisible \code{numeric vector} containing the message ids.
    #' @family complementary operations
    #' @examples
    #' \dontrun{
    #' con$select_folder(name = "INBOX")
    #' # Add the "\\Seen" permanent flag to the messages received in the last hour
    #' con$search_younger_than(seconds = 3600) %>% # depends on the WITHIN extension
    #'   con$add_flags(flags_to_set = "\\Seen")
    #' }
    add_flags = function(msg_id, use_uid = FALSE, flags_to_set, mute = FALSE,
                         retries = 1) {
      out <- add_flags_int(self, msg_id, use_uid, flags_to_set, mute, retries)

      invisible(out)

    },

    #' @description Replace the current flags of one or more messages
    #' @param msg_id A \code{numeric vector} containing one or more message ids.
    #' @param use_uid Default is \code{FALSE}. In this case, the operation will
    #'   be performed using message sequence numbers. A message sequence number
    #'   is a message's relative position to the oldest message in a mail folder.
    #'   It may change after deleting or moving messages. If a message is deleted,
    #'   sequence numbers are reordered to fill the gap. If \code{TRUE}, the
    #'   command will be performed using the \code{"UID"} or unique identifier.
    #'   UIDs are always the same during the life cycle of a message in a mail folder.
    #' @param flags_to_set A \code{character vector} containing one or more flag
    #'   names that will replace the current ones. If the flag to be set is a
    #'   system flag, such as \code{\\SEEN}, \code{\\ANSWERED}, the name should be
    #'   preceded by two backslashes \code{\\}.
    #' @param mute A \code{logical}. If \code{TRUE}, mutes the confirmation message
    #'   when the command is successfully executed. Default is \code{FALSE}.
    #' @param retries Number of attempts to connect and execute the command.
    #'   Default is \code{1}.
    #' @note \href{#method-replace_flags}{\code{ImapCon$replace_flags()}}: Unlike the
    #'   search operations, the add/replace/delete flags operations
    #'   demand system flag names to be preceded by two backslashes \code{"\\\\"}.
    #' @note \href{#method-replace_flags}{\code{ImapCon$replace_flags()}}: \code{add_flags},
    #'   \code{remove_flags}, and \code{replace_flags} accept not only flags but
    #'   also keywords (any word not beginning with two backslashes) which are
    #'   custom flags defined by the user.
    #' @return An invisible \code{numeric vector} containing the message ids.
    #' @family complementary operations
    #' @examples
    #' \dontrun{
    #' con$select_folder(name = "INBOX")
    #' # Replace the current flags of the messages in the search results for the
    #' #.. flags "\\UNSEEN" and "\\Flagged"
    #' con$search_since(date_char = "20-Aug-2020") %>%
    #'   con$replace_flags(flags_to_set = c("\\UNSEEN", "\\Flagged")
    #' }
    replace_flags = function(msg_id, use_uid = FALSE, flags_to_set, mute = FALSE,
                             retries = 1) {
      out <- replace_flags_int(self, msg_id, use_uid, flags_to_set, mute, retries)

      invisible(out)

    },

    #' @description Remove flag(s) of one or more messages
    #' @param msg_id A \code{numeric vector} containing one or more message ids.
    #' @param use_uid Default is \code{FALSE}. In this case, the operation will
    #'   be performed using message sequence numbers. A message sequence number
    #'   is a message's relative position to the oldest message in a mail folder.
    #'   It may change after deleting or moving messages. If a message is deleted,
    #'   sequence numbers are reordered to fill the gap. If \code{TRUE}, the
    #'   command will be performed using the \code{"UID"} or unique identifier.
    #'   UIDs are always the same during the life cycle of a message in a mail folder.
    #' @param flags_to_unset A \code{character vector} containing one or more
    #'   flag names that will be unset (removed). If the flag to be removed is a
    #'   system flag, such as \code{\\SEEN}, \code{\\ANSWERED}, the name should be
    #'   preceded by two backslashes \code{\\}.
    #' @param mute A \code{logical}. If \code{TRUE}, mutes the confirmation message
    #'   when the command is successfully executed. Default is \code{FALSE}.
    #' @param retries Number of attempts to connect and execute the command.
    #'   Default is \code{1}.
    #' @note \href{#method-remove_flags}{\code{ImapCon$remove_flags()}}: Unlike the
    #'   search operations, the add/replace/delete flags operations
    #'   demand system flag names to be preceded by two backslashes \code{"\\\\"}.
    #' @note \href{#method-remove_flags}{\code{ImapCon$remove_flags()}}: \code{add_flags},
    #'   \code{remove_flags}, and \code{replace_flags} accept not only flags but
    #'   also keywords (any word not beginning with two backslashes) which are
    #'   custom flags defined by the user.
    #' @return An invisible \code{numeric vector} containing the message ids.
    #' @family complementary operations
    #' @examples
    #' \dontrun{
    #' con$select_folder(name = "INBOX")
    #' # Remove the the "\\SEEN" flag from the messages in the search result
    #' con$search_since(date_char = "20-Aug-2020") %>%
    #'   con$remove_flags(flags_to_unset = "\\UNSEEN")
    #' }
    remove_flags = function(msg_id, use_uid = FALSE, flags_to_unset, mute = FALSE,
                            retries = 1) {
      out <- remove_flags_int(self, msg_id, use_uid, flags_to_unset, mute, retries)

      invisible(out)

    },

    ## ATTACHMENTS

    #' @description Extract attached file(s) from fetched message(s)
    #' @param msg_list A \code{list} with the body or text content of the messages
    #'   fetched with \href{#method-fetch_body}{\code{ImapCon$fetch_body()}} or
    #'   \href{#method-fetch_text}{\code{ImapCon$fetch_text()}}.
    #' @param content_disposition A \code{string} indicating which type of
    #'   "Content-Disposition" attachments should be retrieved. Default is
    #'   \code{"both"}, which retrieves regular attachments ("Content-Disposition:
    #'   attachment") and  inline attachments ("Content-Disposition: inline").
    #' @param override A \code{logical}. Provides a confirmation message if the
    #'   command is successfully executed. Default is \code{FALSE}.
    #' @param mute A \code{logical}. If \code{TRUE}, mutes the confirmation message
    #'   when the command is successfully executed. Default is \code{FALSE}.
    #' @param as_is If \code{TRUE} then write out attachments without base64
    #'   decoding. Default is \code{FALSE}.
    #' @note \href{#method-get_attachments}{\code{ImapCon$get_attachments()}}:
    #'   This method is to be used after the body or the
    #'   text part of one or more messages were fetched. This makes sense if the
    #'   user is interested in keeping the message content (body or text) besides
    #'   downloading the message attachments. Nonetheless, this is not the
    #'   recommended approach if the user is only interested in downloading the files
    #'   as the previous fetching operation will probably be costly. In this last
    #'   case, the recommendation is to use
    #'   \href{#method-fetch_attachments}{\code{ImapCon$fetch_attachments()}} as
    #'   it will only fetch the attachment part.
    #' @note \href{#method-get_attachments}{\code{ImapCon$get_attachments()}}: All
    #'   attachments will be stored in a folder labeled with the message id
    #'   inside the \code{working directory > servername > foldername}.
    #'   This function currently handles only attachments
    #'   encoded as \code{base64} text. It tries to guess all file extensions while
    #'   decoding the text, but it may not be possible to do so in some circumstances.
    #'   If it happens, you can try to change the file extension directly by renaming
    #'   the file.
    #' @note \href{#method-get_attachments}{\code{ImapCon$get_attachments()}}: The
    #'   "Content-Disposition" header specifies if the multipart electronic
    #'   messages will be presented as a main document with a list of separate
    #'   attachments ("Content-Disposition: attachment") or as a single document
    #'   with the various parts displayed inline. The first requires positive action
    #'   on the part of the recipient (downloading the file, for example) whereas inline
    #'   components are displayed automatically when the message is viewed
    #'   (\cite{Troost, R., Dorner, S., and K. Moore, Ed. (1997)}). You can choose
    #'   to download \code{both}, or only one type of attachment, using the
    #'   argument \code{content_disposition}.
    #' @references \href{#method-get_attachments}{\code{ImapCon$get_attachments()}}:
    #'   Troost, R., Dorner, S., and K. Moore (1997), Communicating
    #'   Presentation Information in Internet Messages: The Content-Disposition
    #'   Header Field, RFC 2183, August 1997, https://www.rfc-editor.org/rfc/rfc2183.
    #' @return \code{TRUE} if the operation is successful. The files are saved
    #' locally.
    #' @family attachments
    #' @examples
    #' \dontrun{
    #' # example 1
    #' con$select_folder(name = "INBOX")
    #' con$search_string(expr = "@gmail", where = "CC") %>%
    #'   con$fetch_text(write_to_disk = TRUE) %>% # saving the message's content as txt files
    #'   con$get_attachments()
    #'
    #' # example 2
    #' res <- con$search_string(expr = "@gmail", where = "CC") %>%
    #' out <- con$fetch_body(msg = res)
    #' con$get_attachments(msg_list = out)
    #' }
    get_attachments = function(msg_list, content_disposition = "both",
                               override = FALSE, mute = FALSE, as_is = FALSE) {
      out <- get_attachments_int(self, msg_list, content_disposition, override,
                                 mute, as_is)

      invisible(out)

    },

    # list_attachments() doesnt need anything from self, so it will be a specific function and
    #... not a method form an R6 class

    # NEW ATTACHMENT-FETCH FUNCTIONS

    #' @description Fetch attachments' list
    #' @param msg_id A \code{numeric vector} containing one or more message ids.
    #' @param use_uid Default is \code{FALSE}. In this case, the operation will
    #'   be performed using message sequence numbers. A message sequence number
    #'   is a message's relative position to the oldest message in a mail folder.
    #'   It may change after deleting or moving messages. If a message is deleted,
    #'   sequence numbers are reordered to fill the gap. If \code{TRUE}, the
    #'   command will be performed using the \code{"UID"} or unique identifier.
    #'   UIDs are always the same during the life cycle of a message in a mail folder.
    #' @param retries Number of attempts to connect and execute the command. Default
    #'   is \code{1}.
    #' @return A \code{list} with the fetch contents.
    #' @family fetch
    #' @examples
    #' \dontrun{
    #' con$select_folder(name = "INBOX")
    #' # do a search and fetch the attachments' list of the messages
    #' out < con$search_string(expr = "@k-state.edu", where = "FROM") %>%
    #'   con$fetch_attachments_list()
    #' out
    #'
    #' # or using a traditional approach
    #' res <- con$search_string(expr = "@k-state.edu", where = "FROM")
    #' out <- con$fetch_attachments_list(msg = res)
    #' out
    #'
    #' }
    fetch_attachments_list = function(msg_id, use_uid = FALSE, retries = 1) {
      out <- fetch_attachments_list_int(self, msg_id, use_uid, retries)

      return(out)

    },

    #' @description Fetch message attachments
    #' @param msg_id A \code{numeric vector} containing one or more message ids.
    #' @param use_uid Default is \code{FALSE}. In this case, the operation will
    #'   be performed using message sequence numbers. A message sequence number
    #'   is a message's relative position to the oldest message in a mail folder.
    #'   It may change after deleting or moving messages. If a message is deleted,
    #'   sequence numbers are reordered to fill the gap. If \code{TRUE}, the
    #'   command will be performed using the \code{"UID"} or unique identifier.
    #'   UIDs are always the same during the life cycle of a message in a mail folder.
    #' @param content_disposition A \code{string} indicating which type of
    #'   "Content-Disposition" attachments should be retrieved. The options are
    #'   \code{both}, \code{attachment}, and \code{inline}. Default is
    #'   \code{"both"}, which retrieves regular attachments ("Content-Disposition:
    #'   attachment") and  inline attachments ("Content-Disposition: inline").
    #' @param override A \code{logical}. Provides a confirmation message if the
    #'   command is successfully executed. Default is \code{FALSE}.
    #' @param mute A \code{logical}. If \code{TRUE}, mutes the confirmation message
    #'   when the command is successfully executed. Default is \code{FALSE}.
    #' @param retries Number of attempts to connect and execute the command. Default
    #'   is \code{1}.
    #' @param as_is If \code{TRUE} then write out attachments without base64
    #'   decoding. Default is \code{FALSE}.
    #' @note \href{#method-fetch_attachments}{\code{ImapCon$fetch_attachments()}}: All
    #'   attachments will be stored in a folder labeled with the message id
    #'   inside the \code{working directory > servername > foldername}.
    #'   This function currently handles only attachments
    #'   encoded as \code{base64} text. It tries to guess all file extensions while
    #'   decoding the text, but it may not be possible to do so in some circumstances.
    #'   If it happens, you can try to change the file extension directly by renaming
    #'   the file.
    #' @note \href{#method-fetch_attachments}{\code{ImapCon$fetch_attachments()}}: The
    #'   "Content-Disposition" header specifies if the multipart electronic
    #'   messages will be presented as a main document with a list of separate
    #'   attachments ("Content-Disposition: attachment") or as a single document
    #'   with the various parts displayed inline. The first requires positive action
    #'   on the part of the recipient (downloading the file, for example) whereas inline
    #'   components are displayed automatically when the message is viewed
    #'   (\cite{Troost, R., Dorner, S., and K. Moore, Ed. (1997)}). You can choose
    #'   to download \code{both}, or only one type of attachment, using the
    #'   argument \code{content_disposition}.
    #' @references \href{#method-fetch_attachments}{\code{ImapCon$fetch_attachments()}}:
    #'   Troost, R., Dorner, S., and K. Moore (1997), Communicating
    #'   Presentation Information in Internet Messages: The Content-Disposition
    #'   Header Field, RFC 2183, DOI 10.17487/RFC2183, August 1997,
    #'   https://www.rfc-editor.org/rfc/rfc2183.
    #' @return A \code{list} with the fetch contents.
    #' @family fetch
    #' @examples
    #' \dontrun{
    #' con$select_folder(name = "INBOX")
    #' # do a search and fetch the attachments' list of the messages
    #' con$search_string(expr = "@k-state.edu", where = "FROM") %>%
    #'   con$fetch_attachments() # the attachments will be downloaded to disk
    #'
    #'
    #' # or using a traditional approach
    #' res <- con$search_string(expr = "@k-state.edu", where = "FROM")
    #' con$fetch_attachments(msg = res)
    #'
    #' }
    fetch_attachments = function(msg_id, use_uid = FALSE, content_disposition = "both",
                                 override = FALSE, mute = FALSE, retries = 1,
                                 as_is = FALSE) {
      out <- fetch_attachments_int(self, msg_id, use_uid, content_disposition,
                                   override, mute, retries, as_is)

      invisible(out)

    }#,

  )
)

