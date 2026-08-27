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
    #' @param oauth_mechanism The SASL mechanism used to send the OAuth 2.0
    #'   token: \code{"XOAUTH2"} (default; Gmail, Yahoo, Microsoft 365) or
    #'   \code{"OAUTHBEARER"} (RFC 7628; Gmail). Ignored when authenticating
    #'   with a password.
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
                          oauth_mechanism = c("XOAUTH2", "OAUTHBEARER"),
                          use_ssl = TRUE,
                          verbose = FALSE,
                          buffersize = 16000,
                          timeout_ms = 0,
                          ...) {

      out <- config_con_handle_and_params(url = url, username = username,
                                   password = password, xoauth2_bearer = xoauth2_bearer,
                                   oauth_mechanism = oauth_mechanism,
                                   use_ssl = use_ssl, verbose = verbose,
                                   buffersize = buffersize, timeout_ms = timeout_ms,
                                   ...)

      # print(out$con_params)

      self$con_params <- out$con_params
      self$con_handle <- out$con_handle
      self$con_debug <- out$con_debug

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

    #' @description Disconnect and release the connection handle. After calling
    #'   this method the connection object can no longer be used to issue
    #'   commands; a new one must be created with \code{\link{configure_imap}}.
    #'   Dropping the handle reference lets 'libcurl' close the underlying
    #'   connection when the handle is garbage-collected.
    #' @return \code{TRUE}, invisibly.
    #' @examples
    #' \dontrun{
    #' con$disconnect()
    #' }
    disconnect = function() {
      self$con_handle <- NULL
      self$con_params$folder <- NA
      invisible(TRUE)
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

    #' @description Enable server extensions for the current session (IMAP
    #'   \code{ENABLE}, RFC 5161). Some extensions, such as \code{CONDSTORE}
    #'   or \code{UTF8=ACCEPT}, only take effect after the client enables
    #'   them. Requires the server \code{ENABLE} capability. The command is
    #'   only accepted before a folder is selected (RFC 5161), and what it
    #'   enables lasts for the current connection; the package handles
    #'   \code{UTF8=ACCEPT} itself for non-ASCII searches.
    #' @param capabilities A \code{character} vector with the names of the
    #'   extensions to enable.
    #' @param retries Number of attempts to connect and execute the command.
    #'   Default is \code{1}.
    #' @return A \code{character} vector with the extensions the server
    #'   confirmed as enabled (possibly empty).
    #' @examples
    #' \dontrun{
    #' con$enable("CONDSTORE")
    #' }
    enable = function(capabilities, retries = 1) {
      out <- enable_int(self, capabilities, retries)
      return(out)
    },

    #' @description Request the server's namespaces (IMAP \code{NAMESPACE}, RFC
    #'   2342): the personal, other users', and shared namespace prefixes and
    #'   their hierarchy delimiters. Requires the server \code{NAMESPACE}
    #'   capability.
    #' @param retries Number of attempts to connect and execute the command.
    #'   Default is \code{1}.
    #' @return A named \code{list} with elements \code{personal},
    #'   \code{other_users} and \code{shared}, each a \code{data.frame} with
    #'   \code{prefix} and \code{delimiter} columns, or \code{NULL} when the
    #'   server returns \code{NIL} for that component.
    #' @examples
    #' \dontrun{
    #' con$namespace()
    #' }
    namespace = function(retries = 1) {
      out <- namespace_int(self, retries)
      return(out)
    },

    #' @description Exchange client/server identification (IMAP \code{ID}, RFC
    #'   2971). Optionally sends the client's id fields and returns the server's
    #'   id. Requires the server \code{ID} capability.
    #' @param fields A named \code{character} vector with the client id fields to
    #'   send, e.g. \code{c(name = "mRpostman", version = "1.2.1")}. If
    #'   \code{NULL} (default), sends \code{ID NIL} (asks for the server id
    #'   without disclosing the client id).
    #' @param retries Number of attempts to connect and execute the command.
    #'   Default is \code{1}.
    #' @return A named \code{character} vector with the server's id fields
    #'   (empty when the server returns \code{NIL}).
    #' @examples
    #' \dontrun{
    #' con$id()
    #' con$id(fields = c(name = "mRpostman", version = "1.2.1"))
    #' }
    id = function(fields = NULL, retries = 1) {
      out <- id_int(self, fields, retries)
      return(out)
    },

    #' @description Get the quota root(s) and quota usage/limits of a mail folder
    #'   (IMAP \code{GETQUOTAROOT}, RFC 2087). Requires the server \code{QUOTA}
    #'   capability.
    #' @param name A \code{character} string with the mail folder name. If no
    #'   name is passed, the command uses the previously selected folder.
    #' @param retries Number of attempts to connect and execute the command.
    #'   Default is \code{1}.
    #' @return A \code{data.frame} with columns \code{quota_root},
    #'   \code{resource}, \code{usage} and \code{limit} (one row per resource;
    #'   \code{STORAGE} is reported by the server in kibibytes).
    #' @examples
    #' \dontrun{
    #' con$get_quota_root(name = "INBOX")
    #' }
    get_quota_root = function(name = NULL, retries = 1) {
      out <- get_quota_root_int(self, name, retries)
      return(out)
    },

    #' @description Get the quota usage/limits of a quota root (IMAP
    #'   \code{GETQUOTA}, RFC 2087). Requires the server \code{QUOTA} capability.
    #' @param quota_root A \code{character} string with the quota root name.
    #'   Default is \code{""} (the default root). Use \code{get_quota_root()} to
    #'   discover the root(s) of a folder.
    #' @param retries Number of attempts to connect and execute the command.
    #'   Default is \code{1}.
    #' @return A \code{data.frame} with columns \code{quota_root},
    #'   \code{resource}, \code{usage} and \code{limit}.
    #' @examples
    #' \dontrun{
    #' con$get_quota(quota_root = "")
    #' }
    get_quota = function(quota_root = "", retries = 1) {
      out <- get_quota_int(self, quota_root, retries)
      return(out)
    },

    #' @description Set the resource limits of a quota root (IMAP
    #'   \code{SETQUOTA}, RFC 2087). Most servers restrict this command to
    #'   administrators. Requires the server \code{QUOTA} capability.
    #' @param quota_root A \code{character} string with the quota root name,
    #'   as returned by \code{get_quota_root()}.
    #' @param storage \code{NULL} or the new \code{STORAGE} limit, in
    #'   kibibytes.
    #' @param message \code{NULL} or the new \code{MESSAGE} limit (number of
    #'   messages). At least one of the two limits must be given.
    #' @param retries Number of attempts to connect and execute the command.
    #'   Default is \code{1}.
    #' @return A \code{data.frame} with the quota as confirmed by the server
    #'   (columns \code{quota_root}, \code{resource}, \code{usage},
    #'   \code{limit}).
    #' @examples
    #' \dontrun{
    #' con$set_quota(quota_root = "User quota", storage = 2 * 1024^2)
    #' }
    set_quota = function(quota_root, storage = NULL, message = NULL,
                         retries = 1) {
      out <- set_quota_int(self, quota_root, storage, message, retries)
      return(out)
    },

    ## METADATA (RFC 5464)
    #' @description Get metadata entries (annotations) of a mail folder or of
    #'   the server (IMAP \code{GETMETADATA}, RFC 5464). Requires the server
    #'   \code{METADATA} (or \code{METADATA-SERVER}) capability.
    #' @param name A \code{character} string with the mail folder name, or
    #'   \code{NULL} for server-level entries.
    #' @param entries A \code{character} vector of entry names, e.g.
    #'   \code{"/private/comment"} or \code{"/shared/vendor/..."}.
    #' @param depth \code{NULL} (default), \code{"0"}, \code{"1"}, or
    #'   \code{"infinity"}: how many levels below each entry to return.
    #' @param max_size \code{NULL} (default) or the maximum size, in bytes, of
    #'   a value to return.
    #' @param retries Number of attempts to connect and execute the command.
    #'   Default is \code{1}.
    #' @return A \code{data.frame} with columns \code{mailbox}, \code{entry},
    #'   and \code{value} (\code{NA} when the entry has no value).
    #' @examples
    #' \dontrun{
    #' con$get_metadata(name = "INBOX", entries = "/private/comment")
    #' con$get_metadata(name = NULL, entries = "/shared/comment")
    #' }
    get_metadata = function(name = NULL, entries, depth = NULL, max_size = NULL,
                            retries = 1) {
      out <- get_metadata_int(self, name, entries, depth, max_size, retries)
      return(out)
    },

    #' @description Set (or remove) metadata entries of a mail folder or of the
    #'   server (IMAP \code{SETMETADATA}, RFC 5464). Requires the server
    #'   \code{METADATA} (or \code{METADATA-SERVER}) capability.
    #' @param name A \code{character} string with the mail folder name, or
    #'   \code{NULL} for server-level entries.
    #' @param entries A named \code{character} vector: the names are the
    #'   entries, the values the new values; \code{NA} removes an entry.
    #'   Values cannot contain line breaks.
    #' @param retries Number of attempts to connect and execute the command.
    #'   Default is \code{1}.
    #' @return \code{TRUE} in case the operation is successful.
    #' @examples
    #' \dontrun{
    #' con$set_metadata(name = "INBOX", entries = c("/private/comment" = "reviewed"))
    #' con$set_metadata(name = "INBOX", entries = c("/private/comment" = NA))
    #' }
    set_metadata = function(name = NULL, entries, retries = 1) {
      invisible(set_metadata_int(self, name, entries, retries))
    },

    ## ACL (RFC 4314)
    #' @description Get the access control list of a mail folder (IMAP
    #'   \code{GETACL}, RFC 4314). Requires the server \code{ACL} capability.
    #' @param name A \code{character} string with the mail folder name. If no
    #'   name is passed, the command uses the previously selected folder.
    #' @param retries Number of attempts to connect and execute the command.
    #'   Default is \code{1}.
    #' @return A \code{data.frame} with columns \code{identifier} (a user
    #'   name, or a group such as \code{anyone}) and \code{rights} (a string
    #'   of right letters, e.g. \code{"lrwstipekxa"}).
    #' @examples
    #' \dontrun{
    #' con$get_acl(name = "INBOX")
    #' }
    get_acl = function(name = NULL, retries = 1) {
      out <- get_acl_int(self, name, retries)
      return(out)
    },

    #' @description Set or modify the rights of an identifier on a mail folder
    #'   (IMAP \code{SETACL}, RFC 4314). Requires the server \code{ACL}
    #'   capability and the \code{a} (administer) right on the folder.
    #' @param name A \code{character} string with the mail folder name. If no
    #'   name is passed, the command uses the previously selected folder.
    #' @param identifier A \code{character} string with the user name (or
    #'   group, e.g. \code{"anyone"}) whose rights are set.
    #' @param rights A \code{character} string of right letters. Without a
    #'   prefix it replaces the current rights (e.g. \code{"lrs"}); prefixed
    #'   with \code{"+"} or \code{"-"} it adds or removes rights (e.g.
    #'   \code{"+w"}, \code{"-d"}).
    #' @param retries Number of attempts to connect and execute the command.
    #'   Default is \code{1}.
    #' @return \code{TRUE} in case the operation is successful.
    #' @examples
    #' \dontrun{
    #' con$set_acl(name = "Shared", identifier = "anyone", rights = "lrs")
    #' con$set_acl(name = "Shared", identifier = "anyone", rights = "+w")
    #' }
    set_acl = function(name = NULL, identifier, rights, retries = 1) {
      invisible(set_acl_int(self, name, identifier, rights, retries))
    },

    #' @description Remove all rights of an identifier on a mail folder (IMAP
    #'   \code{DELETEACL}, RFC 4314). Requires the server \code{ACL}
    #'   capability.
    #' @param name A \code{character} string with the mail folder name. If no
    #'   name is passed, the command uses the previously selected folder.
    #' @param identifier A \code{character} string with the user name (or
    #'   group) whose rights are removed.
    #' @param retries Number of attempts to connect and execute the command.
    #'   Default is \code{1}.
    #' @return \code{TRUE} in case the operation is successful.
    #' @examples
    #' \dontrun{
    #' con$delete_acl(name = "Shared", identifier = "anyone")
    #' }
    delete_acl = function(name = NULL, identifier, retries = 1) {
      invisible(delete_acl_int(self, name, identifier, retries))
    },

    #' @description List the rights that may be granted to an identifier on a
    #'   mail folder (IMAP \code{LISTRIGHTS}, RFC 4314). Requires the server
    #'   \code{ACL} capability.
    #' @param name A \code{character} string with the mail folder name. If no
    #'   name is passed, the command uses the previously selected folder.
    #' @param identifier A \code{character} string with the user name (or
    #'   group).
    #' @param retries Number of attempts to connect and execute the command.
    #'   Default is \code{1}.
    #' @return A \code{list} with \code{required} (the rights the identifier
    #'   always has) and \code{optional} (a \code{character} vector with the
    #'   sets of rights that may be granted).
    #' @examples
    #' \dontrun{
    #' con$list_rights(name = "INBOX", identifier = "anyone")
    #' }
    list_rights = function(name = NULL, identifier, retries = 1) {
      out <- list_rights_int(self, name, identifier, retries)
      return(out)
    },

    #' @description Get the rights of the current user on a mail folder (IMAP
    #'   \code{MYRIGHTS}, RFC 4314). Requires the server \code{ACL}
    #'   capability.
    #' @param name A \code{character} string with the mail folder name. If no
    #'   name is passed, the command uses the previously selected folder.
    #' @param retries Number of attempts to connect and execute the command.
    #'   Default is \code{1}.
    #' @return A \code{character} string of right letters.
    #' @examples
    #' \dontrun{
    #' con$my_rights(name = "INBOX")
    #' }
    my_rights = function(name = NULL, retries = 1) {
      out <- my_rights_int(self, name, retries)
      return(out)
    },

    #' @description Issue a \code{NOOP} command. It does nothing on the server
    #'   other than resetting the inactivity autologout timer, which makes it
    #'   useful as a keep-alive during long idle periods and as a way to keep
    #'   the connection handle alive between operations.
    #' @param retries Number of attempts to connect and execute the command.
    #'   Default is \code{1}.
    #' @return \code{TRUE} in case the operation is successful.
    #' @examples
    #' \dontrun{
    #' con$noop()
    #' }
    noop = function(retries = 1) {
      invisible(noop_int(self, retries))
    },

    #' @description Request a checkpoint of the selected mail folder (IMAP
    #'   \code{CHECK}). The server performs any implementation-dependent
    #'   housekeeping of the mailbox, such as flushing its state to disk.
    #'   The command has no client-observable effect; use \code{noop()} as a
    #'   keep-alive.
    #' @param retries Number of attempts to connect and execute the command.
    #'   Default is \code{1}.
    #' @return \code{TRUE} in case the operation is successful.
    #' @examples
    #' \dontrun{
    #' con$select_folder(name = "INBOX")
    #' con$check()
    #' }
    check = function(retries = 1) {
      invisible(check_int(self, retries))
    },

    ## mailbox operations
    #' @description List mail folders in a mailbox.
    #' @param retries Number of attempts to connect and execute the command.
    #'   Default is \code{1}.
    #' @param detailed A \code{logical}. If \code{TRUE}, issues
    #'   \code{LIST ... RETURN (CHILDREN SUBSCRIBED SPECIAL-USE)} (LIST-EXTENDED,
    #'   RFC 5258) and returns a \code{data.frame} with one row per folder and
    #'   its attributes instead of the root/children list. Requires the server
    #'   \code{LIST-EXTENDED} capability. Default is \code{FALSE}.
    #' @return A \code{list} containing the mail folder names and their inherent
    #'   structure or, with \code{detailed = TRUE}, a \code{data.frame} with
    #'   columns \code{folder}, \code{delimiter}, \code{attributes},
    #'   \code{selectable}, \code{has_children}, \code{subscribed}, and
    #'   \code{special_use}.
    #' @examples
    #' \dontrun{
    #' folders <- con$list_mail_folders()
    #' folders
    #' }
    list_mail_folders = function(retries = 1, detailed = FALSE) {
      out <- list_mail_folders_int(self, retries, detailed = detailed)
      return(out)
    },

    #' @description List the subscribed mail folders in a mailbox (IMAP
    #'   \code{LSUB}). Unlike \code{list_mail_folders()} (which issues
    #'   \code{LIST} and returns every folder), this returns only the folders
    #'   the user is subscribed to.
    #' @param retries Number of attempts to connect and execute the command.
    #'   Default is \code{1}.
    #' @return A \code{list} containing the subscribed mail folder names and
    #'   their inherent structure.
    #' @examples
    #' \dontrun{
    #' subscribed <- con$list_subscribed_folders()
    #' subscribed
    #' }
    list_subscribed_folders = function(retries = 1) {
      out <- list_subscribed_folders_int(self, retries)
      return(out)
    },

    #' @description List the mail folders together with their status counts in
    #'   a single round trip (IMAP \code{LIST ... RETURN (STATUS ...)}, RFC
    #'   5819). Equivalent to \code{list_mail_folders()} followed by
    #'   \code{status()} on every folder, but issued as one command. Requires
    #'   the server \code{LIST-STATUS} capability.
    #' @param items A \code{character} vector with the status data items to
    #'   request. Must be a subset of \code{"MESSAGES"}, \code{"RECENT"},
    #'   \code{"UIDNEXT"}, \code{"UIDVALIDITY"}, and \code{"UNSEEN"}, plus the extension items
    #'   \code{"SIZE"} (STATUS=SIZE, RFC 8438) and \code{"HIGHESTMODSEQ"}
    #'   (CONDSTORE, RFC 7162), which require the corresponding capability. Default
    #'   is \code{c("MESSAGES", "UNSEEN")}.
    #' @param retries Number of attempts to connect and execute the command.
    #'   Default is \code{1}.
    #' @return A \code{data.frame} with the column \code{folder} followed by one
    #'   numeric column per requested item (\code{NA} for folders that cannot
    #'   be selected).
    #' @examples
    #' \dontrun{
    #' con$list_folders_status()
    #' con$list_folders_status(items = c("MESSAGES", "UNSEEN", "UIDNEXT"))
    #' }
    list_folders_status = function(items = c("MESSAGES", "UNSEEN"), retries = 1) {
      out <- list_folders_status_int(self, items, retries)
      return(out)
    },

    #' @description List the special-use mail folders (IMAP
    #'   \code{LIST (SPECIAL-USE)}, RFC 6154), i.e. the folders the server has
    #'   tagged with a role such as \code{\\Sent}, \code{\\Drafts},
    #'   \code{\\Junk}, \code{\\Trash}, \code{\\Archive}, \code{\\All}, or
    #'   \code{\\Flagged}. Requires the server \code{SPECIAL-USE} capability.
    #' @param retries Number of attempts to connect and execute the command.
    #'   Default is \code{1}.
    #' @return A \code{data.frame} with columns \code{folder} and
    #'   \code{special_use} (one row per folder/attribute).
    #' @examples
    #' \dontrun{
    #' con$list_special_use_folders()
    #' }
    list_special_use_folders = function(retries = 1) {
      out <- list_special_use_folders_int(self, retries)
      return(out)
    },

    #' @description Select a mail folder.
    #' @param name A string containing the name of an existing mail folder on the
    #'   user's mailbox.
    #' @param mute A \code{logical}. If \code{TRUE}, mutes the confirmation message
    #'   when the command is successfully executed. Default is \code{FALSE}.
    #' @param condstore A \code{logical}. If \code{TRUE}, issues
    #'   \code{SELECT ... (CONDSTORE)} (RFC 7162), so that the server reports
    #'   modification sequences in this session. The folder's
    #'   \code{HIGHESTMODSEQ}, when reported, is kept in
    #'   \code{con$con_params$highestmodseq}. Default is \code{FALSE}.
    #' @param retries Number of attempts to connect and execute the command.
    #'   Default is \code{1}.
    #' @return A \code{list} containing the mail folder names and their inherent
    #'   structure.
    #' @examples
    #' \dontrun{
    #' con$select_folder(name = "INBOX")
    #' }
    select_folder = function(name, mute = FALSE, retries = 1, condstore = FALSE) {
      self$con_params$folder <- select_folder_int(self, name, mute, retries,
                                                  condstore = condstore)
      invisible(TRUE)
    },

    #' @description Select a mail folder with \code{QRESYNC} (RFC 7162) and
    #'   report what changed since a known state: the UIDs expunged since the
    #'   given modification sequence and the current flags of the messages
    #'   modified since then. Requires the server \code{QRESYNC} capability
    #'   (and \code{UNSELECT} if a folder is currently selected, since the
    #'   extension must be enabled with no folder selected).
    #' @param name A \code{character} string with the mail folder name.
    #' @param uidvalidity The folder's \code{UIDVALIDITY} at the time of the
    #'   known state (from \code{status()} or a previous \code{resync_folder()}).
    #' @param modseq The modification sequence of the known state (e.g. the
    #'   \code{HIGHESTMODSEQ} recorded then).
    #' @param retries Number of attempts to connect and execute the command.
    #'   Default is \code{1}.
    #' @return A \code{list} with \code{vanished} (an integer vector of
    #'   expunged UIDs), \code{changed} (a \code{data.frame} with \code{seq},
    #'   \code{uid}, \code{flags}, \code{modseq}), \code{highestmodseq},
    #'   \code{uidvalidity}, \code{uidnext}, and \code{exists}. The folder
    #'   is left selected.
    #' @examples
    #' \dontrun{
    #' st <- con$status("INBOX", items = c("UIDVALIDITY", "HIGHESTMODSEQ"))
    #' # ... later:
    #' delta <- con$resync_folder("INBOX", uidvalidity = st[["UIDVALIDITY"]],
    #'                            modseq = st[["HIGHESTMODSEQ"]])
    #' delta$vanished; delta$changed
    #' }
    resync_folder = function(name, uidvalidity, modseq, retries = 1) {
      out <- resync_folder_int(self, name, uidvalidity, modseq, retries)
      return(out)
    },

    #' @description Fetch the flag changes (and, with \code{QRESYNC}, the
    #'   expunges) in the selected folder since a modification sequence
    #'   (\code{UID FETCH 1:* (FLAGS MODSEQ) (CHANGEDSINCE ... VANISHED)},
    #'   RFC 7162). Requires the server \code{CONDSTORE} capability, and
    #'   \code{QRESYNC} for \code{vanished = TRUE}.
    #' @param modseq The modification sequence to compare with.
    #' @param vanished A \code{logical}. If \code{TRUE} (default), the UIDs
    #'   expunged since \code{modseq} are reported as well.
    #' @param retries Number of attempts to connect and execute the command.
    #'   Default is \code{1}.
    #' @return A \code{list} with \code{vanished} (an integer vector of UIDs,
    #'   empty unless \code{vanished = TRUE}) and \code{changed} (a
    #'   \code{data.frame} with \code{seq}, \code{uid}, \code{flags},
    #'   \code{modseq}).
    #' @examples
    #' \dontrun{
    #' con$select_folder("INBOX", condstore = TRUE)
    #' last <- con$con_params$highestmodseq
    #' # ... later in the session:
    #' con$fetch_changes(modseq = last)
    #' }
    fetch_changes = function(modseq, vanished = TRUE, retries = 1) {
      out <- fetch_changes_int(self, modseq, vanished, retries)
      return(out)
    },

    #' @description Close the currently selected mail folder (IMAP \code{CLOSE}),
    #'   permanently removing the messages flagged \code{\\Deleted}. After this,
    #'   no folder is selected.
    #' @param retries Number of attempts to connect and execute the command.
    #'   Default is \code{1}.
    #' @return \code{TRUE} in case the operation is successful.
    #' @examples
    #' \dontrun{
    #' con$select_folder("INBOX")
    #' con$close_folder()
    #' }
    close_folder = function(retries = 1) {
      close_folder_int(self, retries)
      self$con_params$folder <- NA
      invisible(TRUE)
    },

    #' @description Close the currently selected mail folder \strong{without}
    #'   expunging (IMAP \code{UNSELECT}, RFC 3691). Requires the server
    #'   \code{UNSELECT} capability. After this, no folder is selected.
    #' @param retries Number of attempts to connect and execute the command.
    #'   Default is \code{1}.
    #' @return \code{TRUE} in case the operation is successful.
    #' @examples
    #' \dontrun{
    #' con$select_folder("INBOX")
    #' con$unselect_folder()
    #' }
    unselect_folder = function(retries = 1) {
      unselect_folder_int(self, retries)
      self$con_params$folder <- NA
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

    #' @description Request the status of a mail folder without selecting it.
    #'   Unlike \code{examine_folder()}, this does not change the currently
    #'   selected folder.
    #' @param name A \code{character} string containing the name of an existing
    #'   mail folder on the user's mailbox. If no name is passed, the command
    #'   will be executed using the previously selected mail folder name.
    #' @param items A \code{character} vector with the status data items to
    #'   request. Must be a subset of \code{"MESSAGES"}, \code{"RECENT"},
    #'   \code{"UIDNEXT"}, \code{"UIDVALIDITY"}, and \code{"UNSEEN"}, plus the extension items
    #'   \code{"SIZE"} (STATUS=SIZE, RFC 8438) and \code{"HIGHESTMODSEQ"}
    #'   (CONDSTORE, RFC 7162), which require the corresponding capability. Default is
    #'   all of them.
    #' @param retries Number of attempts to connect and execute the command.
    #'   Default is \code{1}.
    #' @return A named \code{numeric} vector with the requested status counts.
    #' @examples
    #' \dontrun{
    #' con$status(name = "INBOX")
    #'
    #' # or, for the selected folder and specific items only:
    #' con$select_folder("INBOX")
    #' con$status(items = c("MESSAGES", "UNSEEN"))
    #' }
    status = function(name = NULL, items = c("MESSAGES", "RECENT", "UIDNEXT",
                                             "UIDVALIDITY", "UNSEEN"),
                      retries = 1) {
      out <- status_int(self, name, items, retries)
      return(out)
    },

    #' @description Create a new mail folder.
    #' @param name A string containing the name of the new mail folder to be
    #'   created.
    #' @param mute A \code{logical}. If \code{TRUE}, mutes the confirmation message
    #'   when the command is successfully executed. Default is \code{FALSE}.
    #' @param retries Number of attempts to connect and execute the command.
    #'   Default is \code{1}.
    #' @param special_use \code{NULL} (default) or a \code{character} vector of
    #'   special-use attributes to assign to the new folder, e.g.
    #'   \code{"\\Archive"} (CREATE-SPECIAL-USE, RFC 6154; requires that
    #'   server capability).
    #' @return \code{TRUE} in case the operation is successful.
    #' @examples
    #' \dontrun{
    #' con$create_folder(name = "New Folder Name")
    #' }
    create_folder = function(name, mute = FALSE, retries = 1, special_use = NULL) {
      invisible(create_folder_int(self, name, mute, retries, special_use = special_use))
    },

    #' @description Rename a mail folder.
    #' @param name A string containing the name of the mail folder to be
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

    #' @description Delete a mail folder.
    #' @param name A string containing the name of the mail folder to be
    #'   deleted.
    #' @param mute A \code{logical}. If \code{TRUE}, mutes the confirmation message
    #'   when the command is successfully executed. Default is \code{FALSE}.
    #' @param retries Number of attempts to connect and execute the command.
    #'   Default is \code{1}.
    #' @return \code{TRUE} in case the operation is successful.
    #' @examples
    #' \dontrun{
    #' con$delete_folder(name = "Folder to remove")
    #' }
    delete_folder = function(name, mute = FALSE, retries = 1) {
      invisible(delete_folder_int(self, name, mute, retries))
    },

    #' @description Subscribe to a mail folder (IMAP \code{SUBSCRIBE}), adding it
    #'   to the set returned by \code{list_subscribed_folders()}.
    #' @param name A string containing the name of the mail folder to subscribe
    #'   to.
    #' @param mute A \code{logical}. If \code{TRUE}, mutes the confirmation message
    #'   when the command is successfully executed. Default is \code{FALSE}.
    #' @param retries Number of attempts to connect and execute the command.
    #'   Default is \code{1}.
    #' @return \code{TRUE} in case the operation is successful.
    #' @examples
    #' \dontrun{
    #' con$subscribe_folder(name = "INBOX")
    #' }
    subscribe_folder = function(name, mute = FALSE, retries = 1) {
      invisible(subscribe_folder_int(self, name, mute, retries))
    },

    #' @description Unsubscribe from a mail folder (IMAP \code{UNSUBSCRIBE}),
    #'   removing it from the set returned by \code{list_subscribed_folders()}.
    #' @param name A string containing the name of the mail folder to
    #'   unsubscribe from.
    #' @param mute A \code{logical}. If \code{TRUE}, mutes the confirmation message
    #'   when the command is successfully executed. Default is \code{FALSE}.
    #' @param retries Number of attempts to connect and execute the command.
    #'   Default is \code{1}.
    #' @return \code{TRUE} in case the operation is successful.
    #' @examples
    #' \dontrun{
    #' con$unsubscribe_folder(name = "INBOX")
    #' }
    unsubscribe_folder = function(name, mute = FALSE, retries = 1) {
      invisible(unsubscribe_folder_int(self, name, mute, retries))
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

    ## SORT and THREAD (RFC 5256)
    #' @description Sort messages on the server (IMAP \code{SORT}, RFC 5256).
    #'   Returns the message ids ordered by the server according to the sort
    #'   keys. Requires the server to advertise the \code{SORT} capability (check
    #'   with \code{list_server_capabilities()}).
    #' @param by A \code{character} vector of sort keys, a subset of
    #'   \code{"ARRIVAL"}, \code{"CC"}, \code{"DATE"}, \code{"FROM"},
    #'   \code{"SIZE"}, \code{"SUBJECT"}, and \code{"TO"}. Default is
    #'   \code{"DATE"}.
    #' @param reverse A \code{logical}. If \code{TRUE}, each sort key is prefixed
    #'   with \code{REVERSE} (descending order). Default is \code{FALSE}.
    #' @param criteria A \code{character} string with the search criteria that
    #'   restricts the set to be sorted. Default is \code{"ALL"}.
    #' @param use_uid A \code{logical}. If \code{TRUE}, issues \code{UID SORT} and
    #'   returns UIDs instead of sequence numbers. Default is \code{FALSE}.
    #' @param char_set A \code{character} string with the charset of the search
    #'   criteria. Default is \code{"UTF-8"}.
    #' @param return \code{NULL} (default) or a \code{character} vector with
    #'   any of \code{"COUNT"}, \code{"MIN"}, \code{"MAX"}, and \code{"ALL"}.
    #'   When given, issues \code{SORT RETURN (...)} (ESORT, RFC 5267) and
    #'   returns only the requested items, computed by the server in sort
    #'   order, as a named \code{list}. Requires the server \code{ESORT}
    #'   capability.
    #' @param retries Number of attempts to connect and execute the command.
    #'   Default is \code{1}.
    #' @return An \code{integer} vector of message ids in the server-provided
    #'   (sorted) order.
    #' @examples
    #' \dontrun{
    #' con$select_folder("INBOX")
    #' con$sort(by = "DATE", reverse = TRUE)
    #' }
    sort = function(by = "DATE", reverse = FALSE, criteria = "ALL",
                    use_uid = FALSE, char_set = "UTF-8", return = NULL,
                    retries = 1) {
      out <- sort_int(self, by, reverse, criteria, use_uid, char_set, retries,
                      return = return)
      return(out)
    },

    #' @description Thread messages on the server (IMAP \code{THREAD}, RFC 5256).
    #'   Returns the messages grouped into threads. Requires the server to
    #'   advertise a \code{THREAD=} capability (check with
    #'   \code{list_server_capabilities()}).
    #' @param algorithm A \code{character} string with the threading algorithm,
    #'   either \code{"REFERENCES"} or \code{"ORDEREDSUBJECT"}. Default is
    #'   \code{"REFERENCES"}.
    #' @param criteria A \code{character} string with the search criteria that
    #'   restricts the set to be threaded. Default is \code{"ALL"}.
    #' @param use_uid A \code{logical}. If \code{TRUE}, issues \code{UID THREAD}
    #'   and returns UIDs instead of sequence numbers. Default is \code{FALSE}.
    #' @param char_set A \code{character} string with the charset of the search
    #'   criteria. Default is \code{"UTF-8"}.
    #' @param retries Number of attempts to connect and execute the command.
    #'   Default is \code{1}.
    #' @return A \code{list} of \code{integer} vectors, one per top-level thread.
    #' @examples
    #' \dontrun{
    #' con$select_folder("INBOX")
    #' con$thread(algorithm = "REFERENCES")
    #' }
    thread = function(algorithm = "REFERENCES", criteria = "ALL",
                      use_uid = FALSE, char_set = "UTF-8", retries = 1) {
      out <- thread_int(self, algorithm, criteria, use_uid, char_set, retries)
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
    #'   \code{\link{older_than}}.
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
    #' @param save A logical. Default is \code{FALSE}. If \code{TRUE}, the
    #'   result is saved on the server (\code{SEARCH RETURN (SAVE)}, SEARCHRES,
    #'   RFC 5182) instead of being returned, and the method returns the
    #'   \code{"$"} reference, which the fetch, flag, copy, move, and delete
    #'   methods accept as \code{msg_id}. Requires the server
    #'   \code{SEARCHRES} capability.
    #' @param retries Number of attempts to connect and execute the command.
    #'   Default is \code{1}.
    #' @note \href{#method-search}{\code{ImapCon$search()}}: IMAP queries follow
    #'   Polish notation, i.e. operators such as \code{OR} come before arguments,
    #'   e.g. "OR argument1 argument2". Therefore, the relational-operator-helper-functions
    #'   in this package should be used like the following examples:
    #'   \code{OR(before("17-Apr-2015"), string("FROM", "John"))}. Even though there
    #'   is no "AND" operator in IMAP, this package adds a helper function
    #'   \code{\link{AND}} to indicate multiple arguments that must be searched
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
                      esearch = FALSE, save = FALSE, retries = 1) {
      out <- search_int(self, request, negate, use_uid, esearch, retries,
                        save = save)
      if (isTRUE(save)) {
        return(invisible(out))
      }
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
    #' con$search_larger_than(size = 512000)
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
    #' con$search_smaller_than(size = 512000)
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
    #'                   negate = TRUE)
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
    #' @family search by date
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
    #'                   negate = TRUE)
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
    #'   truncated by special formatting characters such as \code{\\r\\n} for example.
    #'   It is recommended to perform this type of search using \code{where = "BODY"},
    #'   instead of \code{"TEXT"} (\cite{Heinlein, P. and Hartleben, P. (2008)}).
    #' @references \href{#method-search_string}{\code{ImapCon$search_string()}}:
    #'   Heinlein, P. and Hartleben, P. (2008). The Book of IMAP: Building a
    #'   Mail Server with Courier and Cyrus. No Starch Press. ISBN 978-1-59327-177-0.
    #' @return A \code{numeric vector} containing the message ids.
    #' @family search by string
    #' @examples
    #' \dontrun{
    #' con$select_folder(name = "INBOX")
    #' # search for messages with "@k-state.edu" in the FROM field
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
    #'   The extension attributes \code{"PREVIEW"} (RFC 8970),
    #'   \code{"SAVEDATE"} (RFC 8514), and \code{"MODSEQ"} (CONDSTORE, RFC 7162)
    #'   may also be requested when the server advertises the corresponding
    #'   capability.
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
    #' @param changed_since \code{NULL} (default) or a modification sequence:
    #'   with it only the messages modified after that sequence are returned
    #'   (\code{CHANGEDSINCE}, CONDSTORE, RFC 7162), each with its
    #'   \code{MODSEQ}.
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
                              mute = FALSE, retries = 1, changed_since = NULL) {
      out <- fetch_metadata_int(self, msg_id, use_uid, attribute, write_to_disk,
                                keep_in_mem, mute, retries,
                                changed_since = changed_since)

      if (isTRUE(write_to_disk)) {
        invisible(out)
      } else {
        return(out)
      }

    },

    #' @description Fetch the server-generated preview of messages (IMAP
    #'   \code{FETCH ... (PREVIEW)}, RFC 8970): a short text snippet of each
    #'   message, produced by the server without transferring the message
    #'   body. Requires the server \code{PREVIEW} capability.
    #' @param msg_id A \code{numeric vector} containing one or more message
    #'   ids, or the \code{"$"} reference of a saved search.
    #' @param use_uid Default is \code{FALSE}. If \code{TRUE}, the command is
    #'   performed with UIDs and the result is named by UID.
    #' @param retries Number of attempts to connect and execute the command.
    #'   Default is \code{1}.
    #' @return A named \code{character} vector with one preview per message
    #'   (\code{NA} when the server has none).
    #' @examples
    #' \dontrun{
    #' con$select_folder(name = "INBOX")
    #' con$search_flag("UNSEEN") %>% con$fetch_preview()
    #' }
    fetch_preview = function(msg_id, use_uid = FALSE, retries = 1) {
      out <- fetch_preview_int(self, msg_id, use_uid, retries)
      return(out)
    },

    #' @description Fetch the envelope of messages parsed into a data frame
    #'   (IMAP \code{FETCH ... (ENVELOPE)}): date, subject, and the address
    #'   lists, with RFC 2047 encoded words decoded. See
    #'   \code{\link{parse_envelope}}.
    #' @param msg_id A \code{numeric vector} containing one or more message
    #'   ids, or the \code{"$"} reference of a saved search.
    #' @param use_uid Default is \code{FALSE}. If \code{TRUE}, the command is
    #'   performed with UIDs and the first column is \code{uid}.
    #' @param retries Number of attempts to connect and execute the command.
    #'   Default is \code{1}.
    #' @return A \code{data.frame} with one row per message: \code{id} (or
    #'   \code{uid}), \code{date}, \code{subject}, \code{from},
    #'   \code{sender}, \code{reply_to}, \code{to}, \code{cc}, \code{bcc},
    #'   \code{in_reply_to}, and \code{message_id}.
    #' @examples
    #' \dontrun{
    #' con$select_folder(name = "INBOX")
    #' con$search_since(date_char = "01-Jan-2026") %>% con$fetch_envelope()
    #' }
    fetch_envelope = function(msg_id, use_uid = FALSE, retries = 1) {
      out <- fetch_envelope_int(self, msg_id, use_uid, retries)
      return(out)
    },

    #' @description Fetch the MIME structure of messages parsed into a data
    #'   frame of parts (IMAP \code{FETCH ... (BODYSTRUCTURE)}), one row per
    #'   part with its section number, type, charset, filename, encoding,
    #'   size, and disposition. See \code{\link{parse_bodystructure}}.
    #' @param msg_id A \code{numeric vector} containing one or more message
    #'   ids, or the \code{"$"} reference of a saved search.
    #' @param use_uid Default is \code{FALSE}. If \code{TRUE}, the command is
    #'   performed with UIDs and the first column is \code{uid}.
    #' @param retries Number of attempts to connect and execute the command.
    #'   Default is \code{1}.
    #' @return A \code{data.frame} with one row per MIME part of each message.
    #' @examples
    #' \dontrun{
    #' con$select_folder(name = "INBOX")
    #' parts <- con$fetch_bodystructure(msg_id = 1:10)
    #' parts[parts$is_attachment, ]
    #' }
    fetch_bodystructure = function(msg_id, use_uid = FALSE, retries = 1) {
      out <- fetch_bodystructure_int(self, msg_id, use_uid, retries)
      return(out)
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
    #' con$search_string(expr = "@k-state.edu", where = "FROM") %>%
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
    #'   When the server advertises \code{UIDPLUS} (RFC 4315), the vector
    #'   carries a \code{"copyuid"} attribute: a \code{data.frame} mapping each
    #'   \code{source_uid} to the \code{dest_uid} assigned in the destination
    #'   folder.
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

      ids <- out$msg_id
      if (!is.null(out$copyuid)) {
        attr(ids, "copyuid") <- out$copyuid
      }
      invisible(ids)

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
    #'   When the server advertises \code{UIDPLUS} (RFC 4315), the vector
    #'   carries a \code{"copyuid"} attribute: a \code{data.frame} mapping each
    #'   \code{source_uid} to the \code{dest_uid} assigned in the destination
    #'   folder.
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

      ids <- out$msg_id
      if (!is.null(out$copyuid)) {
        attr(ids, "copyuid") <- out$copyuid
      }
      invisible(ids)

    },

    #' @description Append a full RFC 822 message to a mail folder (IMAP
    #'   \code{APPEND}). Useful to save a message to folders such as
    #'   \code{Drafts} or \code{Sent}. Unlike the other operations this is
    #'   performed by an upload to the folder. The message is stored with the
    #'   flags given in \code{flags} (none by default). When the server
    #'   advertises \code{UIDPLUS} (RFC 4315), the UID assigned to the message
    #'   is returned.
    #' @param message A \code{character} string or \code{raw} vector with the
    #'   full RFC 822 message (headers and body).
    #' @param folder A \code{character} string with the destination folder. If no
    #'   folder is passed, the previously selected folder is used.
    #' @param flags \code{NULL} (default) or a \code{character} vector with the
    #'   flags to store with the message: any of \code{"Seen"},
    #'   \code{"Flagged"}, \code{"Answered"}, \code{"Draft"}, and
    #'   \code{"Deleted"}. Requires libcurl >= 8.13; earlier versions ignore
    #'   this argument and always store the message with \code{\\Seen}.
    #' @param mute A \code{logical}. If \code{TRUE}, mutes the confirmation message
    #'   when the command is successfully executed. Default is \code{FALSE}.
    #' @param retries Number of attempts to connect and execute the command.
    #'   Default is \code{1}.
    #' @return Invisibly, the UID assigned to the appended message when the
    #'   server reports it (\code{APPENDUID} response code, UIDPLUS), or
    #'   \code{NA} otherwise.
    #' @examples
    #' \dontrun{
    #' msg <- paste("From: me@example.com", "To: you@example.com",
    #'              "Subject: Hi", "", "Message body.", sep = "\r\n")
    #' con$append_msg(message = msg, folder = "Drafts", flags = "Draft")
    #' }
    append_msg = function(message, folder = NULL, flags = NULL, mute = FALSE,
                          retries = 1) {
      invisible(append_int(self, message, folder, flags, mute, retries))
    },

    #' @description Count the number of messages with a specific flag(s) in a
    #'   folder (depends on ESEARCH capability)
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
    #'   Expunging specific messages (\code{UID EXPUNGE}) requires the server
    #'   \code{UIDPLUS} capability (RFC 4315); a plain expunge does not.
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
    #'   (depends on ESEARCH capability)
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
    #'   (depends on ESEARCH capability)
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
    #' @param unchanged_since \code{NULL} (default) or a modification sequence:
    #'   with it the \code{STORE} is conditional (\code{UNCHANGEDSINCE},
    #'   CONDSTORE, RFC 7162) and only the messages not modified after that
    #'   sequence are updated; the ids the server refused are returned in the
    #'   \code{"modified"} attribute of the result.
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
                         retries = 1, unchanged_since = NULL) {
      out <- add_flags_int(self, msg_id, use_uid, flags_to_set, mute, retries,
                      unchanged_since = unchanged_since)

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
    #' @param unchanged_since \code{NULL} (default) or a modification sequence:
    #'   with it the \code{STORE} is conditional (\code{UNCHANGEDSINCE},
    #'   CONDSTORE, RFC 7162) and only the messages not modified after that
    #'   sequence are updated; the ids the server refused are returned in the
    #'   \code{"modified"} attribute of the result.
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
    #'   con$replace_flags(flags_to_set = c("\\UNSEEN", "\\Flagged"))
    #' }
    replace_flags = function(msg_id, use_uid = FALSE, flags_to_set, mute = FALSE,
                             retries = 1, unchanged_since = NULL) {
      out <- replace_flags_int(self, msg_id, use_uid, flags_to_set, mute, retries,
                      unchanged_since = unchanged_since)

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
    #' @param unchanged_since \code{NULL} (default) or a modification sequence:
    #'   with it the \code{STORE} is conditional (\code{UNCHANGEDSINCE},
    #'   CONDSTORE, RFC 7162) and only the messages not modified after that
    #'   sequence are updated; the ids the server refused are returned in the
    #'   \code{"modified"} attribute of the result.
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
    #' # Remove the "\\SEEN" flag from the messages in the search result
    #' con$search_since(date_char = "20-Aug-2020") %>%
    #'   con$remove_flags(flags_to_unset = "\\UNSEEN")
    #' }
    remove_flags = function(msg_id, use_uid = FALSE, flags_to_unset, mute = FALSE,
                            retries = 1, unchanged_since = NULL) {
      out <- remove_flags_int(self, msg_id, use_uid, flags_to_unset, mute, retries,
                      unchanged_since = unchanged_since)

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
    #' @param local_dir A \code{character} string with the base directory where the
    #'   attachments will be saved. A subfolder tree
    #'   \code{<local_dir>/<username>/<mail folder>/<msg id>} is created inside it.
    #'   Default is \code{"."} (the current working directory).
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
    #' res <- con$search_string(expr = "@gmail", where = "CC")
    #' out <- con$fetch_body(msg = res)
    #' con$get_attachments(msg_list = out)
    #' }
    get_attachments = function(msg_list, content_disposition = "both",
                               override = FALSE, mute = FALSE, as_is = FALSE,
                               local_dir = ".") {
      out <- get_attachments_int(self, msg_list, content_disposition, override,
                                 mute, as_is, local_dir)

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
    #' out <- con$search_string(expr = "@k-state.edu", where = "FROM") %>%
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

    #' @description Fetch attachments by MIME part, guided by the message's
    #'   \code{BODYSTRUCTURE}. The structure of each message is retrieved
    #'   first (see \code{fetch_bodystructure()}), the attachment parts are
    #'   selected, each is fetched with \code{BODY.PEEK[<part>]}, decoded from
    #'   its transfer encoding, and written to disk, or returned as raw
    #'   vectors. Unlike \code{fetch_attachments()}, which parses MIME
    #'   boundaries from the fetched body, this method relies on the parts as
    #'   declared by the server and transfers nothing but the attachments.
    #' @param msg_id A \code{numeric vector} containing one or more message
    #'   ids, or the \code{"$"} reference of a saved search.
    #' @param use_uid Default is \code{FALSE}. If \code{TRUE}, the command is
    #'   performed with UIDs.
    #' @param parts \code{NULL} (default: the parts selected by
    #'   \code{content_disposition}) or a \code{character} vector of section
    #'   numbers to fetch, e.g. \code{c("2", "3.1")}.
    #' @param content_disposition As in \code{fetch_attachments()}:
    #'   \code{"both"} (default), \code{"attachment"}, or \code{"inline"},
    #'   selecting the parts by the \code{Content-Disposition} the server
    #'   declares in the \code{BODYSTRUCTURE}. With \code{"both"}, non-text
    #'   parts that carry a filename but no disposition are included as well.
    #' @param local_dir The base directory where the files are written, in a
    #'   \code{<username>/<folder>/<msg id>} tree, as in
    #'   \code{fetch_attachments()}. Default is \code{"."}. If \code{NULL},
    #'   nothing is written and the payloads are returned in a \code{content}
    #'   list column.
    #' @param override A \code{logical}. If \code{TRUE}, overwrites existing
    #'   files; otherwise repeated filenames are numbered. Default is
    #'   \code{FALSE}.
    #' @param mute A \code{logical}. If \code{TRUE}, mutes the confirmation
    #'   message. Default is \code{FALSE}.
    #' @param retries Number of attempts to connect and execute the command.
    #'   Default is \code{1}.
    #' @return A \code{data.frame} with one row per fetched part: \code{id}
    #'   (or \code{uid}), \code{part}, \code{filename}, \code{type},
    #'   \code{size} (bytes), and \code{path} (or \code{content}).
    #' @examples
    #' \dontrun{
    #' con$select_folder(name = "INBOX")
    #' con$search_string(expr = "report", where = "SUBJECT") %>%
    #'   con$fetch_attachment_parts(local_dir = "~/attachments")
    #' }
    fetch_attachment_parts = function(msg_id, use_uid = FALSE, parts = NULL,
                                      content_disposition = "both",
                                      local_dir = ".", override = FALSE,
                                      mute = FALSE, retries = 1) {
      out <- fetch_attachment_parts_int(self, msg_id, use_uid, parts, local_dir,
                                        override, mute, retries,
                                        content_disposition = content_disposition)
      invisible(out)
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
    #' @param local_dir A \code{character} string with the base directory where the
    #'   attachments will be saved. A subfolder tree
    #'   \code{<local_dir>/<username>/<mail folder>/<msg id>} is created inside it.
    #'   Default is \code{"."} (the current working directory).
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
                                 as_is = FALSE, local_dir = ".") {
      out <- fetch_attachments_int(self, msg_id, use_uid, content_disposition,
                                   override, mute, retries, as_is, local_dir)

      invisible(out)

    }#,

  )
)

