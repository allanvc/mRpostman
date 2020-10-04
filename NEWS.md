## mRpostman 0.9.1 (2020-10-03 Github)

### Main features
* added RFC 2047 quoted-printable and base64 MIME header decoder

### Functions & methods
* added the `decode_mime_header()` function
    - it is used inside `get_attachments()` and `fetch_attachments()` for correctly naming the filenames
    - it is also exported for the user in order to be used for a header decoding operation after fetching metadata, for example.
    - it was necessary to add the {stringi} package as dependency

### Bug fixes

* `list_mail_folders()`: fixed the regex related to the hierarchy separator to accept the "|" separator. It was causing a misbehavior in Yandex accounts.

* `clean_fetch_results()` (internal): fixed the regex responsible for cleaning the attachment content. It was causing a misbehavior in Yandex accounts. All calls to gsub() in this function now have `ignore.case = TRUE`. 

### Minor changes

* The confirmation message in `fetch_attahments()` changed to "\n::mRpostman: the fetch operation is complete.\n"



## mRpostman 0.9.0.0 (2020-09-08 Github/2020-09-15 CRAN)

### Important message

In this version, in order to drastically enhance the package usability,  we had to adopt several profound changes with no backward compatibility. We are sorry that these changes will be painful for old users but it will certainly be strongly beneficial even in the short term. The primary update is that `mRpostman` now is built on an `R6` class and its methods, providing a much more convenient and elegant way of usage. It is structured following an OO framework that works well either with the tidy style using pipes or under the base R approach. The syntax now will be `ConnectionObject$method()`, where the `ConnectionObject` is created with `configure_imap()` or `ImapCon$new()`. This completely modifies how the connection handle and other configuration information is passed among the methods/functions in `mRpostman`.

This is a summary of the main modifications in the package:

* The package title has changed to mRpostman: An IMAP client for R

* All the main functions, except by `list_attachments` and the custom-search helper functions, now are methods of the R6 class `ImapConf`;
    
* The way the connection token is passed between the functions has changed. The connection handle is created only  inside `configure_imap()` (or `ImapCon$new()`) and only modified with custom requests inside the methods. As a consequence, the password, username, and other connection parameters are hidden inside the curl handle C pointer, resulting in a more secure token chain. This resulted in changes in every request-functions. They do not use `config_handle()` anymore, and a call to `curl::set_opt()` is made in every request function so that a custom request is supplied or replaced by a new one in the original handle.
    
* the argument "by" used in search and fetch functions was replaced by `use_uid`, which is a logical with default value set as `FALSE`. This is equivalent to the former `by = MSN` default configuration.
    
* all functions that returned `invisible(0L)` now return `invisible(TRUE)`



### Changes without backward compatibility
    
#### More appropriate function names [old function -> new method]:

* `list_mailboxes()`     -> `list_mail_folders()`
* `select_mailbox()`     -> `select_folder()`
* `examine_mailbox()`    -> `examine_folder()`
* `custom_search()`      -> `search()`
* `fetch_full_msg()`     -> `fetch_body()`
* `fetch_msg_header()`   -> `fetch_header()`
* `fetch_msg_text()`     -> `fetch_text()`
* `fetch_msg_metadata()` -> `fetch_metadata()`
* `get_min_id()`         -> `esearch_min_id()`
* `get_max_id()`         -> `esearch_max_id()`
* `count_msg()`          -> `esearch_count()`


#### Functions that were removed:

* `section_or_fields()`

* `flag_options()`


#### More appropriate argument names [old - > new]:


* `mbox` -> `name`:
    
    + OBS: type has not changed, still a character string
    
    + Affected functions: `select_folder()` (former select_mailbox), `examine_folder()` (former select_mailbox), `rename_folder()` (former rename_mailbox)


* `reselect_mbox` -> `reselect`:
    
    + OBS: type has not changed, still a logical with `TRUE` as default
    
    + Affected functions: `rename_folder()` (former rename_mailbox), `copy_msg()`, `move_msg()`
    

* `by` -> `use_uid`:
    
    + OBS: type HAS CHANGED from a character string with default value `MSN` to a logical with `FALSE` as default. Former `by = "UID"` now is equivalent to `use_uid = TRUE`
    
    + Affected functions:
        + all search operations: `search()` (former custom search), `search_before()`, `search_since()`, `search_on()`, `search_period()`, `search_sent_before()`, `search_sent_since()`, `search_sent_on()`, `search_sent_period()`, `search_larger_than()`, `search_smaller_than()`, `search_older_than()`, `search_younger_than()`, `search_flag()`, `search_string()`;
        
        + all fetch operations: `fetch_body()` (former fetch_full_msg), `fetch_header()` (former fetch_msg_text), `fetch_text()` (former fetch_msg_text), `fetch_metadata()` (former fetch_msg_metadata);
        
        + all complementary operations: `copy_msg()`, `move_msg()`, `delete_msg()`, `esearch_max_id()` (former get_max_id), `esearch_min_id()` (former get_min_id), `esearch_count()` (former count_msg), `add/replace/remove_flags()`
        
        
* `flag` -> `name`:
    
    + OBS: type has not changed, still a character string containing the flag name
    
    + Affected functions: `search_flag()`, `flag()` (custom search helper function)
        
        
* `string` -> `expr`:
    
    + OBS: type has not changed, still a character string containing the string or expression to be searched; shifted to the first position of the arguments in the functions/methods.
    
    + Affected functions: `search_string()`, `string()` (custom search helper function)


* `section_or_fields` -> `where`:
    
    + OBS: type has not changed, still a character string containing the name of the message section or the header field in which to execute the search for the informed expression; 
    
    + Affected functions: `search_string()`, `string()` (custom search helper function)
        
        
* `specific_UID` -> `msg_uid`:
    
    + OBS: type has not changed, still a numeric vector containing message uids
    
    + Affected functions: `expunge()`
    

* `to_mbox` -> `to_folder`:
    
    + OBS: type has not changed, still a character vector containing the folder name
    
    + Affected functions: `copy_msg()`, `move_msg()`


* `try_b64decode` -> `base64_decode`:
    
    + OBS: type has not changed, still a logical with default value `FALSE`
    
    + Affected functions: `fetch_text()` (former fetch_msg_text)
        
        
* `show_pass` -> removed:
    
    + OBS: This argument is not available anymore.  
    
    + Affected functions: `configure_imap()`
    
    
* `fresh_connect` -> removed:
    
    + OBS: This `curl` argument is not accepted by mRpostman anymore.  
    
    + Affected functions: `configure_imap()`
    

* `return_imapconf` -> removed:
    
    + OBS: This argument is not available anymore.  
    
    + Affected functions: all search methods.


#### Changes in the arguments' position

* in `search_string()` and `string()`:
    + ``expr` (former string) now is 1st; `where` (former section or fields)` now is 2nd

* in `esearch_count()` (former count_msgs), `esearch_max_id()` (former get_max_id), `esearch_min_id()` (former get_min_id):
    + swapped the order between the arguments `use_uid` (former by) and `flag`    


#### Behaviour changes

* default value of arguments:
    
    + `reselect_mbox = FALSE` ->  `reselect = TRUE` (former reselect_mbox) in `move_msg()` and `copy_msg()`



### Changes that should not cause any break


#### Default behavior changes:

* returned object:
    
    + functions/methods that returned `invisible(0L)` in the previous version, now return `invisible(TRUE)`. Applied to: `select_folder()`, `create_folder()`, `rename_folder()`, `get_attachments()`, `fetch_attachments()`, `expunge()`
    
    + all search functions now return `NA` when there is no match. The previous behavior was to return 0.
    
    + `add/replace/remove_flags()` methods now invisibly return the msg_ids in case the user intends to chain any further operation (perhaps expunge) using the pipe.


* default value of arguments:
    
    + `retries = 2` -> `retries = 1` in all functions
    

#### New methods:

* `create_folder()`: Create a new mail folder (New IMAP functionality!)

* `list_flags()`: List flags in a selected mail folder (New IMAP functionality!)

* `fetch_attachments_list()`: Fetch attachments' list without the previously need to fetch a message's text or body 

* `fetch_attachments()`: Fetch attachments without the previously need to fetch a message's text or body 

* `reset_*()`: reset one (*) of the original parameter that were informed in `configure_imap()`.


#### New arguments:

* `mute`:
    
    * OBS:  A logical. If TRUE, mutes the confirmation message when the command is successfully executed. Default is          FALSE. In the case of the `fetch_*()` functions, it only has effect when `write_to_disk = TRUE`.
    
    * applied to methods/functions: `select_folder()`, `create_folder()`, `rename_folder()`, `fetch_body()`, `fetch_header()`, `fetch_text()`, `fetch_metadata()`, `copy_msg()`, `move_msg()`,       `delete_msg()`, `expunge()`, `add/replace/remove_flags()`, `get_attachments()`, `fetch_attachments()`

        
* `override`:
    
    * OBS:  A logical. If TRUE, overrides existent files containing the same name in the local directory. Default is FALSE.
    
    * applied to methods/functions: `get_attachments()`, `fetch_attachments()`.
    
    
* `xoauth2_bearer`:
    
    * OBS:  added the `xoauth2_bearer` parameter for oauth2.0 authentication (libcurl >= 7.70 is required because of bugs in previous versions). In Linux, if you use Ubuntu 20, you should be fine. Versions below this require updating libcurl if the user intends to use oauth2.0 authentication.
    
    * applied to methods/functions: `configure_imap()`
        
        
#### New functionalities:

* in `examine_folder()` and `rename_folder()`:
    + if a folder was previous selected, the user does not have to provide the folder name
    + if the user wants to examine or rename a folder different from the selected folder, he/she has just to inform the       folder `name` argument
    
* `flag` argument in all search functions:
    + all search methods that have the optional argument `flag` now accept more than one flag as a filtering parameter
    + this is also applied to the `name` argument in `search_flag`
    + the `flag` parameter was added to the `search_string` method
    
* in `get_attachments()` and `fetch_attachments()`:
    + `override` argument was added and allows to control over the file writing process


### Bug fixes

* retry bug fixed: it was causing the loss of the search and fetch `customrequest` when executing a retry + selection operation or when there was a considerable period between two requests given that the second depends on a previous folder selection. When there was a considerable delay between the executions of two commands, the curl handle would establish a new connection to execute the last one, but without the mail folder selection. This was causing an error during the retry or the next IMAP command since the IMAP session would have lost the mail folder selection. This bug was happening mainly when the functions were used under the base R approach

    + applied to: all request functions/methods, such as search, fetch, mailbox operations (except those that don't need a previously folder selection), and complementary operations
    
* fixed bug that was writing metadata .txt files as textUID*.txt


### Internal changes

* Better error handling:
    
    + An unique internal function called `check_args()` was created to check for the validity of the arguments of all methods and functions, replacing all the cheack_args_* specific functions
    
    + added the `response_error_handling()` function to catch operation/resolving timeout errors and login error as well

* All methods work as wrappers for internal functions with similar names and suffix `*_int`

* Search, Fetch and Complementary functions have a central internal function called `execute_*()` that is responsible for configuring and executing the requests towards the IMAP server

* removed `config_handle()`as the connection token chain has changed

* added `config_conn_handle()` which is called inside the `iniliaze()` method of the R6 `ImapCon` class.

* added `adjust_repeated_filenames()`, `serialize_filenames()` and `extract_MIME_level_and_filenames()` as helper functions to the new attachments fetch operations

* added `adjust_folder_name()` which is called in almost every request function

* some regex adjustments were made to `fix_search_stripping()`, `has_attachment()`, and `clear_fetch_results()`

* in `list_server_capabilities()`: changed IMAP command to "CAPABILITY" instead of establishing a new connection

* REGEX structure replacement from `stringr` to base R in: `list_server_capabalities()`, `examine_folder()` (former examine_mailbox), `list_mail_folders()` (former list_mailboxes), `select_folder()` (former select_mailbox)

### Other general changes:

* `fetch_attachments_list()` and `fetch_attachments()` are a faster and smart way to respectively list and download messages' attachments. They do not depend on a former fetching step, unlike `list_attachments()` and `get_attachments()`. the new methods use BODYSTRUCTURE metadata fetching to identify the attachments, and `fetch_attachments()` also issue a FETCH BODY[level.MIME] command to fetch only the parts of the messages that contain the attachments. This prevents unnecessary fetching when users are only interested in attachments. However, `get_attachments()` and `list_attachments()` are still available in the package.

* All `fetch_*` methods, and `get_attachments()` now use a different path for saving the fetched files. The folder to be created now will have the following structure: imap_server > mail_folder > <messageID> or <messageUID>.

* Fetched messages that are saved to disk will have different filename structures:
    + if `use_uid = FALSE`: body<id>.txt, header<id>.txt, meta<id>.txt, text<id>.txt
    + if `use_uid` = TRUE: body<uid>.txt, header<uid>.txt, meta<uid>.txt, text<uid>.txt
    
* Confirmation messages were added to methods: `select_folder()`, `create_folder()`, `rename_folder()`, `copy_msg()`, `move_msg()`,       `delete_msg()`, `expunge()`, `fetch_*()`, `add/replace/remove_flags()`, `get_attachments()`, `fetch_attachments()`

* A startup message informing about the breaking changes of the version were added by creating the zzz.R file

* Besides the _`mRpostman` Basics_ vignette, two more were added: _Migrating old code to the new mRpostman's syntax_, and _IMAP OAuth2.0 authentication in mRpostman_

---

## mRpostman 0.3.1 (2020-03-27 Github/ 2020-04-18 CRAN)

### Features

* `get_attachments()` function:
    + added support to inline attachments: argument "content_disposition"
    + changed REGEX for retrieving text and filenames

* `check_args_get_atatchments()` helper function:
    + added support to inline attachments: argument `"content_disposition"`

* `list_attachments()` function:
    + added support to inline attachments: added a column `"content_disposition"` to output
    + changed REGEX for retrieving text and filenames

* `check_args_list_attachments()` function:
    + added helper function `check_args_list_attachments()`

* `has_attachment()` helper function:
    + added support to inline attachments

* `loop_fetch_msg_*()` functions group:
    + changed error message

### Documentation

* DESCRIPTION FILE:
    + changed title to IMAP Toolkit
    + changed Description field

* README.md:
    + item 6) Attachments: added mention to inline attachments
    + changed description
    + added Outlook - Office 365 configuration example
    + changed all examples from UC Riverside to Kansas State University (sorry UCR!)

* Basics Vigntte
    + changed description
    + added Outlook - Office 365 configuration example
    + changed all examples from UC Riverside to Kansas State University (sorry UCR!)
    
* `get_attachments()`:
    + added new note to explain Content-Disposition types
    + added reference to RFC2183

* `check_args_get_attachments()`:
    + added new argument `"content_disposition"`

### Other

* removed garbage script `environments_tests.R`


---

## mRpostman 0.3.0 (2019-10-07)


### Features

* `get_attachments()` function:
    + added `get_attachments()`
    + added helper functions: `has_attachment()`, `check_args_get_attachments()`

* `list_attachments()` function:
    + added `list_attachments()`

* `loop_fetch_msg_*()` functions:
    + error msg: fixed from `examineMailbox()` to `examine_mailbox()`
    + fixed "forbiden[_]char[s]" in `loop_fetch_full_msg()`
    + changed forbiden_chars to `[\\/:*?\"<>|]`, considering WIN-*NIX-OSX cases

* fetch_full_msg():
+ added warning message related to `get_attachments()` when  `keep_in_mem = TRUE`

* `fetch_*()` functions:
+ added `attr(msg_list, which = 'mbox') = new_imapconf$mbox` to all -- to be used by `get_attachments()`

* `list_server_capabilities()` function:
+ converted regex match/extraction from stringr to base R

* examples:
+ fixed typo - from "configureIMAP" to "configure_imap" in the following functions examples: `list_mailboxes()`, `list_server_capabilities()`
+ changed all imaps://imap.gmail.com server examples to a generic one: imaps://your.imap.server.com

### Documentation

* DESCRIPTION FILE:
    + changed title to IMAP Toolkit
    + changed Description field

* README.md:
    + changed dev installation example from `devtools` to `remotes` because the former depends on `curl`
    + fixed typo - from "configureIMAP" to "configure_imap" in README.md
    + fixed typo - two AOL examples instead of one AOL and one Yahoo configure_imap example
    + added Yandex `configure_imap()` example

* basics.Rmd vignette:
    + fixed typo - from "configureIMAP" to "configure_imap" in README.md
    + fixed typo - two AOL examples instead of one AOL and one Yahoo configure_imap example
    + added Yandex `configure_imap()` example
    + removed figures (already appear on README.md)

* `count_msgs()`:
    + changed title

---

## mRpostman 0.2.1-X (2019-08-22 Github only)


### Features (0.2.1-2)

* `loop_fetch_msg_XXXX()` functions for fetching msgs:
    + added `select_mailbox(imapconf = new_imapconf, mbox = new_imapconf$mbox)` inside
    the tryCatch in the while loop -- it prevents errors especially when messages have
    larger attachments taking too much time to fetch. In some IMAP servers (such as 
    Yandex) it may lose the mbox selection. Other alternatives: set a larger 
    `timeout_ms` in `configure_imap()`.

* `list_mailboxes()`:
    + REGEX for cleaning mbox names from `list_mailboxes()` was modified to deal with 
    the return of Yandex IMAP server: from `.*\" \"*(.*?)\\"\r\n' to '.*\" \"*(.*?)[(\\"\r\n)|(\r\n\\*)]`

### Documentation (0.2.1-1)

* function `configure_imap()`:
    + `@param retries description` typo: from "retires" to "retries"

* internal function `conifg_handle()`:
    + removed `@family config`, so it does not appear as "see also" in `config_IMAP()`

* function `flag_options()`:
    + updated `@note` to be more clear

* all internal functions from `@family check args search`:
    + `@param esearch` typo: from `code{1,2,3,4,5}` to `\code{1,2,3,4,5}`

* pkg logo:
    + improved logo with background transparency - better looking favicons

* README.md:
    + changed section "First Things First" to "Allowing Less Secure Apps Access"


***

## mRpostman 0.2.0 (2019-08-18 - CRAN submission)

- changed function name patterns to those specified in the tidyverse style guide

* changed return from mailboxes operation functions and some miscellanea functions: 
    - now `select_mailbox()`, `rename_mailbox()`, `copy_msg()`, `move_msg()`, `delete_msg()`, 
`expunge()`, `add/remove/replace_flags()` outputs are invisible and only return 
`imapconf` or a list (imapconf+msg_ids).

- changed package logo

***

## mRpostman 0.1.0 (2019-08-13 - Github release)
