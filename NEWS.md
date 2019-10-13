## mRpostman 0.3.0 (2019-10-07)


### Features

* get_attachments() function:
+ added get_attachment()
+ added helper functions: has_attachment(), check_args_get_attachment()

* list_attachments() function:
+ added list_attachments()

* loop_fetch_XXXX() functions:
+ error msg: fixed from examineMailbox() to examine_mailbox()
+ fixed "forbiden[_]char[s]" in loop_fetch_full_msg()
+ changed forbiden_chars to "[\\/:*?\"<>|]", considering WIN-*NIX-OSX cases

* fetch_full_msg():
+ added warning message related to get_attachment() when  "keep_in_mem = TRUE"

* fetch_XXXX_XXXX() functions:
+ added attr(msg_list, which = 'mbox') = new_imapconf$mbox to all -- to be used by get_attachment()

* list_server_capabilities() function:
+ converted regex match/extraction from stringr to base R

* examples:
+ fixed typo - from "configureIMAP" to "configure_imap" in the following functions examples: list_mailboxes(), list_server_capabilities()
+ changed all imaps://imap.gmail.com server examples to a generic one: imaps://your.imap.server.com


### Documentation

* DESCRIPTION FILE:
+ changed title to IMAP Toolkit
+ changed Description field

* README.md:
+ changed dev installation example from 'devtools' to 'remotes' because the former depends on 'curl'
+ fixed typo - from "configureIMAP" to "configure_imap" in README.md
+ fixed typo - two AOL examples instead of one AOL and one Yahoo configure_imap example
+ added Yandex configure_imap() example

* basics.Rmd vignette:
+ fixed typo - from "configureIMAP" to "configure_imap" in README.md
+ fixed typo - two AOL examples instead of one AOL and one Yahoo configure_imap example
+ added Yandex configure_imap() example
+ removed figures (already appear on README.md)

* count_msgs()
+ changed title

---
## mRpostman 0.2.1-X (2019-08-22 Github only)


### Features (0.2.1-2)
* loop-fetch-XXXX() functions for fetching msgs:
+ added select_mailbox(imapconf = new_imapconf, mbox = new_imapconf$mbox) inside
the tryCatch in the while loop -- it prevents errors specially when messages have
larger attachments taking too much time to fetch. In some IMAP servers (such as 
Yandex) it may lose the mbox selection. Other alternatives: set a larger 
timeout_ms in configure_imap.

* list_mailboxes():
+ REGEX for cleaning mbox names from list_mailboxes() was modified to deal with 
the return of Yandex 
IMAP server: from '.*\" \"*(.*?)\\"\r\n' to '.*\" \"*(.*?)[(\\"\r\n)|(\r\n\\*)]'


### Documentation (0.2.1-1)
* function configure_imap():
+ @param retries description typo: from "retires" to "retries"

* internal function conifg_handle():
+ removed @family config, so it does not appear as "see also" in config_IMAP()

* function flag_options():
+ updated @note to be more clear

* all internal functions from @family check args search:
+ @param esearch typo: from code{1,2,3,4,5} to \code{1,2,3,4,5}

* pkg logo:
+ improved logo with background transparency - better looking favicons

* README.md:
+ changed section "First Things First" to "Allowing Less Secure Apps Access"



---
## mRpostman 0.2.0 (2019-08-18 - CRAN submission)

- changed function name patterns to those specified in the tidyverse style guide

- changed return from mailboxes operation functions and some miscellanea functions: 
now select_mailbox(), rename_mailbox(), copy_msg(), move_msg(), delete_msg(), 
expunge(), add/remove/replace_flags() returns are invisible and only return 
imapconf or a list (imapconf+msg_ids).

- changed package logo



---
## mRpostman 0.1.0 (2019-08-13 - Github release)
