
<!-- # mRpostman -->

# mRpostman <img src="man/figures/logo.png" align="right" width="138" />

<!-- # mRpostman <img src="man/figures/logo.png" align="right" /> -->

<!-- [![Downloads](http://cranlogs.r-pkg.org/badges/mRpostman?color=brightgreen)](http://www.r-pkg.org/pkg/mRpostman) -->

<!-- one space after links to display badges side by side -->

<!-- [![Travis-CI Build Status](https://travis-ci.org/allanvc/mRpostman.svg?branch=master)](https://travis-ci.org/allanvc/mRpostman)  -->

<!-- badges: start -->

[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/mRpostman)](https://cran.r-project.org/package=mRpostman)
[![Downloads from the RStudio CRAN
mirror](https://cranlogs.r-pkg.org/badges/mRpostman)](https://cran.r-project.org/package=mRpostman)
[![Downloads from the RStudio CRAN
mirror](https://cranlogs.r-pkg.org/badges/grand-total/mRpostman)](https://cran.r-project.org/package=mRpostman)
[![CRAN/METACRAN](https://img.shields.io/cran/l/mRpostman)](https://opensource.org/license/gpl-3-0)
[![R-CMD-check](https://github.com/allanvc/mRpostman/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/allanvc/mRpostman/actions/workflows/R-CMD-check.yaml)
<!-- [![Codecov test coverage](https://codecov.io/gh/allanvc/mRpostman/branch/master/graph/badge.svg)](https://app.codecov.io/gh/allanvc/mRpostman?branch=master) -->
<!-- badges: end -->

An IMAP Client for R

## Overview

`mRpostman` is a session-based IMAP client that implements the full
command sets of the IMAP4rev2 (RFC 9051) and IMAP4rev1 (RFC 3501)
protocols, along with the optional extensions registered with IANA,
allowing you to perform virtually all e-mail operations from within R.
The aim of this package is to pave the way for email data analysis in R.
To do so, `mRpostman` makes extensive use of the {curl} package and the
libcurl C library.

`mRpostman`’s official website: <https://allanvc.github.io/mRpostman/>

Cite `mRpostman`: A. V. C. Quadros, “mRpostman: An IMAP Client for R”,
Journal of Open Research Software, vol. 12, no. 1, p. 4, 2024, doi:
10.5334/jors.480. [http](https://doi.org/10.5334/jors.480). Refer to
`citation("mRpostman")`.

**IMPORTANT**:

1.  Old versions of the libcurl C library ({curl}’s main engine) will
    cause the malfunction of this package. If your libcurl’s version is
    above 7.58.0, you should be fine. In case you intend to use OAuth
    2.0 authentication, then you will need libcurl \>= 7.65.0. To learn
    more about the OAuth 2.0 authentication in this package, refer to
    the [*“Using IMAP OAuth2.0 authentication in
    mRpostman”*](https://allanvc.github.io/mRpostman/articles/xoauth2.0.html)
    vignette.

2.  Most mail providers discontinued less secure apps access. If it is
    still available and you are comfortable with this type of access you
    can enable this option for your account on your mail provider. Some
    providers, such as Yahoo Mail, also offer the option to generate a
    password to be used by third-party apps such as mRpostman. The other
    option, as mentioned above, is to set up OAuth2 (two-factor
    authentication) in order to access your mailbox. Please also refer
    to the [*“Using IMAP OAuth2.0 authentication in
    mRpostman”*](https://allanvc.github.io/mRpostman/articles/xoauth2.0.html)
    vignette.

## Providers and their IMAP urls

| **Provider**                       | **IMAP Server**           |
| ---------------------------------- | ------------------------- |
| Gmail                              | `imap.gmail.com`          |
| Office 365                         | `outlook.office365.com`\* |
| Outlook.com (Hotmail and Live.com) | `imap-mail.outlook.com`   |
| Yahoo Mail                         | `imap.mail.yahoo.com`     |
| iCloud Mail                        | `imap.mail.me.com`        |
| AOL Mail                           | `imap.aol.com`            |
| Zoho Mail                          | `imap.zoho.com`           |
| Yandex Mail                        | `imap.yandex.com`         |
| GMX Mail                           | `imap.gmx.com`            |
| Mail.com                           | `imap.mail.com`           |
| FastMail                           | `imap.fastmail.com`       |

\* For Office 365 accounts, the `username` should be set as
`user@yourcompany.com` or `user@youruniversity.edu` for example.

## Introduction

From version 0.9.0.0 onward, `mRpostman` is implemented under the OO
paradigm, based on an R6 class called `ImapCon`. Its derived methods,
and a few independent functions enable the R user to perform a myriad of
IMAP commands.

Below, we present all the available methods and functions, grouped by
type of operation:

  - **configuration and connection methods**: `configure_imap()`,
    `disconnect()`, `noop()`, `reset_url()`, `reset_username()`,
    `reset_password()`, `reset_verbose()`, `reset_use_ssl()`,
    `reset_buffersize()`, `reset_timeout_ms()`,
    `reset_xoauth2_bearer()`;
  - **server information methods**: `list_server_capabilities()`,
    `enable()`, `id()`, `namespace()`, `get_quota_root()`,
    `get_quota()`, `set_quota()`, `get_metadata()`, `set_metadata()`;
  - **mailbox operations methods**: `list_mail_folders()`,
    `list_subscribed_folders()`, `list_folders_status()`,
    `list_special_use_folders()`, `get_acl()`, `set_acl()`,
    `delete_acl()`, `list_rights()`, `my_rights()`, `select_folder()`,
    `resync_folder()`, `fetch_changes()`, `examine_folder()`,
    `status()`, `create_folder()`, `rename_folder()`, `delete_folder()`,
    `subscribe_folder()`, `unsubscribe_folder()`, `close_folder()`,
    `unselect_folder()`, `check()`, `list_flags()`;
  - **single-search methods**: `search_before()`, `search_since()`,
    `search_period()`, `search_on()`,
    `search_sent_before()`,`search_sent_since()`,
    `search_sent_period()`, `search_sent_on()`, `search_string()`,
    `search_flag()`, `search_smaller_than()`, `search_larger_than()`,
    `search_younger_than()`, `search_older_than()`;
  - **the custom-search method and its helper functions**: `search()`;
      - relational operators functions: `AND()`, `OR()`;
      - criteria definition functions: `before()`, `since()`, `on()`,
        `sent_before()`, `sent_since()`, `sent_on()`, `string()`,
        `flag()`, `smaller_than()`, `larger_than()`, `younger_than()`,
        `older_than()`, `saved_before()`, `saved_since()`, `saved_on()`,
        `modseq()`;
  - **server-side sort and thread methods**: `sort()`, `thread()`;
  - **fetch methods**: `fetch_body()`, `fetch_header()`, `fetch_text()`,
    `fetch_metadata()`, `fetch_envelope()`, `fetch_bodystructure()`,
    `fetch_preview()`, `fetch_binary()`, `metadata_options()`,
    `fetch_attachments_list()`, `fetch_attachments()`,
    `fetch_attachment_parts()`;
  - **attachments methods**: `list_attachments()`, `get_attachments()`,
    `fetch_attachments_list()`, `fetch_attachments()`;
  - **complementary methods**: `copy_msg()`, `move_msg()`,
    `append_msg()`, `append_msgs()`, `append_catenate()`, `idle()`,
    `notify()`, `esearch_min_id()`, `esearch_max_id()`,
    `esearch_count()`, `delete_msg()`, `expunge()`, `add_flags()`,
    `remove_flags()`, `replace_flags()`;
  - **MIME-decoding and message-text helper functions**:
    `decode_mime_header()`, `clean_msg_text()`, `parse_envelope()`,
    `parse_bodystructure()`, `imap_utf7_encode()`, `imap_utf7_decode()`,
    `imap_url()`.

## Supported IMAP commands and capabilities

The IMAP protocol has a **mandatory core** — the IMAP4rev1 commands
defined in [RFC 3501](https://datatracker.ietf.org/doc/html/rfc3501),
revised and consolidated by IMAP4rev2 in
[RFC 9051](https://datatracker.ietf.org/doc/html/rfc9051), which every
compliant server must implement — plus a set of **optional extensions**,
each advertised by the server in its `CAPABILITY` response. `mRpostman`
covers both. For the extension-based methods, `mRpostman` checks the
server’s advertised capabilities and, if the required one is missing,
raises an informative error instead of letting the server reply with a
cryptic `BAD Unknown command`. You can inspect what your server supports
with `list_server_capabilities()`.

### Core commands (RFC 3501 — always available)

| IMAP command                   | `mRpostman` method(s)                                                                       |
| ------------------------------ | ------------------------------------------------------------------------------------------- |
| `CAPABILITY`                   | `list_server_capabilities()`                                                                |
| `NOOP`                         | `noop()`                                                                                    |
| `CHECK`                        | `check()`                                                                                   |
| `LOGIN` / `AUTHENTICATE`       | `configure_imap()`                                                                          |
| `LOGOUT`                       | `disconnect()`                                                                              |
| `SELECT` / `EXAMINE`           | `select_folder()` / `examine_folder()`                                                      |
| `CREATE` / `DELETE` / `RENAME` | `create_folder()` / `delete_folder()` / `rename_folder()`                                   |
| `SUBSCRIBE` / `UNSUBSCRIBE`    | `subscribe_folder()` / `unsubscribe_folder()`                                               |
| `LIST` / `LSUB`                | `list_mail_folders()` / `list_subscribed_folders()`                                         |
| `STATUS`                       | `status()`                                                                                  |
| `APPEND`                       | `append_msg()`                                                                              |
| `SEARCH`                       | `search()`, `search_before()`, `search_since()`, `search_string()`, … (all `search_*`)      |
| `FETCH`                        | `fetch_body()`, `fetch_header()`, `fetch_text()`, `fetch_metadata()`, `fetch_attachments()` |
| `STORE`                        | `add_flags()`, `remove_flags()`, `replace_flags()`                                          |
| `COPY`                         | `copy_msg()`                                                                                |
| `CLOSE`                        | `close_folder()`                                                                            |
| `EXPUNGE`                      | `expunge()`, `delete_msg()`                                                                 |

### Optional extensions (server-dependent — capability-checked)

| IMAP command                                                                                                                                | `mRpostman` method(s)                                                                                                                                                                                               | Capability                                                    | RFC                                                                                                           |
| ------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------- |
| `SORT`                                                                                                                                      | `sort()`                                                                                                                                                                                                            | `SORT`                                                        | [5256](https://datatracker.ietf.org/doc/html/rfc5256)                                                         |
| `THREAD`                                                                                                                                    | `thread()`                                                                                                                                                                                                          | `THREAD=REFERENCES` / `THREAD=ORDEREDSUBJECT` / `THREAD=REFS` | [5256](https://datatracker.ietf.org/doc/html/rfc5256)                                                         |
| `GETQUOTA` / `GETQUOTAROOT`                                                                                                                 | `get_quota()` / `get_quota_root()`                                                                                                                                                                                  | `QUOTA`                                                       | [9208](https://datatracker.ietf.org/doc/html/rfc9208)                                                         |
| `NAMESPACE`                                                                                                                                 | `namespace()`                                                                                                                                                                                                       | `NAMESPACE`                                                   | [2342](https://datatracker.ietf.org/doc/html/rfc2342)                                                         |
| `ID`                                                                                                                                        | `id()`                                                                                                                                                                                                              | `ID`                                                          | [2971](https://datatracker.ietf.org/doc/html/rfc2971)                                                         |
| `UNSELECT`                                                                                                                                  | `unselect_folder()`                                                                                                                                                                                                 | `UNSELECT`                                                    | [3691](https://datatracker.ietf.org/doc/html/rfc3691)                                                         |
| `LIST` (special-use)                                                                                                                        | `list_special_use_folders()`                                                                                                                                                                                        | `SPECIAL-USE`                                                 | [6154](https://datatracker.ietf.org/doc/html/rfc6154)                                                         |
| `MOVE`                                                                                                                                      | `move_msg()`                                                                                                                                                                                                        | `MOVE`                                                        | [6851](https://datatracker.ietf.org/doc/html/rfc6851)                                                         |
| `SEARCH RETURN` (ESEARCH)                                                                                                                   | `search(esearch = TRUE)`, `esearch_count()`, `esearch_min_id()`, `esearch_max_id()`                                                                                                                                 | `ESEARCH`                                                     | [4731](https://datatracker.ietf.org/doc/html/rfc4731)                                                         |
| `UID EXPUNGE`, `APPENDUID` / `COPYUID`                                                                                                      | `expunge(msg_uid = ...)`, `append_msg()` (returns the UID), `copy_msg()` / `move_msg()` (`"copyuid"` attribute)                                                                                                     | `UIDPLUS`                                                     | [4315](https://datatracker.ietf.org/doc/html/rfc4315)                                                         |
| `LIST ... RETURN (STATUS ...)`                                                                                                              | `list_folders_status()`                                                                                                                                                                                             | `LIST-STATUS`                                                 | [5819](https://datatracker.ietf.org/doc/html/rfc5819)                                                         |
| `SETQUOTA`                                                                                                                                  | `set_quota()`                                                                                                                                                                                                       | `QUOTA`                                                       | [9208](https://datatracker.ietf.org/doc/html/rfc9208)                                                         |
| `GETACL` / `SETACL` / `DELETEACL` / `LISTRIGHTS` / `MYRIGHTS`                                                                               | `get_acl()` / `set_acl()` / `delete_acl()` / `list_rights()` / `my_rights()`                                                                                                                                        | `ACL`                                                         | [4314](https://datatracker.ietf.org/doc/html/rfc4314)                                                         |
| `ENABLE`                                                                                                                                    | `enable()`                                                                                                                                                                                                          | `ENABLE`                                                      | [5161](https://datatracker.ietf.org/doc/html/rfc5161)                                                         |
| `SEARCH RETURN (SAVE)`                                                                                                                      | `search(save = TRUE)`, then `msg_id = "$"` in fetch/flag/copy/move/delete methods                                                                                                                                   | `SEARCHRES`                                                   | [5182](https://datatracker.ietf.org/doc/html/rfc5182)                                                         |
| `SORT RETURN (...)`                                                                                                                         | `sort(return = ...)`                                                                                                                                                                                                | `ESORT`                                                       | [5267](https://datatracker.ietf.org/doc/html/rfc5267)                                                         |
| `LIST ... RETURN (CHILDREN SUBSCRIBED SPECIAL-USE)`                                                                                         | `list_mail_folders(detailed = TRUE)`                                                                                                                                                                                | `LIST-EXTENDED`                                               | [5258](https://datatracker.ietf.org/doc/html/rfc5258)                                                         |
| `STATUS (SIZE)`                                                                                                                             | `status(items = "SIZE")`, `list_folders_status(items = "SIZE")`                                                                                                                                                     | `STATUS=SIZE`                                                 | [8438](https://datatracker.ietf.org/doc/html/rfc8438)                                                         |
| `FETCH (PREVIEW)`                                                                                                                           | `fetch_preview()`, `fetch_metadata(attribute = "PREVIEW")`                                                                                                                                                          | `PREVIEW`                                                     | [8970](https://datatracker.ietf.org/doc/html/rfc8970)                                                         |
| `FETCH (SAVEDATE)`, `SEARCH SAVEDBEFORE/SAVEDON/SAVEDSINCE`                                                                                 | `fetch_metadata(attribute = "SAVEDATE")`, `saved_before()` / `saved_on()` / `saved_since()`                                                                                                                         | `SAVEDATE`                                                    | [8514](https://datatracker.ietf.org/doc/html/rfc8514)                                                         |
| `SELECT (CONDSTORE)`, `STATUS (HIGHESTMODSEQ)`, `FETCH (MODSEQ)`, `FETCH ... (CHANGEDSINCE)`, `STORE ... (UNCHANGEDSINCE)`, `SEARCH MODSEQ` | `select_folder(condstore = TRUE)`, `status(items = "HIGHESTMODSEQ")`, `fetch_metadata(attribute = "MODSEQ", changed_since = )`, `add_flags()`/`replace_flags()`/`remove_flags()` `(unchanged_since = )`, `modseq()` | `CONDSTORE`                                                   | [7162](https://datatracker.ietf.org/doc/html/rfc7162)                                                         |
| `SELECT (QRESYNC ...)`, `UID FETCH ... (CHANGEDSINCE VANISHED)`                                                                             | `resync_folder()`, `fetch_changes()`                                                                                                                                                                                | `QRESYNC`                                                     | [7162](https://datatracker.ietf.org/doc/html/rfc7162)                                                         |
| `GETMETADATA` / `SETMETADATA`                                                                                                               | `get_metadata()` / `set_metadata()`                                                                                                                                                                                 | `METADATA`                                                    | [5464](https://datatracker.ietf.org/doc/html/rfc5464)                                                         |
| `IDLE`                                                                                                                                      | `idle()` (on a dedicated second connection)                                                                                                                                                                         | `IDLE`                                                        | [2177](https://datatracker.ietf.org/doc/html/rfc2177)                                                         |
| `APPEND` (multiple literals)                                                                                                                | `append_msgs()`                                                                                                                                                                                                     | `MULTIAPPEND`                                                 | [3502](https://datatracker.ietf.org/doc/html/rfc3502)                                                         |
| `NOTIFY SET` / `NOTIFY NONE`                                                                                                                | `notify()`                                                                                                                                                                                                          | `NOTIFY`                                                      | [5465](https://datatracker.ietf.org/doc/html/rfc5465)                                                         |
| `FETCH (BINARY.PEEK[...])`                                                                                                                  | `fetch_binary()`                                                                                                                                                                                                    | `BINARY`                                                      | [3516](https://datatracker.ietf.org/doc/html/rfc3516)                                                         |
| `APPEND ... CATENATE`                                                                                                                       | `append_catenate()`, `imap_url()`                                                                                                                                                                                   | `CATENATE`                                                    | [4469](https://datatracker.ietf.org/doc/html/rfc4469)                                                         |
| `COMPRESS DEFLATE`                                                                                                                          | `compress = TRUE` in the raw-socket methods                                                                                                                                                                         | `COMPRESS=DEFLATE`                                            | [4978](https://datatracker.ietf.org/doc/html/rfc4978)                                                         |
| `LIST ... RETURN (MYRIGHTS)`                                                                                                                | `list_mail_folders(detailed = TRUE)` (`my_rights` column)                                                                                                                                                           | `LIST-MYRIGHTS`                                               | [8440](https://datatracker.ietf.org/doc/html/rfc8440)                                                         |
| `CREATE ... (USE (...))`                                                                                                                    | `create_folder(special_use = ...)`                                                                                                                                                                                  | `CREATE-SPECIAL-USE`                                          | [6154](https://datatracker.ietf.org/doc/html/rfc6154)                                                         |
| `AUTHENTICATE OAUTHBEARER`                                                                                                                  | `configure_imap(oauth_mechanism = "OAUTHBEARER")`                                                                                                                                                                   | `AUTH=OAUTHBEARER`                                            | [7628](https://datatracker.ietf.org/doc/html/rfc7628)                                                         |
| `SORT` (display keys)                                                                                                                       | `sort(by = "DISPLAYFROM"/"DISPLAYTO")`                                                                                                                                                                              | `SORT=DISPLAY`                                                | [5957](https://datatracker.ietf.org/doc/html/rfc5957)                                                         |
| `SEARCH RETURN (PARTIAL m:n)`                                                                                                               | `esearch_partial()`                                                                                                                                                                                                 | `PARTIAL` / `CONTEXT=SEARCH`                                  | [9394](https://datatracker.ietf.org/doc/html/rfc9394) / [5267](https://datatracker.ietf.org/doc/html/rfc5267) |
| `SORT RETURN (PARTIAL m:n)`                                                                                                                 | `esort_partial()` \*                                                                                                                                                                                                | `CONTEXT=SORT`                                                | [5267](https://datatracker.ietf.org/doc/html/rfc5267)                                                         |
| `REPLACE`                                                                                                                                   | `replace_msg()` \*                                                                                                                                                                                                  | `REPLACE`                                                     | [8508](https://datatracker.ietf.org/doc/html/rfc8508)                                                         |
| `FETCH (EMAILID THREADID)`, `STATUS (MAILBOXID)`                                                                                            | `fetch_objectid()` \*, `status(items = "MAILBOXID")` \*                                                                                                                                                             | `OBJECTID`                                                    | [8474](https://datatracker.ietf.org/doc/html/rfc8474)                                                         |
| `UIDBATCHES`                                                                                                                                | `uid_batches()` \*                                                                                                                                                                                                  | `UIDBATCHES`                                                  | [10022](https://datatracker.ietf.org/doc/html/rfc10022)                                                       |
| `ESEARCH IN (...)`                                                                                                                          | `esearch_multi()` \*                                                                                                                                                                                                | `MULTISEARCH`                                                 | [7377](https://datatracker.ietf.org/doc/html/rfc7377)                                                         |
| `UNAUTHENTICATE`                                                                                                                            | `unauthenticate()` \*                                                                                                                                                                                               | `UNAUTHENTICATE`                                              | [8437](https://datatracker.ietf.org/doc/html/rfc8437)                                                         |
| `LANGUAGE` / `COMPARATOR`                                                                                                                   | `language()` \* / `comparator()` \*                                                                                                                                                                                 | `LANGUAGE` / `I18NLEVEL=2`                                    | [5255](https://datatracker.ietf.org/doc/html/rfc5255)                                                         |
| `GENURLAUTH` / `URLFETCH`                                                                                                                   | `genurlauth()` \* / `urlfetch()` \*                                                                                                                                                                                 | `URLAUTH`                                                     | [4467](https://datatracker.ietf.org/doc/html/rfc4467)                                                         |
| `CONVERT`                                                                                                                                   | `fetch_convert()` \*                                                                                                                                                                                                | `CONVERT`                                                     | [5259](https://datatracker.ietf.org/doc/html/rfc5259)                                                         |
| `FETCH ANNOTATION` / `STORE ANNOTATION`                                                                                                     | `fetch_annotation()` \* / `store_annotation()` \*                                                                                                                                                                   | `ANNOTATE-EXPERIMENT-1`                                       | [5257](https://datatracker.ietf.org/doc/html/rfc5257)                                                         |
| `SEARCH ... FUZZY`                                                                                                                          | `fuzzy()` criterion modifier \*                                                                                                                                                                                     | `SEARCH=FUZZY`                                                | [6203](https://datatracker.ietf.org/doc/html/rfc6203)                                                         |
| `SEARCH ... FILTER`                                                                                                                         | `filter_stored()` criterion \*                                                                                                                                                                                      | `FILTERS`                                                     | [5466](https://datatracker.ietf.org/doc/html/rfc5466)                                                         |
| `APPEND` size guard, `STATUS (APPENDLIMIT)`                                                                                                 | automatic in `append_msg()`/`append_msgs()`; `status(items = "APPENDLIMIT")`                                                                                                                                        | `APPENDLIMIT`                                                 | [7889](https://datatracker.ietf.org/doc/html/rfc7889)                                                         |
| non-synchronizing literals                                                                                                                  | automatic on the raw-socket methods                                                                                                                                                                                 | `LITERAL+` / `LITERAL-`                                       | [7888](https://datatracker.ietf.org/doc/html/rfc7888)                                                         |

Every capability registered with IANA is covered. The methods marked
with an asterisk (\*) are **experimental**: they follow the RFC
grammars, but no widely deployed server advertises those capabilities
(they are rare, brand new, or were never adopted), so they could not be
exercised against a live server. Availability of the others varies by
provider: Gmail, for instance, supports every non-experimental extension
above **except `SORT` and `THREAD`**, which it has never implemented; to
exercise `sort()` and `thread()` you need a server that advertises them
(e.g. Dovecot-based hosts, Yandex, or Outlook/Office 365).
Announcement-only capabilities (`AUTH=`, `LOGINDISABLED`, `RIGHTS=`,
`QUOTA=`, `APPENDLIMIT=n`, `MESSAGELIMIT=`/`SAVELIMIT=`, `IMAPSIEVE=`,
`JMAPACCESS`, `INPROGRESS`, `CHILDREN`, `I18NLEVEL=1`, referrals)
require no dedicated command and are honored where they matter (folder
listings, appends, error reporting). `UIDONLY` (RFC 9586) responses
(`UIDFETCH`) are understood after `enable("UIDONLY")`. The raw-socket
methods (`idle()`, `notify()`, `append_msgs()`, `append_catenate()`,
`fetch_binary()`, `replace_msg()`, `esearch_multi()`, `urlfetch()`,
`fetch_convert()`) need an `imaps://` URL for TLS, since STARTTLS is not
available on that connection.

## Installation

``` r
# CRAN version
install.packages("mRpostman")

# Dev version
if (!require('remotes')) install.packages('remotes')
remotes::install_github("allanvc/mRpostman")
```

## Trying it without a mail account

The package ships a disposable local IMAP server (Dovecot, in a Docker
container) plus a deterministic synthetic corpus generator, so every
feature can be exercised offline and reproducibly — no credentials,
OAuth2 setup, or provider rate limits involved:

``` r
# after starting the container (see the "sandbox" vignette):
con <- configure_imap(url = "imap://localhost:1430", username = "testuser",
                      password = "sandbox", use_ssl = FALSE)
populate_sandbox(con, n = 200) # uploads the corpus with the package's own APPEND
```

Real data works too: `ingest_maildir()` uploads any local maildir-style
directory to the server via `APPEND`, and `enron_sandbox()` builds on it
to download (once, with consent, cached) the public Enron corpus and
ingest a subset selected by custodian, folder, and date — turning the
sandbox into a full e-mail data-analysis laboratory.

See the [*“A reproducible IMAP sandbox with
Docker”*](https://allanvc.github.io/mRpostman/articles/sandbox.html)
vignette for the guided tour. The sandbox’s Dovecot server also
advertises `SORT` and `THREAD`, making it a convenient place to try the
extensions your provider may lack.

## Basic Usage

### 1\) Configure an IMAP connection and list the server’s capabilities

``` r

library(mRpostman)

# Outlook - Office 365
con <- configure_imap(url="imaps://outlook.office365.com",
                      username="your_user@company.com",
                      password=rstudioapi::askForPassword()
)

# other IMAP providers that were tested: Hotmail ("imaps://imap-mail.outlook.com"),
#  Gmail (imaps://imap.gmail.com), Yahoo (imaps://imap.mail.yahoo.com/), 
#  AOL (imaps://export.imap.aol.com/), Yandex (imaps://imap.yandex.com)

# Other non-tested mail providers should work as well

con$list_server_capabilities()
```

### 2\) List mail folders and select “INBOX”

``` r

# Listing
con$list_mail_folders()

# Selecting
con$select_folder(name = "INBOX")
```

### 3\) Search messages by date

``` r

res1 <- con$search_on(date_char = "02-Jan-2020")

res1
```

### 4\) Customizing a search with multiple criteria

Executing a search by string:

``` r

# messages that contain either "@k-state.edu" OR "ksu.edu" in the "TO" header field
res2 <- con$search(OR(
  string(expr = "@k-state.edu", where = "TO"),
  string(expr = "@ksu.edu", where = "TO")
))

res2
```

### 5\) Fetch messages’ text using single-search results

``` r

res3 <- con$search_string(expr = "Welcome!", where = "SUBJECT") %>%
  con$fetch_text(write_to_disk = TRUE) # also writes results to disk

res3
```

### 6\) Attachments

You can list the attachments of one or more messages with:

1)  the `list_attachments()` function:

<!-- end list -->

``` r

con$search_since(date_char = "02-Jan-2020") %>%
  con$fetch_text() %>% # or with fetch_body()
  list_attachments() # does not depend on the 'con' object
```

… or more directly with:

2)  `fetch_attachments_list()`

<!-- end list -->

``` r

con$search_since(date_char = "02-Jan-2020") %>%
  con$fetch_attachments_list()
```

If you want to download the attachments of one or more messages, there
are also two ways of doing that.

1)  Using the `get_attachments()` method:

<!-- end list -->

``` r

con$search_since(date_char = "02-Jan-2020") %>%
  con$fetch_text() %>% # or with fetch_body()
  con$get_attachments()
```

… and more directly with the

2)  `fetch_attachments()` method:

<!-- end list -->

``` r

con$search_since(date_char = "02-Jan-2020") %>%
  con$fetch_attachments()
```

## Future Improvements

  - add further IMAP features;
  - eliminate the {stringr} dependency in REGEX;
  - implement a progress bar in fetch operations;

## Known bugs

  - *search results truncation*: This is a [libcurl’s known
    bug](https://curl.se/docs/knownbugs.html#IMAP_SEARCH_ALL_truncated_respon)
    which causes the search results to be truncated when there is a
    large number of message ids returned. To circumvent this problem,
    you can set a higher `buffersize` value, increasing the buffer
    capacity, and `verbose = TRUE` for monitoring the server response
    for truncated results when executing a search. When possible,
    `mRpostman` tries to issue a warning for possible truncated values.

  - *`verbose = TRUE` malfunction on Windows*: This seems to be related
    to the [{curl} R
    package](https://github.com/jeroen/curl/issues/230). When using the
    `verbose = TRUE` on Windows, the flow of information between the
    IMAP server and the R session presents an intermittent behavior,
    which causes it to not be shown on the console, or with a
    considerable delay.

  - *shared mailbox access not working*: This seems to be another
    [libcurl’s bug](https://github.com/allanvc/mRpostman/issues/2),
    although more tests need to be done to confirm it. It does not allow
    the user to connect to a shared mailbox. To circumvent this, if the
    shared mailbox has a password associated with it, you can try a
    direct regular connection.

  - *`xoauth2_bearer` SASL error*: This is related to [old libcurl’s
    versions](https://curl.se/bug/?i=2487) which causes the access token
    to not be properly passed to the server. This bug was fixed in
    libcurl 7.65.0. The problem is that many Linux distributions, such
    as Ubuntu 18.04, still provide libcurl 7.58.0 in their official
    distribution (libcurl4-openssl-dev). If you use a newer Linux distro
    such as Ubuntu 20.04, you should be fine as the distributed
    libcurl’s version will be above 7.65.0. Another alternative is to
    use plain authentication instead of OAuth2.0.

## License

This package is licensed under the terms of the GPL-3 License.

## References

Crispin, M. (2003), *INTERNET MESSAGE ACCESS PROTOCOL - VERSION 4rev1*,
RFC 3501, March 2003, [http](https://www.rfc-editor.org/rfc/rfc3501).

Heinlein, P. and Hartleben, P. (2008). *The Book of IMAP: Building a
Mail Server with Courier and Cyrus*. No Starch Press. ISBN
978-1-59327-177-0.

Ooms, J. (2020), *curl: A Modern and Flexible Web Client for R*. R
package version 4.3, [http](https://CRAN.R-project.org/package=curl).

Quadros, A. V. C. *mRpostman: An IMAP Client for R*, Journal of Open
Research Software, vol. 12, no. 1, p. 4, 2024, doi: 10.5334/jors.480.
[http](https://doi.org/10.5334/jors.480).

Stenberg, D. *Libcurl - The Multiprotocol File Transfer Library*,
[http](https://curl.se/libcurl/).
