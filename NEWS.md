## mRpostman 2.3.0 (2026-08-28 feature update)

### New features

- **The query language**: new `ImapCon$query()` searches with an ordinary R expression, e.g. `con$query((subject == "budget" | "budget 3") & flag != "SEEN" & size > 5e6)`. Fields: `subject`, `from`, `to`, `cc`, `bcc`, `body`, `text` (`==` means contains), `flag` (system flags and custom keywords, `!=` maps to the `UN*` forms), `size` in bytes and `age` in seconds (`>`, `>=`, `<`, `<=`), the date fields `sent`, `date`, and `saved` (`>=`, `>`, `<`, `<=`, `==`, exact at day granularity), and `header("Name")`. Expressions combine with `&`, `|`, `!`, `%in%` (one field, several values), and parentheses, with R's own precedence; variables and criterion constructors are evaluated in the caller's environment, and a bare string next to `|` inherits the field of the preceding comparison. The translator is exported as the pure function `imap_query()`, so queries can be inspected, and tested, offline.
- **An actionable error for oversized results**: a `SEARCH` matching many thousands of messages can produce an id list larger than libcurl accepts in one response line (`CURLE_TOO_LARGE`); this now fails with a message recommending `esearch = TRUE`, the `esearch_*()` aggregations, or narrower criteria, instead of a generic request error.
- **Native operators on criteria**: the criterion constructors (`string()`, `flag()`, `sent_since()`, ...) now return classed objects that combine with `&`, `|`, and `!` directly, e.g. `con$search(string("budget", where = "SUBJECT") & !flag("SEEN"))`. `AND()` and `OR()` keep working unchanged.

## mRpostman 2.2.0 (2026-08-27 feature update)

### New features

A sweep of the IANA IMAP capability registry: every registered capability the package did not yet cover is now implemented.

Exercised against a live server (Dovecot sandbox):

- **SORT=DISPLAY** (RFC 5957): `sort()` and `esort_partial()` accept the `DISPLAYFROM` and `DISPLAYTO` keys (sorting by the display name of the address).
- **THREAD=REFS**: `thread(algorithm = "REFS")` (Dovecot's threading by references only).
- **PARTIAL** (RFC 9394) / **CONTEXT=SEARCH** (RFC 5267): new `ImapCon$esearch_partial()` returns only one slice `m:n` of the result set (negative positions count from the most recent match).
- **LITERAL+ / LITERAL-** (RFC 7888): the raw socket layer now sends non-synchronizing `{n+}` literals in a single write when the server allows it, sparing one round trip per literal in `append_msgs()`, `append_catenate()`, and `replace_msg()`.
- **APPENDLIMIT** (RFC 7889): `append_msg()`, `append_msgs()`, and `replace_msg()` fail before uploading a message larger than the server's advertised limit, and `status(items = "APPENDLIMIT")` queries the per-mailbox limit.

Implemented from the RFC grammars and marked **experimental** in the documentation, since no server available for validation advertises these capabilities (they are rare, brand new, or were never adopted):

- `esort_partial()` (**CONTEXT=SORT**, RFC 5267), `replace_msg()` (**REPLACE**, RFC 8508), `fetch_objectid()` and `status(items = "MAILBOXID")` (**OBJECTID**, RFC 8474), `uid_batches()` (**UIDBATCHES**, RFC 10022), `esearch_multi()` (**MULTISEARCH**, RFC 7377), `unauthenticate()` (RFC 8437), `language()` and `comparator()` (**LANGUAGE**/**I18NLEVEL=2**, RFC 5255), `genurlauth()` and `urlfetch()` (**URLAUTH**, RFC 4467), `fetch_convert()` (**CONVERT**, RFC 5259), `fetch_annotation()` and `store_annotation()` (**ANNOTATE-EXPERIMENT-1**, RFC 5257), and the search criterion helpers `fuzzy()` (**SEARCH=FUZZY**, RFC 6203) and `filter_stored()` (**FILTERS**, RFC 5466).

### Minor improvements

- The package now presents itself as an IMAP4rev2 and IMAP4rev1 client: version 2.1.0 had already completed everything RFC 9051 consolidates into the core (the last piece being BINARY), and 2.2.0 adds the remaining registered extensions.
- **UIDONLY** (RFC 9586) tolerance: `UIDFETCH` responses are understood by the fetch parsers after `enable("UIDONLY")`.
- QUOTA references updated from RFC 2087 to RFC 9208, which obsoleted it (the commands are unchanged).

## mRpostman 2.1.0 (2026-08-27 feature update)

### New features

All four remaining protocol extensions now run on the raw socket layer introduced in 2.0.0, each on a dedicated second connection and capability-checked:

- **NOTIFY** (RFC 5465): new `ImapCon$notify()` registers interest in events (`MessageNew`, `MessageExpunge`, `FlagChange`, `MailboxName`, ...) for the selected folder, every personal folder, the subscribed ones, or named folders, and collects what the server reports (`EXISTS`/`EXPUNGE`/`FETCH` for the selected folder, `STATUS` lines for the others, `LIST` lines for mailbox changes) until a timeout or a callback stops it; then `NOTIFY NONE`.

- **BINARY** (RFC 3516): new `ImapCon$fetch_binary()` fetches a message part with `BINARY.PEEK[<part>]`, so that the server reverses the base64/quoted-printable encoding and returns the bytes as a binary literal.

- **CATENATE** (RFC 4469): new `ImapCon$append_catenate()` appends a message assembled by the server from parts of stored messages (`imap_url()` objects, a new exported helper building RFC 5092 URLs) and client-supplied text, without downloading anything.

- **COMPRESS=DEFLATE** (RFC 4978): every raw-socket method (`idle()`, `notify()`, `append_msgs()`, `append_catenate()`, `fetch_binary()`) gained a `compress` argument; with `compress = TRUE` the second connection is switched to deflate in both directions (streaming zlib in `src/deflate_stream.c`).

- the raw session now reads server literals (`{n}` and `~{n}`), which `fetch_binary()` relies on.

### Tests

- the Docker sandbox enables Dovecot's `imap_zlib` plugin, so `COMPRESS=DEFLATE` is testable locally alongside `NOTIFY`, `BINARY`, and `CATENATE`; new offline tests cover the zlib streams and the `NOTIFY` event parsing (`test-raw-session.R`).

## mRpostman 2.0.0 (2026-08-27 major update)

### New features

- **A raw socket layer, and with it `IDLE`.** The package gains a small compiled component (`src/imap_socket.c`) that asks libcurl for a connection in `CONNECT_ONLY` mode, so that libcurl still performs the TCP connection and the TLS handshake (certificate verification, SNI, system CA bundle) but hands the established socket over, and a minimal IMAP session written in R on top of it (`R/raw-session.R`: tagged commands, `+` continuations, literals, `AUTHENTICATE PLAIN`/`XOAUTH2`/`OAUTHBEARER`). This is used only where libcurl's one-shot request model cannot go; everything else keeps using libcurl as before.

- new `ImapCon$idle()` method (`IDLE`, RFC 2177): opens a second, dedicated connection, selects the folder there, and collects the server's unsolicited notifications (`EXISTS`, `EXPUNGE`, `FETCH`, `RECENT`) until a timeout elapses or a callback returns `FALSE`; the `IDLE` command is renewed periodically (servers close long idles). The main connection stays free to fetch what the events announce. Requires an `imaps://` URL for TLS (STARTTLS is not available on the raw socket) or `use_ssl = FALSE` for a plain server such as the Docker sandbox.

- new `ImapCon$append_msgs()` method (`MULTIAPPEND`, RFC 3502): several messages appended in one command, one literal per message, returning the assigned UIDs; falls back to one `append_msg()` per message on servers without the capability.

- the credentials are now kept in a private field of the connection object (never printed, not part of `con_params`, cleared by `disconnect()`), so that the second connection can authenticate; `reset_password()`/`reset_xoauth2_bearer()` update it.

### Notes

- The compiled component links against the system libcurl (`pkg-config --libs libcurl`; Rtools' bundled libcurl on Windows). The same functionality has been proposed to the `curl` package as `curl_send()`/`curl_recv()`; once available there, `src/` will be removed.

## mRpostman 1.5.5 (2026-08-27 feature update)

### New features

- **CONDSTORE** (RFC 7162), completing the subset added in 1.5.2: `select_folder(condstore = TRUE)` issues `SELECT ... (CONDSTORE)`, and the folder's `HIGHESTMODSEQ` is now kept in `con$con_params$highestmodseq` after every selection that reports it; `fetch_metadata(changed_since = n)` returns only the messages modified after modification sequence `n` (`CHANGEDSINCE`); `add_flags()`, `replace_flags()`, and `remove_flags()` gained `unchanged_since = n` for conditional stores (`UNCHANGEDSINCE`), returning the ids the server refused in the `"modified"` attribute.

- **QRESYNC** (RFC 7162): new `ImapCon$resync_folder(name, uidvalidity, modseq)` selects a folder with `QRESYNC` and returns the UIDs expunged since the given state (`VANISHED (EARLIER)`) and the current flags of the messages changed since then; new `ImapCon$fetch_changes(modseq, vanished = TRUE)` reports the same for the selected folder (`UID FETCH 1:* (FLAGS MODSEQ) (CHANGEDSINCE n VANISHED)`). Both enable the extension once per connection, releasing the selected folder with `UNSELECT` when needed.

- **METADATA** (RFC 5464): new `ImapCon$get_metadata()` and `ImapCon$set_metadata()` read and write mailbox or server annotations (`GETMETADATA`/`SETMETADATA`), returning a data frame of entries and values; `NA` removes an entry.

### Improvements

- the per-connection handling of `ENABLE`d extensions introduced in 1.5.4 for `UTF8=ACCEPT` is now generic (`ensure_enabled()`) and used for `QRESYNC` as well.

### Tests

- the Docker sandbox enables Dovecot's `METADATA` support (`mail_attribute_dict`, `imap_metadata = yes`); new offline tests cover the `MODIFIED`, `HIGHESTMODSEQ`, `VANISHED`/changed-flags, and `METADATA` parsers (`test-round6.R`). Verified live against Dovecot (all) and Gmail (CONDSTORE).

## mRpostman 1.5.4 (2026-08-27 feature update)

### New features

- new `ImapCon$fetch_attachment_parts()` method: attachments fetched by MIME part, guided by the server's `BODYSTRUCTURE` (`fetch_bodystructure()`), each part retrieved with `BODY.PEEK[<part>]`, decoded from base64 or quoted-printable, and written to the same `<username>/<folder>/<msg id>` tree as `fetch_attachments()` (or returned as raw vectors with `local_dir = NULL`). Only the attachments are transferred, and no MIME boundary parsing of the fetched body is involved. Accepts the same `content_disposition` filter as `fetch_attachments()` (`"both"`, `"attachment"`, `"inline"`). Returns a manifest with the part, filename, type, size, and path of every file.

### Bug fixes

- **non-ASCII search terms on Gmail.** Gmail rejects a `CHARSET UTF-8` search (`BAD Could not parse command`) and only accepts UTF-8 terms after `ENABLE UTF8=ACCEPT` (RFC 6855), which in turn is accepted only with no folder selected and lasts for the current connection (Gmail closes the connection after any rejected command). The search methods now choose per server: on servers that advertise `UTF8=ACCEPT`, the extension is enabled once per connection (releasing and re-selecting the folder with `UNSELECT`) and the term is sent as is; on the others, `CHARSET UTF-8` is declared as before. A connection replaced by libcurl is detected and the extension re-enabled.

- **duplicate `APPEND` after a dropped connection.** When a reused connection turns out to be dead, libcurl re-sends the request on a fresh connection; for `APPEND` this stored the message twice whenever the server had accepted the first copy before closing (observed on Gmail after deleting the selected folder). `append_msg()` now sends a `NOOP` first whenever the connection may be stale (after the selected folder was deleted, closed, or unselected, or after a minute without server activity), so that any reconnection happens before the upload.

- `search_larger_than()`, `search_smaller_than()`, `search_younger_than()`, `search_older_than()` and the matching criterion constructors formatted large numbers in scientific notation (`LARGER 5e+06`), which servers reject; numbers are now written in full.

- `search_younger_than()`, `search_older_than()`, and the `younger_than()`/`older_than()` criteria are now gated on the `WITHIN` capability (Gmail does not implement it and answered `BAD Could not parse command`); likewise the `saved_*()` criteria on `SAVEDATE` and `modseq()` on `CONDSTORE`.

- `delete_folder()` on the currently selected folder now clears the selection recorded in the connection object.

- the libcurl debug callback introduced in 1.5.2 (which records the server's `NO`/`BAD` replies) tried to convert every debug event to text, including the raw TLS records libcurl also reports; on `imaps://` connections this printed harmless but noisy `embedded nul in string` errors after every command. Only text and header events are processed now.

### Improvements

- `configure_imap(oauth_mechanism = ...)` was verified live against Gmail: both `XOAUTH2` and `OAUTHBEARER` authenticate and operate normally (folder listing, `STATUS`, search, `ENVELOPE`, special-use folders, quota).

- searches whose term the server cannot handle in the declared character set (`NO [BADCHARSET ...]`) are retried automatically in the charsets the server lists in its reply, or in `ISO-8859-1` and `US-ASCII`, whenever the term is representable in them; otherwise the error reports the server's reply and the reason.

### Tests

- new offline tests for the `BADCHARSET` request rewriting and the quoted-printable/base64 part decoders (`test-round5.R`); both features verified live against the Dovecot sandbox (the PNG and PDF attachments of the synthetic corpus are valid after extraction by part, and a search forged with an unknown charset is recovered in `ISO-8859-1`).

## mRpostman 1.5.3 (2026-08-27 feature update)

### New features

- **Non-ASCII folder names.** Mailbox names are now encoded to IMAP modified UTF-7 (RFC 3501, section 5.1.3) in every command that sends one (`select_folder()`, `create_folder()`, `rename_folder()`, `copy_msg()`, `append_msg()`, `status()`, ...) and decoded in every listing (`list_mail_folders()`, `list_folders_status()`, `list_special_use_folders()`), so folders such as `École` or `Entwürfe` can be referred to by their real names. Previously such names had to be given and were returned in their encoded form (`&AMk-cole`). The helpers `imap_utf7_encode()` and `imap_utf7_decode()` are exported.

- new `ImapCon$fetch_envelope()` method and exported `parse_envelope()` helper: the `ENVELOPE` of each message parsed into a data frame (date, subject, from, sender, reply_to, to, cc, bcc, in_reply_to, message_id), with addresses formatted as `Name <mailbox@host>` and RFC 2047 encoded words decoded.

- new `ImapCon$fetch_bodystructure()` method and exported `parse_bodystructure()` helper: the MIME structure of each message parsed into one row per part, with the section number usable in `fetch_body(mime_level = )`, type/subtype, charset, filename, encoding, size, disposition, and an `is_attachment` flag. Both parsers are built on a new IMAP list tokenizer that handles quoted strings, literals, and `NIL`.

- `configure_imap()` / `ImapCon$new()` gained `oauth_mechanism = c("XOAUTH2", "OAUTHBEARER")`, selecting the SASL mechanism used to send the OAuth 2.0 token. Gmail advertises both `AUTH=XOAUTH2` and `AUTH=OAUTHBEARER` (RFC 7628); the Microsoft 365 IMAP server advertises `AUTH=XOAUTH2` only. It maps to libcurl's `CURLOPT_LOGIN_OPTIONS = "AUTH=OAUTHBEARER"` (verified live against Gmail in 1.5.4).

- `list_mail_folders(detailed = TRUE)` adds a `my_rights` column when the server advertises `LIST-MYRIGHTS` (RFC 8440).

- `create_folder()` gained a `special_use` argument (`CREATE name (USE (\Archive))`, CREATE-SPECIAL-USE, RFC 6154), capability-checked.

### Bug fixes

- `list_mail_folders()` classified every folder as a child on servers whose hierarchy delimiter is `.` (Dovecot, and therefore most self-hosted servers): the delimiter was interpolated unescaped into a regular expression. It is now matched literally, and `root`/`children` are split correctly.

- `list_special_use_folders()` returned the delimiter (`"."`) instead of the folder name on servers that send unquoted mailbox names (Dovecot). The name is now read as the token after the delimiter, quoted or not.

### Tests

- the Docker sandbox now defines special-use mailboxes (`Drafts`, `Sent`, `Junk`, `Trash`, `Archive`), so `list_special_use_folders()` and the `special_use` column of `list_mail_folders(detailed = TRUE)` can be exercised locally. New offline tests cover modified UTF-7 round trips, the IMAP list tokenizer, `parse_envelope()`, `parse_bodystructure()`, the `LIST-MYRIGHTS` join, and the `CREATE ... (USE ...)` request (`test-round4.R`).

## mRpostman 1.5.2 (2026-08-27 feature update)

### New features

- new `ImapCon$fetch_preview()` method (`FETCH ... (PREVIEW)`, RFC 8970): the short text snippet the server generates for each message, without transferring its body. Returns a named character vector; accepts the `"$"` reference of a saved search.

- `ImapCon$fetch_metadata()` accepts the extension attributes `"PREVIEW"` (RFC 8970), `"SAVEDATE"` (RFC 8514), and `"MODSEQ"` (CONDSTORE, RFC 7162), each verified against the corresponding server capability. They are never requested by default.

- `ImapCon$status()` and `ImapCon$list_folders_status()` accept the extension items `"SIZE"` (STATUS=SIZE, RFC 8438; the folder size in bytes) and `"HIGHESTMODSEQ"` (CONDSTORE, RFC 7162), capability-checked.

- new search criterion constructors `saved_before()`, `saved_since()`, and `saved_on()` (SAVEDATE, RFC 8514), which compare the date a message was saved into the folder, and `modseq()` (CONDSTORE, RFC 7162), which matches the messages added or changed since a given modification sequence. Together with `status(items = "HIGHESTMODSEQ")`, `modseq()` supports "what changed since the last run" workflows without transferring anything else.

- `ImapCon$list_mail_folders()` gained a `detailed` argument: with `detailed = TRUE` it issues `LIST "" "*" RETURN (CHILDREN SUBSCRIBED SPECIAL-USE)` (LIST-EXTENDED, RFC 5258) and returns a `data.frame` with one row per folder and its attributes (`delimiter`, `selectable`, `has_children`, `subscribed`, `special_use`).

### Improvements

- commands that the server rejects with a tagged `NO` or `BAD` reply now fail immediately with the server's own reason (e.g. `The server rejected the command: NO [CANNOT] ...`) instead of the generic `Request error: the server returned an error.` after useless retries. The connection handle records the server's response lines through a libcurl debug callback; the `verbose` setting keeps its meaning and only controls whether that conversation is printed.

### Documentation

- the *basics* vignette gained a section on the extensions added since 1.5.0, and the *sandbox* vignette and `inst/docker/README.md` document the extensions the sandbox supports, including the newly enabled `acl` and `quota` plugins.

### Bug fixes

- plain `SEARCH` responses are now parsed by a dedicated helper (`parse_search_ids()`) that discards the `(MODSEQ n)` item appended by servers when a `MODSEQ` criterion is used; previously that number would have been returned as if it were a message id.

### Tests

- new offline tests for the SAVEDATE and CONDSTORE criteria, the `SEARCH` id extraction, the `PREVIEW` item parser, and the LIST-EXTENDED parser (`test-round3-parsers.R`); all features verified live against the Dovecot sandbox.

## mRpostman 1.5.1 (2026-08-27 feature update)

### New features

- **ACL** extension (RFC 4314), capability-checked: new `ImapCon$get_acl()` (`GETACL`, a `data.frame` of identifiers and rights), `ImapCon$set_acl()` (`SETACL`, replacing or, with a `+`/`-` prefix, adding/removing rights), `ImapCon$delete_acl()` (`DELETEACL`), `ImapCon$list_rights()` (`LISTRIGHTS`), and `ImapCon$my_rights()` (`MYRIGHTS`).

- new `ImapCon$set_quota()` method (`SETQUOTA`, RFC 2087) to set the `STORAGE` and/or `MESSAGE` limits of a quota root. Most servers restrict the command to administrators (Dovecot, for instance, refuses it unless a `quota_set` dictionary is configured).

- new `ImapCon$enable()` method (`ENABLE`, RFC 5161) to enable server extensions for the session, returning the ones the server confirmed.

- **SEARCHRES** extension (RFC 5182): `ImapCon$search()` gained a `save` argument. With `save = TRUE` the result is kept on the server (`SEARCH RETURN (SAVE)`) and the method returns the `"$"` reference, which `fetch_body()`, `fetch_header()`, `fetch_text()`, `fetch_metadata()`, `add_flags()`/`remove_flags()`/`replace_flags()`, `copy_msg()`, `move_msg()`, and `delete_msg()` now accept as `msg_id`, so that a search-then-act workflow no longer transfers the message ids. A fetch on `"$"` returns one element per matching message, named by sequence number or UID.

- **ESORT** extension (RFC 5267): `ImapCon$sort()` gained a `return` argument (`"COUNT"`, `"MIN"`, `"MAX"`, `"ALL"`); when given, `SORT RETURN (...)` is issued and only the requested items, computed by the server in sort order, are returned as a named list.

### Bug fixes

- `get_quota()` / `get_quota_root()` no longer return duplicated rows when libcurl delivers the untagged `QUOTA` line through both its header and body callbacks (as libcurl 8.x does).

### Tests

- the Docker sandbox (`inst/docker/dovecot.conf`) now enables Dovecot's `acl` and `quota` plugins (with `acl_anyone = allow` and a 1 GB storage quota), so that the `ACL` and `QUOTA` methods can be exercised locally. The new `ACL`, `ENABLE`, `SEARCHRES`, and `ESORT` features were verified against it.

- new offline tests for the ACL, ESORT, and ENABLE response parsers, the `SEARCH RETURN (SAVE)` request builder, the multi-message `FETCH` splitter, and the `QUOTA` de-duplication (`test-parse-acl-esort-enable.R`).

## mRpostman 1.5.0 (2026-08-27 feature update)

### New features

- new `ImapCon$check()` method issuing the IMAP `CHECK` command (RFC 3501, section 6.4.1), which requests a checkpoint of the selected folder. It has no client-observable effect (use `noop()` as a keep-alive) and is provided for completeness of the IMAP4rev1 client command set.

- **UIDPLUS** extension (RFC 4315), capability-checked:
  - `ImapCon$append_msg()` now returns (invisibly) the UID that the server assigned to the appended message, read from the `[APPENDUID <uidvalidity> <uid>]` response code, or `NA` when the server does not advertise `UIDPLUS`. Previously it returned `TRUE`.
  - `ImapCon$copy_msg()` and `ImapCon$move_msg()` now attach the `[COPYUID ...]` mapping to their (unchanged) return value as a `"copyuid"` attribute: a `data.frame` with `source_uid` and `dest_uid`, and the destination `uidvalidity` as an attribute. The message ids are still returned, so pipes are unaffected.
  - `ImapCon$expunge(msg_uid = ...)` (`UID EXPUNGE`) now verifies the `UIDPLUS` capability before issuing the command; a plain `expunge()` is unaffected.
  - `ingest_maildir()` (and therefore `enron_sandbox()`) records the assigned UID of every uploaded message in a new `uid` column of the returned manifest.

- **LIST-STATUS** extension (RFC 5819): new `ImapCon$list_folders_status()` method issuing `LIST "" "*" RETURN (STATUS (...))`, which returns the folder list together with the requested `STATUS` items (`MESSAGES`, `RECENT`, `UIDNEXT`, `UIDVALIDITY`, `UNSEEN`) in a single round trip, as a `data.frame` with one row per selectable folder. Requires the server `LIST-STATUS` capability.

- `ImapCon$append_msg()` gained a `flags` argument (`"Seen"`, `"Flagged"`, `"Answered"`, `"Draft"`, `"Deleted"`) sent through libcurl's `CURLOPT_UPLOAD_FLAGS`. By default the message is now stored **without** flags. Note that this depends on the libcurl version: libcurl >= 8.13 honors the option, while earlier versions ignore it and always store the message with `\Seen`, as they always did.

### Bug fixes

- `populate_sandbox()` now sets the planned `\Seen` state in both directions (adding it to the read messages and removing it from the unread ones) instead of only removing it. It relied on libcurl marking every appended message as read, which libcurl >= 8.13 no longer does, so the sandbox's read/unread schedule was no longer reproduced on current libcurl builds.

### Tests

- new offline tests for the UIDPLUS response-code parsers (`parse_appenduid()`, `parse_copyuid()`, sequence-set expansion) and for the `LIST-STATUS` response parser (`parse_list_status()`). All three features were also verified live against the Dovecot sandbox (`inst/docker/`).

## mRpostman 1.4.1 (2026-08-26 bugfix update)

### Bug fixes

- `fetch_body()`, `fetch_header()`, `fetch_text()`, and `fetch_attachments()` no longer fail on messages whose fetched part exceeds roughly 90 kB when the `curl` package is built against libcurl >= 8.x (e.g. `curl` 7.1.0, which bundles libcurl 8.14.1). libcurl aborts such a `FETCH` with `CURLE_TOO_LARGE` ("A value or data field grew larger than allowed") and drops the connection, which surfaced as `Fetch error: the server returned an error. Try to increase "timeout_ms"`. `execute_fetch_loop()` now detects this condition, re-selects the folder on the reconnected session, and re-issues the request in IMAP partial slices (`<start.count>`, 64 kB each) through the new internal `fetch_in_chunks()` helper, concatenating the cleaned slices; the result is byte-identical to a single fetch. Unaffected fetches follow the previous code path. Found on a 93 kB header of the Enron corpus (a `To:` line with several hundred recipients) while regenerating the R Journal replication run after a `curl` package upgrade.

### Tests

- new offline tests for the literal-size parser and the slice concatenation used by the chunked-fetch fallback (`test-fetch-in-chunks.R`).

## mRpostman 1.4.0-1 (2026-08-03 documentation update)

### Documentation

- the README now also displays the grand-total CRAN downloads badge, alongside the existing monthly downloads badge.

## mRpostman 1.4.0 (2026-07-12 feature update)

### Bug fixes

- `clean_msg_text()` no longer errors on messages carrying 8-bit bytes that are invalid in the session encoding (e.g. undeclared legacy charsets or binary fragments in old real-world corpora — `Error in gsub("=\r\n", "", msg): input string 1 is invalid`). Such strings are now made valid up front (latin1-to-UTF-8, which maps every byte and preserves ASCII) before any regex operation, and again after each base64 decoding, which can reintroduce raw bytes; declared-charset handling is unaffected. Additionally, the base64 decoding heuristic no longer aborts on payloads decoding to binary with embedded NULs (`rawToChar()` errors that a handler-less `tryCatch()` never actually caught): NUL bytes are dropped, and on any decoding error the text is kept unchanged instead of erroring. Found while decoding the full Enron corpus through the new sandbox ingestion.

### New features

- new `ingest_maildir()`: uploads any local maildir-style directory (one RFC 5322 message per file) to a folder on the connected IMAP server via `APPEND` — mail server backups, exported archives, or public corpora. Files are appended verbatim; failures are skipped with a warning, and an invisible manifest data.frame (`path`, `size`, `appended`) is returned.

- new `enron_sandbox()`: on-demand download of the public Enron e-mail corpus (CMU May 7, 2015 release; ~423 MB, one time, consented via `ask` and cached under `tools::R_user_dir("mRpostman", "cache")`), with subset selection by custodian, folder-name pattern, and `Date:` header window, ingested through `ingest_maildir()` — one server folder per custodian. Provides real data for demonstrations and teaching on top of the synthetic corpus of `sandbox_corpus()`. The download is never triggered by examples, tests, or vignettes, and fails gracefully offline with an informative message.

### Tests

- new offline tests for the internal Enron `Date:` header parser (both header variants — with and without weekday — and the no-`Date:`-header case).

## mRpostman 1.3.0 (2026-07-11 feature update)

### Bug fixes

- fetch results are now properly cleaned on Dovecot servers: `clean_fetch_results()` did not recognize Dovecot's tagged completion line, which carries timing information (e.g. `A240 OK Fetch completed (0.001 + 0.000 secs).`), so `fetch_text()`, `fetch_body()`, and `fetch_metadata()` results kept the trailing server response. Found while testing against the new Docker sandbox; affects any Dovecot-based provider (e.g. FastMail, GMX, many self-hosted servers).

### New features

- new reproducible IMAP sandbox: the package now ships a disposable local IMAP server (Dovecot on Alpine Linux, in `inst/docker/` — reachable via `system.file("docker", package = "mRpostman")`) and two new exported functions to exercise the package against it without a real mail account. `sandbox_corpus()` deterministically generates a corpus of synthetic RFC 822 messages (fixed RNG seed; `Date:` headers spread over 2020, large bodies, MIME encoded-word subjects, quoted-printable bodies, CSV, PNG, and one-page PDF attachments — all generated deterministically in base R — with repeated filenames, reply chains, and planned flags), and `populate_sandbox()` stores it in a mailbox using the package's own IMAP operations (`APPEND`, `CREATE`, `STORE`). A thin `Rscript` wrapper (`inst/docker/populate_mailbox.R`) does the same from the shell.

- new vignette *"A reproducible IMAP sandbox with Docker"* (`sandbox`) with setup instructions and a guided tour of searching, fetching, decoding, attachments, `SORT`/`THREAD`, and flag operations over the synthetic corpus.

### Tests

- the corpus generator is covered by a new offline `testthat` file (`test-sandbox-corpus.R`): determinism, RNG-state preservation, RFC 822 well-formedness, feature/metadata consistency, and round-trip decoding of the generated encoded-word subjects and quoted-printable bodies through the package's own decoders.

## mRpostman 1.2.2 (2026-07-07 bugfix and robustness update)

### Bug fixes

- `ImapCon$append_msg()` no longer hangs after the server's `+ go ahead` continuation when it is called after another command on the same connection. The shared connection handle still carried the previous operation's `CURLOPT_CUSTOMREQUEST`, which conflicted with the `CURLOPT_UPLOAD` that `APPEND` relies on; the custom request is now reset before the upload.

- `ImapCon$status()` no longer returns an empty result after a mid-session reconnection. When libcurl transparently reconnects (e.g. after a stale connection), the response buffer also carries the `CAPABILITY` line, whose `LIST-STATUS` token — together with the `... authenticated (Success)` line — made the parser lock onto the wrong parentheses. `parse_status_counts()` now anchors on the untagged `* STATUS` response.

### Improvements

- Extension-based methods now verify the server's advertised capability before issuing the command and raise an informative error (naming the command and its RFC, and pointing to `list_server_capabilities()`) instead of letting the server reply with a cryptic `BAD Unknown command`. This affects `sort()` (`SORT`), `thread()` (`THREAD=`), `get_quota()` / `get_quota_root()` (`QUOTA`), `namespace()` (`NAMESPACE`), `id()` (`ID`), `unselect_folder()` (`UNSELECT`), `list_special_use_folders()` (`SPECIAL-USE`), `move_msg()` (`MOVE`), and the ESEARCH path (`search(esearch = TRUE)`, `esearch_count()`, `esearch_min_id()`, `esearch_max_id()`). Capabilities are fetched once and cached per connection. Mandatory IMAP4rev1 (RFC 3501) commands are unaffected.

### Documentation

- reworded the package description and README overview to state that `mRpostman` is a session-based IMAP client that implements the full functionality of the IMAP4rev1 protocol (RFC 3501) from within R.

- the `README` now maps each IMAP command to its `mRpostman` method(s) — split into the mandatory RFC 3501 core and the optional, capability-checked extensions (with RFC references) — and its "available methods and functions" list was brought up to date with every method added since v1.1.6.

- removed the obsolete `code_migration` vignette (migrating pre-0.9.0.0 syntax).

- rewrote the OAuth2.0 vignette (`xoauth2.0`) with exact, image-free steps and R code to authenticate to Gmail via OAuth2.0 — covering both `httr`'s built-in flow and a manual loopback flow — plus a troubleshooting section. `httr` and `jsonlite` were added to `Suggests`.

- fixed the `basics` vignette figures, which were not rendering on the package website.

## mRpostman 1.2.1 (2026-07-06 feature update)

### New features

- new `ImapCon$close_folder()` (IMAP `CLOSE`) closes the selected folder and permanently removes its `\Deleted` messages; and `ImapCon$unselect_folder()` (IMAP `UNSELECT`, RFC 3691) closes it **without** expunging (requires the server `UNSELECT` capability). Both leave the connection with no folder selected.

- new `ImapCon$id()` method (IMAP `ID`, RFC 2971) to exchange client/server identification. It optionally sends the client id fields (a named character vector) and returns the server's id as a named character vector; parsed by an offline-tested `parse_id()` helper.

## mRpostman 1.2.0 (2026-07-06 feature update)

### New features

- new `ImapCon$get_quota_root()` and `ImapCon$get_quota()` methods for the IMAP quota extension (`GETQUOTAROOT` / `GETQUOTA`, RFC 2087). They return a `data.frame` with `quota_root`, `resource`, `usage`, and `limit` (one row per resource; `STORAGE` is reported by the server in kibibytes). Requires the server `QUOTA` capability. The responses are parsed by an offline-tested `parse_quota()` helper.

- new `ImapCon$append_msg()` method to append a full RFC 822 message to a mail folder (IMAP `APPEND`) — e.g. saving a message to `Drafts` or `Sent`. Unlike every other operation it is performed by an upload (`CURLOPT_UPLOAD`, via a read callback like `curl::send_mail()`) rather than a `customrequest`; it reuses the connection handle and restores it out of upload mode afterwards. The message is stored with the server's default flags.

- both new areas are documented in the `basics` vignette.

## mRpostman 1.1.9 (2026-07-06 feature update)

### New features

- new `ImapCon$namespace()` method issuing the IMAP `NAMESPACE` command (RFC 2342). It returns a named list (`personal`, `other_users`, `shared`), each a `data.frame` of namespace `prefix`/`delimiter` pairs (or `NULL` when the server returns `NIL`). Requires the server `NAMESPACE` capability.

- new `ImapCon$list_special_use_folders()` method issuing `LIST (SPECIAL-USE)` (RFC 6154). It returns a `data.frame` mapping each special-use folder to its attribute (`\Sent`, `\Drafts`, `\Junk`, `\Trash`, `\Archive`, `\All`, `\Flagged`). Requires the server `SPECIAL-USE` capability.

- both are parsed by dedicated internal helpers (`parse_namespace()`, `parse_special_use()`) covered by offline tests, and documented in the `basics` vignette.

## mRpostman 1.1.8 (2026-07-06 feature update)

### New features

- new `ImapCon$sort()` method for server-side sorting (IMAP `SORT`, RFC 5256). It returns the message ids ordered by the server according to the sort keys (`by`, any subset of `ARRIVAL`, `CC`, `DATE`, `FROM`, `SIZE`, `SUBJECT`, `TO`), with optional `reverse` (descending), a `criteria` restriction (default `ALL`), `use_uid`, and a configurable `char_set`. The server-provided order is preserved (the result is deliberately **not** passed through the ascending-order `fix_search_stripping()` used by `search()`). Requires the server `SORT` capability.

- new `ImapCon$thread()` method for server-side threading (IMAP `THREAD`, RFC 5256), with `algorithm` (`REFERENCES` or `ORDEREDSUBJECT`), a `criteria` restriction, `use_uid`, and a configurable `char_set`. It returns a list of integer vectors, one per top-level thread (nested parent/child ids are flattened into their thread). Requires a server `THREAD=` capability.

- both new methods are parsed by dedicated internal helpers (`parse_sort()`, `parse_thread()`) and share a new `execute_ordered_search()` engine that mirrors the search request/retry logic but keeps the server ordering intact. The parsers are covered by offline tests.

## mRpostman 1.1.7 (2026-07-06 feature update)

### New features

- new `ImapCon$list_subscribed_folders()` method to list the subscribed mail folders (IMAP `LSUB`), complementing `list_mail_folders()` (which issues `LIST` and returns every folder).

- new `ImapCon$subscribe_folder()` and `ImapCon$unsubscribe_folder()` methods to subscribe to / unsubscribe from a mail folder (IMAP `SUBSCRIBE` / `UNSUBSCRIBE`), i.e. to add or remove it from the set returned by `list_subscribed_folders()`.

- new `ImapCon$noop()` method issuing the IMAP `NOOP` command. It does nothing on the server other than resetting the inactivity autologout timer, which makes it useful as a keep-alive during long idle periods.

### Internal

- the `LIST` response parsing was extracted from `list_mail_folders_int()` into a shared internal `parse_folder_list()` helper (parameterized by the command keyword), now used by both `list_mail_folders()` and the new `list_subscribed_folders()`. This removes the duplicated parsing that previously lived in the retry branch and adds offline test coverage (`LIST`, `LSUB`, and the Yandex `|` hierarchy separator).

## mRpostman 1.1.6 (2026-07-06 feature update)

### New features

- new `ImapCon$delete_folder()` method to delete a mail folder (IMAP `DELETE`), completing the folder-management set alongside `create_folder()` and `rename_folder()`. The target folder must be named explicitly (there is no implicit "delete the selected folder"), and a confirmation message is printed unless `mute = TRUE`.

- new `ImapCon$status()` method to query a mail folder's counters via the IMAP `STATUS` command **without selecting the folder** (unlike `examine_folder()`, which issues `EXAMINE`). It returns a named vector with the requested data items — any subset of `MESSAGES`, `RECENT`, `UIDNEXT`, `UIDVALIDITY`, and `UNSEEN` (all by default), configurable via the new `items` argument. The response is parsed by a dedicated internal `parse_status_counts()` helper, so the result stays correct regardless of the order in which the server returns the items.

## mRpostman 1.1.5 (2026-07-06 bug-fix and feature update)

### New features

- `search_string()` (and the custom `search()`) now support **non-ASCII search terms**. When the term contains non-ASCII characters, the command declares `CHARSET UTF-8` and sends the term as UTF-8 bytes, so servers such as Gmail can match accented/non-Latin text (GitHub issue #12). Pure-ASCII searches are unchanged.

- `fetch_attachments()` and `get_attachments()` gained a `local_dir` argument to set the base directory where attachments are saved (default `"."`, the working directory). The `<username>/<mail folder>/<msg id>` subfolder tree is created inside it (GitHub issue #15).

- new `ImapCon$disconnect()` method to release the connection handle when a session is finished (GitHub issue #13).

### Bug fixes

- `decode_mime_header()` now honors the character set declared in the RFC 2047 encoded-word (`=?<charset>?<enc>?...?=`) instead of guessing it heuristically. The declared charset is passed to `iconv()` (via a new internal `apply_charset()` helper) for both the quoted-printable (`Q`) and base64 (`B`) encodings, so headers in any `iconv`-supported charset now decode correctly — including ones the previous heuristic could not handle or mislabeled (e.g. Windows-1251 and KOI8-R Cyrillic, ISO-8859-2/Windows-1250 Central European, Big5, Shift_JIS, EUC-KR). The legacy heuristic is kept only as a fallback for "loose" quoted-printable strings that carry no declared charset.

- the message-body decoders (`decode_mime_text()`, used by `clean_msg_text()`) now honor the charset declared in the MIME `Content-Type` for quoted-printable and base64 parts, via the same `apply_charset()` helper as the header decoder. The previous heuristic is kept as a fallback when no charset is declared, and the charset is not re-applied to parts already normalized to UTF-8 by the HTML parser (avoiding a double conversion).

- the `esearch = TRUE` search path no longer evaluates server-provided text as R code. The `ALL` sequence-set of the `ESEARCH` response is now expanded by a dedicated parser (`parse_esearch_all()`) instead of `eval(parse(...))`, removing a code-injection/robustness risk while producing identical results for valid responses.

- `examine_folder()` no longer assumes the server returns both `EXISTS` and `RECENT`, in that order. The counts are now parsed and labeled by their actual keyword (new internal `parse_examine_counts()`), so the result stays correct when the order differs and no longer errors when `RECENT` is absent.

### Documentation

- fixed several English spelling and grammar errors throughout the `roxygen2` documentation of the `ImapCon` R6 methods and the helper functions (e.g. "trough" → "through", "successfuly" → "successfully", "queries follows" → "queries follow", "multiples arguments" → "multiple arguments", "depend on ESEARCH" → "depends on ESEARCH").

- corrected copy-paste mistakes in the method documentation: `search_string()` now belongs to the `search by string` family (was `search by date`), `search_sent_on()` now belongs to `search by date` (was `search by size`), and the `size` parameter of the size-search helpers is described in bytes instead of seconds.

- fixed broken examples in the `ImapCon` documentation: `select_folder()` (was `select_mail_folder()`), a missing closing parenthesis in the `replace_flags()` example, a missing space after `#'` in the `fetch_text()` example, and `<-` in the `fetch_attachments_list()` example (was `<`).

- removed a duplicated `\link{younger_than}` cross-reference in the custom search documentation (now correctly links to `older_than`) and cleaned up duplicated lines in the `search_since()` `date_char` description.

- fixed typos in user-facing messages, including the flag confirmation messages ("successfuly" → "successfully"), the `esearch_*_id()` type-check message ("must of type" → "must be of type"), the buffer size and length checks ("equal or greater than" → "equal to or greater than"), and the dead-connection error (`ImapConf$new()` → `ImapCon$new()`).

- fixed English spelling and grammar errors in the `README` and in the `basics`, `code_migration`, and `xoauth2.0` vignettes, including invalid Portuguese month abbreviations in date examples ("30-Ago-2019" → "30-Aug-2019") and misspelled function/class references (e.g. `confiure_imap()`, `list_atachments()`, `fetch_attachents()`, `ImapConf`).

## mRpostman 1.1.4 (2024-09-15 CRAN update)

### Methods & Functions

- `decode_quoted_printable_header()`: changed the conversion engine to `iconv` and added `useBytes = TRUE` to the `gsub()` call; and added support to `ISO-8859-10` and  `Windows-1252` (Nordic/latin6), and `ISO-8859-9` and `Windows-1254` (Turkish) encodings.
  
- `decode_quoted_printable_text()`: changed the conversion engine to `iconv` and added `useBytes = TRUE` to the `gsub()` call; and added support to `ISO-8859-10` and  `Windows-1252` (Nordic/latin6), and `ISO-8859-9` and `Windows-1254` (Turkish) encodings.

- `zzz()` - removed message from the transition to `R6` structure and added the citation request instead.

### Documentation

- changed `README.Rmd` and `basics.Rmd` to better reflect the current status of login procedures across most of the IMAP providers and the citation to `mRpostman` paper in JORS.

- added a `CITATION` file to `inst` folder.

## mRpostman 1.1.3 (2024-08-10 Github patch)

- broken Github fix to `decode_quoted_printable_header()` and `decode_quoted_printable_text()` (needed to rewind to v1.1.2 before moving again to v1.1.4)


## mRpostman 1.1.2 (2023-12-17 CRAN update)

### Methods & Functions

- `clean_fetch_results()`: added argument `useBytes = TRUE` to all `gsub()` calls. According to Kurt Hornik, "fetching fails using current versions of R for some contents with non-ASCII characters".

- Added bypass argument `as_is` to methods/functions `get_attachments()`, `execute_fetch_attachments()`, `fetch_attachments()`; and `as_is` functionality to `get_attachments()` and `execute_fetch_attachments` as proposed by Paul Smith to handle non-base64 files.

---

## mRpostman 1.1.1 (2023-07-27 Github patch)

### Documentation

- changed unit test to Github Actions CI/CD

---

## mRpostman 1.1.0 (2022-10-25 CRAN fix bump)

### Documentation

- removed 'LazyData' line to fix CRAN check issue
- added "\\dontrun{}" to `decode_mime_headers()` in order to fix CRAN check issues
- fixed bad URLs everywhere (tools.ietf.org/html/... ===> www.rfc-editor.org/rfc/...)

### Methods & Functions

- updated completion msg in all fetch methods to: `"\n::mRpostman: fetch operation is complete.\n"`

---

## mRpostman 1.0.2 (2021-04-14 Github patch)

### Documentation

- typos in `list_attachments()`
- typos in `README.Rmd`

### Methods & Functions

- updated completion msg in all fetch methods to: `"\n::mRpostman: fetch operation is complete.\n"`

---

## mRpostman 1.0.1 (2021-01-13 Github patch)

### Documentation

- typos in documentation ("Basics" vignette)
- typos in the methods and functions documentation
- updated figures in "Basics" vignette

---

## mRpostman 1.0.0 (2020-11-29 CRAN update)

### Changes

- incorporates all the changes/updates from GitHub patches `0.9.1-1` to `0.9.1-6`.

---

## mRpostman 0.9.1-6 (2020-11-28 Github patch)

### Changes

- modifies the behaviour for assigning names to the local folders. Instead of using the url, now the package uses the "/username/mailbox" as the 'root' directory for saving messages and attachments. For example: `./allanvcq@gmail.com/INBOX/UID1430/` instead of "imap.gmail.com/INBOX/UID1430". This will allow users to better differentiate between local folders, when using different mailboxes in the same server.

---

## mRpostman 0.9.1-5 (2020-11-18 Github patch)

### Bug fixes

- fixed the `pattern8` regex in `clean_fetch_results()`.

---

## mRpostman 0.9.1-4 (2020-11-12 Github patch)

### New features

- changes the default value of the `timeout_ms` parameter to `0`, which is the default value in libcurl, meaning that no timeout is specified.

---

## mRpostman 0.9.1-3 (2020-11-08 Github patch)

### New features

- added the `clean_msg_text()` function. It will be particularly useful for cleaning and preparing message text retrieved with `fetch_body(..., mime_level = 1)`. These texts can be used for sentiment analysis and other text mining tasks;

- added internal functions `decode_mime_text()` and `decode_quoted_printable_text()`. The second is called by the first one;  

- renamed the original `decode_quoted_printable` called from `decode_mime_header()` to `decode_quoted_printable_text()`;

- included the `xml2` and the `rvest` packages as dependencies because of the addition of the `clean_msg_text()`.

---

## mRpostman 0.9.1-2 (2020-11-01 Github patch)

### New features

- added the `mime_level` argument to the `fetch_body()` method. Now, the user can choose whether the fetch will retrieve the full body content or an specific one. This is particularly helpful for retrieving clean text parts without inline and regular attachments for instance.

### Bug fixes

- adapts to the new return pattern of MS Exchange IMAP servers without losing the compatibility with others;

- fixed the misbehavior for the attachment fetching of message in message attachments. Now, either `fetch_attachments()` and `fetch_text/body()` + `get_attachments()` combo can handle this specific case;

- changed the attachment identification to the "name" field instead of the "filename". In very rare cases, the "filename" field identification fails because of encoding errors on the sender's side;

- incorrect decoding of ordinary words (such as "ResearchGate", for instance) in `decode_mime_header()`.

### Changes without backward compatibility

* The default argument in all `reset_*()` methods now are `x` in order to prevent wordy method calls and repetition. The older ones were deprecated in this version.

---

## mRpostman 0.9.1-1 (2020-10-26 Github patch)

### Main features
* added RFC 2047 quoted-printable and base64 MIME header decoder

### Functions & methods
* added the `decode_mime_header()` function
    - it is used inside `get_attachments()` and `fetch_attachments()` for correctly setting the filenames
    - it is also exported for the user in order to be used for a header decoding operation after fetching metadata, for example.
    - it was necessary to add the {stringi} package as dependency

### Bug fixes

* `list_mail_folders()`: fixed the regex related to the hierarchy separator to accept the "|" separator. It was causing a misbehavior in Yandex accounts.

* `clean_fetch_results()` (internal): fixed the regex responsible for cleaning the attachment content. It was causing a misbehavior in Yandex accounts. All calls to gsub() in this function now have `ignore.case = TRUE`. 

* `reset_*()` methods: Except by `reset_password()` and `reset_xoauth2_bearer()`, the other methods were not reflecting the changes in the `ImapCon$con_params` object, although they were succesfully modifying the `ImapCon$con_handle`, which was sufficient to work properly. It could cause the user to thing that the changes wouldn't taking any effect.

### Minor changes

* The confirmation message in `fetch_attahments()` changed to "\n::mRpostman: the fetch operation is complete.\n";
* Fixed some minor typographic errors in the vignettes and the README file;
* Adjusted the order of messages printed by `rename_folder()`.

### Changes without backward compatibility

* The default argument in all `reset_*()` methods now are `x` in order to prevent wordy method calls and repetition. The older ones will be deprecated in version 0.9.1-2;
* The `reset_ssl()` method had the name changed to `reset_use_ssl()` to better reflect the connection parameter to be reset.
* the `metadata` argument in `fetch_metadata` now is `attribute`.

---

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

---

## mRpostman 0.2.0 (2019-08-18 - CRAN submission)

- changed function name patterns to those specified in the tidyverse style guide

* changed return from mailboxes operation functions and some miscellanea functions: 
    - now `select_mailbox()`, `rename_mailbox()`, `copy_msg()`, `move_msg()`, `delete_msg()`, 
`expunge()`, `add/remove/replace_flags()` outputs are invisible and only return 
`imapconf` or a list (imapconf+msg_ids).

- changed package logo

---

## mRpostman 0.1.0 (2019-08-13 - Github release)
