# Package index

## Connection

Configure and hold an IMAP session.

<!-- end list -->

  - `configure_imap()` : IMAP Connection Configuration
  - `ImapCon` : An IMAP Connection Class

## Search criteria

Criterion constructors and combinators for custom search statements; the
same criteria the query language translates to.

<!-- end list -->

  - `AND()` : Relational-operator-function to construct a custom search
    statement
  - `OR()` : Relational-operator-function to construct a custom search
    statement
  - `Ops(<imap_search>)` : Combine search criteria with R's own
    operators
  - `string()` : Criterion constructor function to be combined in a
    custom search statement
  - `flag()` : Criterion constructor function to be combined in a custom
    search statement
  - `before()` : Criterion constructor function to be combined in a
    custom search statement
  - `since()` : Criterion constructor function to be combined in a
    custom search statement
  - `on()` : Criterion constructor function to be combined in a custom
    search statement
  - `sent_before()` : Criterion constructor function to be combined in a
    custom search statement
  - `sent_since()` : Criterion constructor function to be combined in a
    custom search statement
  - `sent_on()` : Criterion constructor function to be combined in a
    custom search statement
  - `saved_before()` `saved_since()` `saved_on()` : Criterion
    constructor functions for the SAVEDATE extension (RFC 8514)
  - `larger_than()` : Criterion constructor function to be combined in a
    custom search statement
  - `smaller_than()` : Criterion constructor function to be combined in
    a custom search statement
  - `older_than()` : Criterion constructor function to be combined in a
    custom search statement
  - `younger_than()` : Criterion constructor function to be combined in
    a custom search statement
  - `modseq()` : Criterion constructor function for the CONDSTORE
    extension (RFC 7162)
  - `fuzzy()` : Criterion modifier for fuzzy (approximate) matching in a
    custom search
  - `filter_stored()` : Criterion constructor referencing a filter
    stored on the server
  - `verbatim()` : Criterion constructor function for a verbatim IMAP
    search fragment

## Decoding and parsing

Turn fetched protocol payloads into analysis-ready data.

<!-- end list -->

  - `clean_msg_text()` : Extract text from MIME level
  - `decode_mime_header()` : Decode RFC 2047 quoted-printable and base64
    MIME headers and strings
  - `decode_mime_text()` : Decode RFC 2047 quoted-printable and base64
    MIME encoded text
  - `extract_attachments()` : Extract attachments from already-fetched
    messages
  - `list_attachments()` : List attachments and content-disposition
    types
  - `parse_envelope()` : Parse an IMAP ENVELOPE into a one-row data
    frame
  - `parse_bodystructure()` : Parse an IMAP BODYSTRUCTURE into a data
    frame of MIME parts
  - `imap_utf7_encode()` `imap_utf7_decode()` : Encode and decode
    mailbox names in IMAP modified UTF-7
  - `imap_url()` : An IMAP URL naming a message, or a part of it, for
    CATENATE (RFC 5092)
  - `metadata_options()` : Message Metadata Options

## Sandbox and corpus tools

A disposable local server and reproducible corpora for tests and demos.

<!-- end list -->

  - `populate_sandbox()` : Populate a Mailbox with the Synthetic Sandbox
    Corpus
  - `sandbox_corpus()` : Generate a Deterministic Synthetic Mail Corpus
  - `enron_sandbox()` : Populate a Mailbox with a Subset of the Enron
    Corpus
  - `ingest_maildir()` : Ingest a Local Maildir into an IMAP Folder

## Misc

<!-- end list -->

  - `mRpostman-package` `mRpostman` : An IMAP client for R
  - `%>%` : Common Pipe operator
