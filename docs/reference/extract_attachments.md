# Extract attachments from already-fetched messages

The offline half of the attachment family (since the 2026 refactoring):
takes the full text of messages already fetched with
[`ImapCon$fetch_body()`](#method-fetch_body), walks the MIME multipart
tree by its declared boundaries, and either reports the attachments
(`dest = NULL`) or writes them to disk. It replaces both
`list_attachments()` (reporting) and `ImapCon$get_attachments()`
(writing).

## Usage

``` r
extract_attachments(
  msg_list,
  dest = NULL,
  content_disposition = "both",
  override = FALSE,
  as_is = FALSE,
  mute = FALSE
)
```

## Arguments

  - msg\_list:
    
    A `list` with the fetched messages, as returned by
    [`ImapCon$fetch_body()`](#method-fetch_body) (the full message,
    whose headers declare the MIME boundaries). Text-only fetches
    (`fetch_text()`) lack those headers and cannot be walked reliably.

  - dest:
    
    `NULL` (default) to only report: the return value is a named `list`
    with one `data.frame` per message (filename, content\_disposition,
    type, size; zero rows when a message has no attachments). A
    directory path writes each message's attachments to `dest/<message
    id>/` and returns the same manifest invisibly.

  - content\_disposition:
    
    One of `"both"` (default), `"attachment"`, or `"inline"`.

  - override:
    
    A `logical`. If `TRUE`, overrides existing files with the same name.
    Default is `FALSE`.

  - as\_is:
    
    If `TRUE`, writes payloads without decoding the transfer encoding.
    Default is `FALSE`.

  - mute:
    
    A `logical`. If `TRUE`, mutes the confirmation message when writing.
    Default is `FALSE`.

## Value

A named `list` of `data.frame`s (see `dest`).

## See also

Other attachments: `ImapCon`, `list_attachments()`

## Examples

``` r
if (FALSE) { # \dontrun{
out <- con$fetch_body(con$query(size > 1e6, use_uid = TRUE), use_uid = TRUE)
extract_attachments(out)                       # report only
extract_attachments(out, dest = "~/attachments") # write files
} # }
```
