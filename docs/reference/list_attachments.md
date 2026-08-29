# List attachments and content-disposition types

List attachments and content-disposition types

## Usage

``` r
list_attachments(msg_list)
```

## Arguments

  - msg\_list:
    
    A `list` containing the messages (body or text) fetched from the
    server.

## Value

A `list` of `data.frames` containing the filenames and their
`Content-Disposition` types for each fetched message.

## Note

Please, note that this is an independent function and not an R6 method
that depends on the connection object. Therefore, it should be called
alone without the ImapCon object.

## See also

Other attachments: `ImapCon`, `extract_attachments()`

## Examples

``` r
if (FALSE) { # \dontrun{
con$select_folder(folder = "INBOX")
# do a search followed by a fetch operation, then extract the attachments' list
out <- con$search_string(expr = "@k-state.edu", where = "FROM") %>%
  con$fetch_body()
att_list <- list_attachments(msg_list = out)

# or
att_list <- con$search_string(expr = "@k-state.edu", where = "FROM") %>%
  con$fetch_body() %>%
  list_attachments()
} # }
```
