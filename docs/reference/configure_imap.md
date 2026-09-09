# IMAP Connection Configuration

Configure and create a new IMAP connection.

## Usage

``` r
configure_imap(
  url,
  username,
  password = NULL,
  xoauth2_bearer = NULL,
  oauth_mechanism = c("XOAUTH2", "OAUTHBEARER"),
  use_ssl = TRUE,
  verbose = FALSE,
  buffersize = 16000,
  timeout_ms = 0,
  use_uid = TRUE,
  mute = FALSE,
  retries = 1,
  ...
)
```

## Arguments

  - url:
    
    A character string containing the IMAP server address

  - username:
    
    A character string containing the username.

  - password:
    
    A character string containing the user's password.

  - xoauth2\_bearer:
    
    A character string containing the oauth2 bearer token.

  - oauth\_mechanism:
    
    The SASL mechanism used to send the OAuth 2.0 token: `"XOAUTH2"`
    (default; advertised by Gmail, Yahoo, and Microsoft 365) or
    `"OAUTHBEARER"` (RFC 7628; advertised by Gmail, but not by the
    Microsoft 365 IMAP server). Check `list_server_capabilities()` for
    the `AUTH=` tokens of your server. Ignored when authenticating with
    a password.

  - use\_ssl:
    
    A logical. `TRUE` (default) requires TLS: an `imaps://` URL is
    encrypted from the start, and an `imap://` URL is upgraded with
    `STARTTLS` before any credential is sent; if the server does not
    offer `STARTTLS`, the connection fails (there is no silent fallback
    to plaintext). `FALSE` sends everything unencrypted, for plain test
    servers such as the Docker sandbox.

  - verbose:
    
    If `FALSE`, mutes the flow of information between the server and the
    client. Default is `FALSE`.

  - buffersize:
    
    The size in bytes for the receive buffer. Default is 16000 bytes or
    16kb, which means it will use the libcurl's default value. According
    to the libcurl's documentation, the maximum buffersize is 512kb (or
    512000 bytes), but any number passed to `buffersize` is treated as a
    request, not an order.

  - timeout\_ms:
    
    Time in milliseconds (ms) to wait for the execution or re-execution
    of a command. Default is 0, which means that no timeout limit is
    set.

  - use\_uid:
    
    Connection-level default for the `use_uid` argument of the methods:
    if `TRUE`, operations use the `"UID"` (unique identifier), stable
    during the life cycle of a message, instead of message sequence
    numbers. Each call can still override it. Since 3.0.0 the default is
    `TRUE`: UIDs are stable, sequence numbers renumber whenever messages
    are expunged.

  - mute:
    
    Connection-level default for the `mute` argument of the methods: if
    `TRUE`, confirmation messages are suppressed. Each call can still
    override it. Default is `FALSE`.

  - retries:
    
    Connection-level default for the `retries` argument of the methods
    (number of attempts to connect and execute a command). Each call can
    still override it. Default is `1`.

  - ...:
    
    Further curl parameters (see `curl::curl_options`) that can be used
    with the IMAP protocol. Only for advanced users.

## Value

A new \`ImapCon\` object.

## Examples

``` r
if (FALSE) { # \dontrun{
# w/ Plain authentication
con <- configure_imap(
  url="imaps://outlook.office365.com",
  username="user@agency.gov.br",
  password=rstudioapi::askForPassword(),
  verbose = TRUE)

# w/ OAuth2.0 authentication
con <- configure_imap(
  url="imaps://outlook.office365.com",
  username="user@agency.gov.br",
  verbose = TRUE,
  xoauth2_bearer = "XX.Ya9...")
} # }
```
