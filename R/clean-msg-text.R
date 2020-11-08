#' Extract text from MIME level
#' @param msg_list A \code{list} with the MIME level 1 of the body or text content
#'   of the messages fetched with \href{#method-fetch_body}{\code{ImapCon$fetch_body()}} or
#'   \href{#method-fetch_text}{\code{ImapCon$fetch_text()}}.
#' @return A \code{list} containing the decoded messages if applicable.
#' @references Moore, K. (1996), MIME (Multipurpose Internet Mail Extensions) Part
#'   Three: Message Header Extensions for Non-ASCII
#'   Text, RFC 2047, November 1996, https://tools.ietf.org/html/rfc2047.
#' @references Freed, N., Borenstein, N. (1996), Multipurpose Internet Mail Extensions
#'   (MIME) Part One: Format of Internet Message Bodies, RFC 2045, November 1996,
#'   https://tools.ietf.org/html/rfc2045.
#' @references Internal parts of this object, regarding the quoted printable type,
#'   were borrowed from https://github.com/hrbrmstr/hrbrmisc/blob/master/R/qp.r with
#'   slight modifications.
#' @export
#' @examples
#' \dontrun{
#' ids <- con$search_since(date_char = "01-Apr-2020", use_uid = TRUE)
#'
#' fetch_res <- ids %>%
#'   con$fetch_body(use_uid = TRUE, mime_level = 1L)
#'
#' clean_text_list <- clean_msg_text(msg_list = fetch_res)
#' }
clean_msg_text <- function(msg_list) {

  msg_list_out <- list()

  for (i in seq_along(msg_list)) {
    # print(i)

    msg <- msg_list[[i]]

    msg <- gsub('\r\n UID \\d+$|UID \\d+$', '', msg)

    # msg <- gsub('\r\n|\t', '', msg)
    pattern_content_type = 'Content-Type: text/html[;\t|\r|\n|\r\n|a-zA-Z0-9 ]+(.*?)[^!]--[^>]|Content-Type: text/plain[;\t|\r|\n|\r\n|a-zA-Z0-9 ]+(.*?)[^!]--[^>]'
    # [^!] & [^>] are important so it does not stop in html comments, such as <!-- or -->
    if (grepl(pattern_content_type, msg)) {
      full_text <- unlist(regmatches(msg, gregexpr(pattern_content_type, msg, ignore.case = TRUE)))[1] #text/html or text/plain whatever

      if (grepl('base64', full_text)) {
        msg_text <- sub('.*Content-', '', full_text, ignore.case = TRUE) # sub extracts only the first match # nao funciona com i=4

        pattern2 = "\\r\\n[^ ]+\\r\\n" # lines that do not contain space # only works for base 64

        msg_text <- unlist(regmatches(msg_text,
                                      regexec(pattern2, msg_text,
                                              perl = TRUE)))

        msg <- rawToChar(base64enc::base64decode(msg_text))

      } else {
        pattern2 = "\r\n\r\n<(.*?)[^!]--[^>]" # lines that do not contain space # only works for base 64

        msg <- unlist(regmatches(full_text,
                                 gregexpr(pattern2, full_text)))

      }

      rm(full_text)

    }

    if (identical(msg, character(0))) {
      msg = ""
    }


    # msg = 'ResearchGate'
    if (!identical(msg, character(0)) && grepl(' ', msg)) {

      if (grepl('<(br|basefont|hr|input|source|frame|param|area|meta|!--|col|link|option|base|img|wbr|!DOCTYPE).*?>|<(a|abbr|acronym|address|applet|article|aside|audio|b|bdi|bdo|big|blockquote|body|button|canvas|caption|center|cite|code|colgroup|command|datalist|dd|del|details|dfn|dialog|dir|div|dl|dt|em|embed|fieldset|figcaption|figure|font|footer|form|frameset|head|header|hgroup|h1|h2|h3|h4|h5|h6|html|i|iframe|ins|kbd|keygen|label|legend|li|map|mark|menu|meter|nav|noframes|noscript|object|ol|optgroup|output|p|pre|progress|q|rp|rt|ruby|s|samp|script|section|select|small|span|strike|strong|style|sub|summary|sup|table|tbody|td|textarea|tfoot|th|thead|time|title|tr|track|tt|u|ul|var|video).*?<\\/\\2>', msg, perl = TRUE)) { # if it is a html message

        pattern = "<html (.*?)</html>"
        html_msg <- unlist(regmatches(msg, gregexpr(pattern, msg, ignore.case = TRUE)))

        if (identical(html_msg, character(0))) {
          page <- xml2::read_html(msg)
        } else {
          page <- xml2::read_html(html_msg[1])
        }

        msg <- page %>%
          rvest::html_nodes('body') %>%
          rvest::html_text()
      }

    } else {

      if (grepl('[^-A-Za-z0-9+/=]|=[^=]|={3,}$', msg)) { # to see if it as base64 encoded string
        tryCatch({
          msg <- rawToChar(base64enc::base64decode(msg))
        })

        if (grepl('<(br|basefont|hr|input|source|frame|param|area|meta|!--|col|link|option|base|img|wbr|!DOCTYPE).*?>|<(a|abbr|acronym|address|applet|article|aside|audio|b|bdi|bdo|big|blockquote|body|button|canvas|caption|center|cite|code|colgroup|command|datalist|dd|del|details|dfn|dialog|dir|div|dl|dt|em|embed|fieldset|figcaption|figure|font|footer|form|frameset|head|header|hgroup|h1|h2|h3|h4|h5|h6|html|i|iframe|ins|kbd|keygen|label|legend|li|map|mark|menu|meter|nav|noframes|noscript|object|ol|optgroup|output|p|pre|progress|q|rp|rt|ruby|s|samp|script|section|select|small|span|strike|strong|style|sub|summary|sup|table|tbody|td|textarea|tfoot|th|thead|time|title|tr|track|tt|u|ul|var|video).*?<\\/\\2>', msg, perl = TRUE)) { # if it is a html message

          pattern = "<html (.*?)</html>"
          html_msg <- unlist(regmatches(msg, gregexpr(pattern, msg, ignore.case = TRUE)))

          if (identical(html_msg, character(0))) {
            page <- xml2::read_html(msg)
          } else {
            page <- xml2::read_html(html_msg[1])
          }

          msg <- page %>%
            rvest::html_nodes('body') %>%
            rvest::html_text()
        }

      }

    }

    msg <- gsub('=\r\n', '', msg) # important!!

    msg <- decode_mime_text(as.character(msg))

    msg_list_out <- c(msg_list_out, msg)

    # cat(clean_msg, '\r\n\r\n-----------------\r\n\n\n')
  }

  names(msg_list_out) = names(msg_list)

  return(msg_list_out)
}
