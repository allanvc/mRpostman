# The expression-based search language behind ImapCon$query() and
# imap_query(): ordinary R expressions, captured unevaluated and translated
# into RFC 3501 search strings by a pure function. Field names (subject,
# from, to, cc, bcc, body, text, flag, size, sent, date, saved, age, and
# header("Name")) are special only on the left side of a comparison;
# everything else is evaluated in the caller's environment.

.query_string_fields <- c(subject = "SUBJECT", from = "FROM", to = "TO",
                          cc = "CC", bcc = "BCC", body = "BODY", text = "TEXT")
.query_date_prefix   <- c(sent = "SENT", date = "", saved = "SAVED")
.query_std_flags     <- c(SEEN = "UNSEEN", ANSWERED = "UNANSWERED",
                          FLAGGED = "UNFLAGGED", DELETED = "UNDELETED",
                          DRAFT = "UNDRAFT", RECENT = "OLD")
.query_fields <- c(names(.query_string_fields), "flag", "keyword", "size",
                   names(.query_date_prefix), "age", "modseq")

#' Translate a query expression into an IMAP search string
#'
#' The pure translator behind \code{ImapCon$query()}: it takes an ordinary
#' R expression and returns the RFC 3501 search string it stands for,
#' without touching any server. Fields are \code{subject}, \code{from},
#' \code{to}, \code{cc}, \code{bcc}, \code{body}, \code{text} (compared
#' with \code{==} and \code{!=}, meaning contains and does not contain),
#' \code{flag} (\code{==}/\code{!=} against system flags or custom
#' keywords), \code{size} in bytes and \code{age} in seconds
#' (\code{>}, \code{>=}, \code{<}, \code{<=}), the date fields \code{sent},
#' \code{date} (internal date), and \code{saved} (SAVEDATE servers),
#' \code{modseq} (CONDSTORE servers, \code{>=}), and \code{header("Name")}. Date fields accept \code{"YYYY-MM-DD"},
#' \code{"DD-Mon-YYYY"}, or \code{Date} values with \code{>=}, \code{>},
#' \code{<}, \code{<=}, and \code{==}. Comparisons are combined with
#' \code{&}, \code{|}, \code{!}, \code{\%in\%} (one field against several
#' values), and parentheses; a bare string next to \code{|} or \code{&}
#' inherits the field of the preceding comparison. Any other call, such as
#' a criterion constructor like \code{string()}, is evaluated in the
#' caller's environment and used as is.
#'
#' @param expr An unquoted expression, e.g.
#'   \code{(subject == "budget" | "budget 3") & flag != "SEEN"}.
#' @return The translated search string, of class \code{"imap_search"}.
#' @examples
#' imap_query((subject == "budget" | "budget 3") & flag != "SEEN")
#' imap_query(sent >= "2001-10-01" & size > 5e6)
#' imap_query(subject %in% c("budget", "forecast") & age < 86400)
#' @export
imap_query <- function(expr) {
  translate_query(substitute(expr), parent.frame())
}

#' @noRd
translate_query <- function(expr, env) {
  st <- new.env(parent = emptyenv())
  as_imap_search(tq(expr, env, st))
}

#' @noRd
tq <- function(e, env, st) {
  if (is.call(e)) {
    op <- as.character(e[[1]])[1]
    if (op == "(") return(tq(e[[2]], env, st))
    if (op == "!") return(qnot(tq(e[[2]], env, st)))
    if (op %in% c("&", "&&")) return(qand(tq(e[[2]], env, st), tq(e[[3]], env, st)))
    if (op %in% c("|", "||")) return(qor(tq(e[[2]], env, st), tq(e[[3]], env, st)))
    if (op %in% c("==", "!=", ">", "<", ">=", "<=")) return(tq_cmp(op, e[[2]], e[[3]], env, st))
    if (op == "%in%") return(tq_in(e[[2]], e[[3]], env, st))
    if (op == "header") {
      stop('header(...) must appear in a comparison, e.g. header("X-Mailer") == "Outlook".',
           call. = FALSE)
    }
    v <- eval(e, env)
    return(tq_value(v, st, deparse1(e)))
  }
  if (is.symbol(e)) {
    nm <- as.character(e)
    if (nm %in% .query_fields) {
      stop('the field "', nm, '" must appear in a comparison, e.g. ',
           nm, ' == value.', call. = FALSE)
    }
    return(tq_value(eval(e, env), st, nm))
  }
  if (is.atomic(e)) return(tq_value(e, st, deparse1(e)))
  stop("unsupported expression in query(): ", deparse1(e), call. = FALSE)
}

# a value standing alone: a ready-made criterion, or a bare value that
# inherits field and operator from the preceding comparison
#' @noRd
tq_value <- function(v, st, label) {
  if (inherits(v, "imap_search")) return(v)
  if (is.atomic(v) && length(v) == 1) {
    if (is.null(st$make)) {
      stop('a bare value ("', as.character(v), '") can only follow a ',
           "comparison whose field it inherits, e.g. ",
           'subject == "a" | "b".', call. = FALSE)
    }
    return(st$make(v))
  }
  stop("cannot use `", label, "` as a search criterion.", call. = FALSE)
}

#' @noRd
tq_cmp <- function(op, lhs, rhs, env, st) {
  v <- eval(rhs, env)
  if (is.call(lhs) && identical(as.character(lhs[[1]])[1], "header")) {
    fld <- as.character(eval(lhs[[2]], env))
    if (!op %in% c("==", "!=")) {
      stop("header() comparisons support == and != only.", call. = FALSE)
    }
    st$make <- function(x) tq_cmp_header(fld, op, x)
    return(tq_cmp_header(fld, op, v))
  }
  if (!is.symbol(lhs) || !(as.character(lhs) %in% .query_fields)) {
    stop("the left side of `", op, "` must be a field name (",
         paste(.query_fields, collapse = ", "), ') or header("Name").',
         call. = FALSE)
  }
  f <- as.character(lhs)
  st$make <- function(x) tq_cmp_field(f, op, x)
  tq_cmp_field(f, op, v)
}

#' @noRd
tq_in <- function(lhs, rhs, env, st) {
  values <- eval(rhs, env)
  assertthat::assert_that(is.atomic(values), length(values) >= 1,
                          msg = "the right side of %in% must be a vector of values.")
  pieces <- lapply(values, function(v) tq_cmp("==", lhs, v, env, st))
  Reduce(qor, pieces)
}

# note: tq_cmp evaluates its rhs; here the values are already evaluated,
# so wrap them so eval() is the identity
#' @noRd
tq_cmp_header <- function(fld, op, v) {
  out <- as_imap_search(paste0("(HEADER ", fld, " ", quote_imap(v), ")"))
  if (op == "!=") qnot(out) else out
}

#' @noRd
tq_cmp_field <- function(f, op, v) {
  if (f %in% names(.query_string_fields)) {
    if (!op %in% c("==", "!=")) {
      stop('the field "', f, '" supports == (contains) and != only.', call. = FALSE)
    }
    out <- as_imap_search(paste0("(", .query_string_fields[[f]], " ", quote_imap(v), ")"))
    return(if (op == "!=") qnot(out) else out)
  }
  if (f %in% c("flag", "keyword")) {
    if (!op %in% c("==", "!=")) {
      stop("flag comparisons support == and != only.", call. = FALSE)
    }
    fl <- toupper(sub("^\\\\", "", as.character(v)))
    if (fl %in% names(.query_std_flags)) {
      key <- if (op == "==") fl else .query_std_flags[[fl]]
      return(as_imap_search(paste0("(", key, ")")))
    }
    key <- if (op == "==") "KEYWORD " else "UNKEYWORD "
    return(as_imap_search(paste0("(", key, as.character(v), ")")))
  }
  if (f == "size") {
    n <- as.numeric(v)
    assertthat::assert_that(length(n) == 1, !is.na(n), msg = '"size" must be compared with a single number of bytes.')
    fmt <- function(x) format(x, scientific = FALSE, trim = TRUE)
    out <- switch(op,
      ">"  = paste0("(LARGER ", fmt(n), ")"),
      ">=" = paste0("(LARGER ", fmt(n - 1), ")"),
      "<"  = paste0("(SMALLER ", fmt(n), ")"),
      "<=" = paste0("(SMALLER ", fmt(n + 1), ")"),
      "==" = paste0("(LARGER ", fmt(n - 1), " SMALLER ", fmt(n + 1), ")"),
      "!=" = paste0("(NOT (LARGER ", fmt(n - 1), " SMALLER ", fmt(n + 1), "))"),
      stop("unsupported size comparison.", call. = FALSE))
    return(as_imap_search(out))
  }
  if (f == "age") {
    n <- as.numeric(v)
    assertthat::assert_that(length(n) == 1, !is.na(n), msg = '"age" must be compared with a single number of seconds.')
    fmt <- function(x) format(x, scientific = FALSE, trim = TRUE)
    out <- switch(op,
      "<"  = paste0("(YOUNGER ", fmt(n), ")"),
      "<=" = paste0("(YOUNGER ", fmt(n + 1), ")"),
      ">"  = paste0("(OLDER ", fmt(n), ")"),
      ">=" = paste0("(OLDER ", fmt(n - 1), ")"),
      stop('"age" supports <, <=, >, and >= (seconds).', call. = FALSE))
    return(as_imap_search(out))
  }
  if (f == "modseq") {
    n <- as.numeric(v)
    assertthat::assert_that(length(n) == 1, !is.na(n), n >= 0,
                            msg = '"modseq" must be compared with a single non-negative number.')
    fmt <- function(x) format(x, scientific = FALSE, trim = TRUE)
    out <- switch(op,
      ">=" = paste0("(MODSEQ ", fmt(n), ")"),
      ">"  = paste0("(MODSEQ ", fmt(n + 1), ")"),
      "<"  = paste0("(NOT (MODSEQ ", fmt(n), "))"),
      stop('"modseq" supports >=, >, and < (the criterion means at least, RFC 7162).', call. = FALSE))
    return(as_imap_search(out))
  }
  if (f %in% names(.query_date_prefix)) {
    p <- .query_date_prefix[[f]]
    d <- query_date(v)
    out <- switch(op,
      ">=" = paste0("(", p, "SINCE ", fmt_imap_date(d), ")"),
      ">"  = paste0("(", p, "SINCE ", fmt_imap_date(d + 1), ")"),
      "<"  = paste0("(", p, "BEFORE ", fmt_imap_date(d), ")"),
      "<=" = paste0("(", p, "BEFORE ", fmt_imap_date(d + 1), ")"),
      "==" = paste0("(", p, "ON ", fmt_imap_date(d), ")"),
      "!=" = paste0("(NOT (", p, "ON ", fmt_imap_date(d), "))"),
      stop("unsupported date comparison.", call. = FALSE))
    return(as_imap_search(out))
  }
  stop('unknown field "', f, '" in query().', call. = FALSE)
}

#' @noRd
quote_imap <- function(v) {
  paste0('"', gsub('"', '\\\\"', as.character(v)), '"')
}

#' @noRd
query_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (is.character(x) && length(x) == 1) {
    m <- regmatches(x, regexec("^([0-9]{1,2})-([A-Za-z]{3})-([0-9]{4})$", x))[[1]]
    if (length(m) == 4) {
      mo <- match(tolower(m[3]), tolower(month.abb))
      if (!is.na(mo)) {
        return(as.Date(sprintf("%s-%02d-%02d", m[4], mo, as.integer(m[2]))))
      }
    }
    d <- suppressWarnings(tryCatch(as.Date(x), error = function(e) as.Date(NA)))
    if (!is.na(d)) return(d)
  }
  stop('"', as.character(x), '" is not a date; use "YYYY-MM-DD", "DD-Mon-YYYY", or a Date object.',
       call. = FALSE)
}

#' @noRd
fmt_imap_date <- function(d) {
  paste0(as.integer(format(d, "%d")), "-",
         month.abb[as.integer(format(d, "%m"))], "-", format(d, "%Y"))
}
