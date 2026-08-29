#' Deprecated functions and methods
#'
#' As of the 2026 refactoring (3.0.0), the expression-based
#' \href{#method-query}{\code{ImapCon$query()}} is the canonical search
#' interface, and criteria combine with R's own operators (\code{&},
#' \code{|}, \code{!}). The forms below keep working but signal a
#' deprecation warning once per session:
#' \itemize{
#'   \item the \code{search_*()} method family - each call has a direct
#'     \code{query()} spelling, e.g. \code{search_before("02-Jan-2020")} is
#'     \code{query(date < "2020-01-02")};
#'   \item \code{AND(...)} and \code{OR(...)} - write
#'     \code{crit1 & crit2} and \code{crit1 | crit2} instead.
#' }
#' @name mRpostman-deprecated
#' @keywords internal
#' @importFrom lifecycle deprecate_warn
NULL
