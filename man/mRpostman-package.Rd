\docType{package}
\name{mRpostman-package}
\alias{mRpostman-package}
\alias{mRpostman}
\title{An IMAP client for R}

\description{
  \pkg{mRpostman} is a session-based IMAP client that implements the full
    command sets of the IMAP4rev2 (RFC 9051) and IMAP4rev1 (RFC 3501)
    protocols, along with the optional extensions registered with IANA,
    allowing virtually all e-mail operations to be performed from within
    \R, paving the way for e-mail data analysis.
}

\section{Options}{
  \describe{
    \item{\code{mRpostman.raw_debug}}{If \code{TRUE}, prints the raw
      dialogue of the second (event) connection used by \code{idle()},
      \code{append_msgs()}, and the other raw-socket commands, in the same
      spirit as \code{verbose = TRUE} for the main connection. Default is
      \code{FALSE}.}
  }
}


\author{

  Author & Mantainer: Allan Quadros \email{allanvcq@gmail.com}

}

\references{

Crispin, M. (2003), \emph{INTERNET MESSAGE ACCESS PROTOCOL - VERSION 4rev1}, RFC 3501, March 2003, \url{https://www.rfc-editor.org/rfc/rfc3501}.

Heinlein, P. and Hartleben, P. (2008). \emph{The Book of IMAP: Building a Mail Server with Courier and Cyrus}. No Starch Press. ISBN 978-1-59327-177-0.

Ooms, J. (2020). \emph{curl: A Modern and Flexible Web Client for R}. R package version 4.3, \url{https://CRAN.R-project.org/package=curl}.

Stenberg, D. \emph{Libcurl - The Multiprotocol File Transfer Library}, \url{https://curl.se/libcurl/}.

}

\seealso{

  Useful links:

  \itemize{
    \item{ \code{mRpostman official website}: \url{https://allanvc.github.io/mRpostman/}}
  }

}
