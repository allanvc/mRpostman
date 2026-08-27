#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

SEXP C_imap_socket_open(SEXP, SEXP, SEXP);
SEXP C_imap_socket_send(SEXP, SEXP, SEXP);
SEXP C_imap_socket_recv(SEXP, SEXP, SEXP);
SEXP C_imap_socket_close(SEXP);
SEXP C_imap_socket_is_open(SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"C_imap_socket_open",    (DL_FUNC) &C_imap_socket_open,    3},
  {"C_imap_socket_send",    (DL_FUNC) &C_imap_socket_send,    3},
  {"C_imap_socket_recv",    (DL_FUNC) &C_imap_socket_recv,    3},
  {"C_imap_socket_close",   (DL_FUNC) &C_imap_socket_close,   1},
  {"C_imap_socket_is_open", (DL_FUNC) &C_imap_socket_is_open, 1},
  {NULL, NULL, 0}
};

void R_init_mRpostman(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
