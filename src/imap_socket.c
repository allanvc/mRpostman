/* Raw IMAP socket on top of libcurl's CONNECT_ONLY mode.
 *
 * libcurl performs the TCP connection and the TLS handshake (certificate
 * verification, SNI, the system CA bundle), then hands the established
 * connection over: curl_easy_send()/curl_easy_recv() move raw bytes. The IMAP
 * dialogue itself is written in R (see R/raw-session.R). This is what makes
 * IDLE (RFC 2177) and IMAP literals possible, which libcurl's one-shot
 * request model cannot express.
 *
 * The same functionality is proposed to the 'curl' R package as
 * curl_send()/curl_recv() (see dev/PR_curl_jeroen); once available there,
 * this file goes away.
 */
#include <stdlib.h>
#include <string.h>
#include <R.h>
#include <Rinternals.h>
#include <curl/curl.h>
#ifdef _WIN32
#include <winsock2.h>
#else
#include <sys/select.h>
#include <sys/time.h>
#include <unistd.h>
#endif

typedef struct {
  CURL *curl;
} imap_sock;

static void imap_sock_finalizer(SEXP ptr) {
  imap_sock *s = (imap_sock *) R_ExternalPtrAddr(ptr);
  if (s) {
    if (s->curl) curl_easy_cleanup(s->curl);
    free(s);
    R_ClearExternalPtr(ptr);
  }
}

static imap_sock *get_sock(SEXP ptr) {
  if (TYPEOF(ptr) != EXTPTRSXP) error("not a socket handle");
  imap_sock *s = (imap_sock *) R_ExternalPtrAddr(ptr);
  if (!s || !s->curl) error("the socket is closed");
  return s;
}

/* wait until the socket is readable (for_recv) or writable; returns 1 when
 * ready, 0 on timeout, -1 on error */
static int wait_on_socket(curl_socket_t sockfd, int for_recv, long timeout_ms) {
  struct timeval tv;
  fd_set infd, outfd, errfd;
  tv.tv_sec = timeout_ms / 1000;
  tv.tv_usec = (timeout_ms % 1000) * 1000;
  FD_ZERO(&infd); FD_ZERO(&outfd); FD_ZERO(&errfd);
  FD_SET(sockfd, &errfd);
  if (for_recv) FD_SET(sockfd, &infd); else FD_SET(sockfd, &outfd);
  return select((int) sockfd + 1, &infd, &outfd, &errfd, &tv);
}

SEXP C_imap_socket_open(SEXP url, SEXP timeout_ms, SEXP verify_peer) {
  CURL *curl = curl_easy_init();
  if (!curl) error("curl_easy_init() failed");
  curl_easy_setopt(curl, CURLOPT_URL, CHAR(STRING_ELT(url, 0)));
  curl_easy_setopt(curl, CURLOPT_CONNECT_ONLY, 1L);
  curl_easy_setopt(curl, CURLOPT_SSL_VERIFYPEER, asLogical(verify_peer) ? 1L : 0L);
  curl_easy_setopt(curl, CURLOPT_SSL_VERIFYHOST, asLogical(verify_peer) ? 2L : 0L);
  long tmo = (long) asInteger(timeout_ms);
  if (tmo > 0) curl_easy_setopt(curl, CURLOPT_CONNECTTIMEOUT_MS, tmo);
  CURLcode rc = curl_easy_perform(curl);
  if (rc != CURLE_OK) {
    char msg[256];
    snprintf(msg, sizeof(msg), "%s", curl_easy_strerror(rc));
    curl_easy_cleanup(curl);
    error("connection failed: %s", msg);
  }
  imap_sock *s = (imap_sock *) malloc(sizeof(imap_sock));
  if (!s) { curl_easy_cleanup(curl); error("out of memory"); }
  s->curl = curl;
  SEXP ptr = PROTECT(R_MakeExternalPtr(s, R_NilValue, R_NilValue));
  R_RegisterCFinalizerEx(ptr, imap_sock_finalizer, TRUE);
  UNPROTECT(1);
  return ptr;
}

SEXP C_imap_socket_send(SEXP ptr, SEXP data, SEXP timeout_ms) {
  imap_sock *s = get_sock(ptr);
  curl_socket_t sockfd;
  if (curl_easy_getinfo(s->curl, CURLINFO_ACTIVESOCKET, &sockfd) != CURLE_OK)
    error("cannot obtain the socket");
  const unsigned char *buf = RAW(data);
  size_t len = (size_t) XLENGTH(data), sent_total = 0, n = 0;
  long tmo = (long) asInteger(timeout_ms);
  while (sent_total < len) {
    CURLcode rc = curl_easy_send(s->curl, buf + sent_total, len - sent_total, &n);
    if (rc == CURLE_AGAIN) {
      int w = wait_on_socket(sockfd, 0, tmo);
      if (w == 0) error("timeout while sending");
      if (w < 0) error("socket error while sending");
      continue;
    }
    if (rc != CURLE_OK) error("send failed: %s", curl_easy_strerror(rc));
    sent_total += n;
  }
  return ScalarInteger((int) sent_total);
}

/* returns a raw vector with the bytes read (possibly empty on timeout);
 * attribute "closed" = TRUE when the peer closed the connection */
SEXP C_imap_socket_recv(SEXP ptr, SEXP timeout_ms, SEXP max_bytes) {
  imap_sock *s = get_sock(ptr);
  curl_socket_t sockfd;
  if (curl_easy_getinfo(s->curl, CURLINFO_ACTIVESOCKET, &sockfd) != CURLE_OK)
    error("cannot obtain the socket");
  long tmo = (long) asInteger(timeout_ms);
  size_t cap = (size_t) asInteger(max_bytes), n = 0;
  if (cap < 1) cap = 1;
  unsigned char *buf = (unsigned char *) R_alloc(cap, 1);
  CURLcode rc = curl_easy_recv(s->curl, buf, cap, &n);
  if (rc == CURLE_AGAIN) {
    int w = wait_on_socket(sockfd, 1, tmo);
    if (w == 0) return allocVector(RAWSXP, 0);         /* timeout */
    if (w < 0) error("socket error while receiving");
    rc = curl_easy_recv(s->curl, buf, cap, &n);
    if (rc == CURLE_AGAIN) return allocVector(RAWSXP, 0);
  }
  if (rc != CURLE_OK) error("recv failed: %s", curl_easy_strerror(rc));
  SEXP out = PROTECT(allocVector(RAWSXP, n));
  if (n > 0) memcpy(RAW(out), buf, n);
  if (n == 0) setAttrib(out, install("closed"), ScalarLogical(1));
  UNPROTECT(1);
  return out;
}

SEXP C_imap_socket_close(SEXP ptr) {
  if (TYPEOF(ptr) == EXTPTRSXP) imap_sock_finalizer(ptr);
  return ScalarLogical(1);
}

SEXP C_imap_socket_is_open(SEXP ptr) {
  if (TYPEOF(ptr) != EXTPTRSXP) return ScalarLogical(0);
  imap_sock *s = (imap_sock *) R_ExternalPtrAddr(ptr);
  return ScalarLogical(s != NULL && s->curl != NULL);
}
