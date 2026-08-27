/* Streaming raw-deflate (RFC 1951) for the IMAP COMPRESS=DEFLATE extension
 * (RFC 4978): after "COMPRESS DEFLATE", both directions of the connection
 * are deflate streams that must be flushed after every command (deflate) and
 * inflated incrementally as bytes arrive. */
#include <stdlib.h>
#include <string.h>
#include <R.h>
#include <Rinternals.h>
#include <zlib.h>

typedef struct { z_stream strm; int inflating; int ended; } zstate;

static void zstate_finalizer(SEXP ptr) {
  zstate *z = (zstate *) R_ExternalPtrAddr(ptr);
  if (z) {
    if (!z->ended) { if (z->inflating) inflateEnd(&z->strm); else deflateEnd(&z->strm); }
    free(z);
    R_ClearExternalPtr(ptr);
  }
}

static zstate *get_zstate(SEXP ptr) {
  if (TYPEOF(ptr) != EXTPTRSXP) error("not a zlib stream");
  zstate *z = (zstate *) R_ExternalPtrAddr(ptr);
  if (!z || z->ended) error("the zlib stream is closed");
  return z;
}

SEXP C_zstream_new(SEXP inflating, SEXP level) {
  zstate *z = (zstate *) calloc(1, sizeof(zstate));
  if (!z) error("out of memory");
  z->inflating = asLogical(inflating) ? 1 : 0;
  int rc = z->inflating ? inflateInit2(&z->strm, -15)
                        : deflateInit2(&z->strm, asInteger(level), Z_DEFLATED, -15, 8, Z_DEFAULT_STRATEGY);
  if (rc != Z_OK) { free(z); error("zlib initialisation failed (%d)", rc); }
  SEXP ptr = PROTECT(R_MakeExternalPtr(z, R_NilValue, R_NilValue));
  R_RegisterCFinalizerEx(ptr, zstate_finalizer, TRUE);
  UNPROTECT(1);
  return ptr;
}

/* deflate `data` and flush (Z_SYNC_FLUSH), so that the peer can decode it now */
SEXP C_zstream_deflate(SEXP ptr, SEXP data) {
  zstate *z = get_zstate(ptr);
  if (z->inflating) error("not a deflate stream");
  size_t in_len = (size_t) XLENGTH(data);
  size_t cap = deflateBound(&z->strm, (uLong) in_len) + 64;
  unsigned char *out = (unsigned char *) R_alloc(cap, 1);
  z->strm.next_in = (Bytef *) RAW(data); z->strm.avail_in = (uInt) in_len;
  z->strm.next_out = out; z->strm.avail_out = (uInt) cap;
  int rc = deflate(&z->strm, Z_SYNC_FLUSH);
  if (rc != Z_OK && rc != Z_BUF_ERROR) error("deflate failed (%d)", rc);
  size_t n = cap - z->strm.avail_out;
  SEXP res = PROTECT(allocVector(RAWSXP, n));
  if (n) memcpy(RAW(res), out, n);
  UNPROTECT(1);
  return res;
}

/* inflate whatever is available in `data`; output may be empty */
SEXP C_zstream_inflate(SEXP ptr, SEXP data) {
  zstate *z = get_zstate(ptr);
  if (!z->inflating) error("not an inflate stream");
  size_t in_len = (size_t) XLENGTH(data);
  size_t cap = in_len * 4 + 1024, n = 0;
  unsigned char *out = (unsigned char *) malloc(cap);
  if (!out) error("out of memory");
  z->strm.next_in = (Bytef *) RAW(data); z->strm.avail_in = (uInt) in_len;
  for (;;) {
    z->strm.next_out = out + n; z->strm.avail_out = (uInt) (cap - n);
    int rc = inflate(&z->strm, Z_SYNC_FLUSH);
    n = cap - z->strm.avail_out;
    if (rc == Z_STREAM_END || (rc == Z_OK && z->strm.avail_in == 0 && z->strm.avail_out > 0)) break;
    if (rc == Z_BUF_ERROR && z->strm.avail_in == 0) break;
    if (rc != Z_OK && rc != Z_BUF_ERROR) { free(out); error("inflate failed (%d)", rc); }
    if (z->strm.avail_out == 0) {           /* grow the output buffer */
      cap *= 2;
      unsigned char *tmp = (unsigned char *) realloc(out, cap);
      if (!tmp) { free(out); error("out of memory"); }
      out = tmp;
    }
  }
  SEXP res = PROTECT(allocVector(RAWSXP, n));
  if (n) memcpy(RAW(res), out, n);
  free(out);
  UNPROTECT(1);
  return res;
}
