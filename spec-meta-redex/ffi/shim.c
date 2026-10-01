/* C shim between Racket's ffi2 and the OCaml implementation.
 *
 *   common/0.3-extern-ffi.rkt  --ffi2-->  shim.c  --caml_callback-->
 *   p4spec/bin/ffi.ml
 *
 * Every call must come from the OS thread that ran the first host_init. */

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

static const value* ml_init = NULL;
static const value* ml_eval = NULL;

/* The last reply, owned here and freed by the next host_eval. */
static char* reply = NULL;

static const char reply_raised[] =
  "{\"error\": \"ml_eval raised through the FFI boundary\"}";
static const char reply_uninit[] =
  "{\"error\": \"host_eval called before host_init\"}";

/* Builds the runner for spec; 0 if ml_init raises. */
static int init_runner(const char* spec) {
  CAMLparam0();
  CAMLlocal2(arg, res);

  arg = caml_copy_string(spec);
  res = caml_callback_exn(*ml_init, arg);
  CAMLreturnT(int, !Is_exception_result(res));
}

/* Starts the runtime on the first call, then (re)builds the runner for spec.
   Returns 1, 0 if ml_init raised, or -1 if ffi.ml's callbacks are missing. */
int64_t host_init(const char* spec) {
  if (ml_init == NULL || ml_eval == NULL) {
    static char* argv[] = { "racket", NULL };
    caml_startup(argv);
    ml_init = caml_named_value("ml_init");
    ml_eval = caml_named_value("ml_eval");
    if (ml_init == NULL || ml_eval == NULL) return -1;
  }
  return init_runner(spec);
}

/* JSON request -> JSON reply, valid until the next call. */
const char* host_eval(const char* req) {
  CAMLparam0();
  CAMLlocal2(arg, res);

  if (ml_eval == NULL) CAMLreturnT(const char*, reply_uninit);

  free(reply);
  reply = NULL;

  arg = caml_copy_string(req);
  res = caml_callback_exn(*ml_eval, arg);
  if (Is_exception_result(res)) CAMLreturnT(const char*, reply_raised);

  mlsize_t n = caml_string_length(res);
  reply = (char*)malloc(n + 1);
  if (reply == NULL) abort();
  memcpy(reply, String_val(res), n);
  reply[n] = '\0';
  CAMLreturnT(const char*, reply);
}
