/* Thin C shim between K's FFI and the OCaml implementation.
 *
 * K's FFI is a libffi wrapper, so it can call any plain C ABI function. OCaml
 * cannot expose one on its own, so this sits in between -- a normal C signature
 * outwards, `caml_callback` inwards:
 *
 *   K rules  --#ffiCall-->  shim.c  --caml_callback-->  p4spec/bin/kffi.ml
 *
 * The shim is a one long-lived process for the whole `krun`. */

#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include <caml/callback.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>

 /* ---- resolved OCaml closure ------------------------------------------- */

static const value* ml_init = NULL;
static const value* ml_eval = NULL;

/* The part of init that touches OCaml values, split out so the
   `CAMLparam0`/`CAMLlocal1` local-roots frame is entered only after
   `caml_startup`: the roots machinery does not exist before the runtime is
   up. */
static void ml_init_call(const char* spec) {
  CAMLparam0();
  CAMLlocal1(arg_spec);

  arg_spec = caml_copy_string(spec);
  caml_callback(*ml_init, arg_spec);

  CAMLreturn0;
}

/* Start the OCaml runtime, resolve the callbacks, and build the runner.
   K must call this once, before `ml_eval_c`.  Returns 1. */
int64_t ml_init_c(const char* spec) {
  /* K owns main(), so caml_startup() never runs on its own. */
  static char* argv[] = { "k_interpreter", NULL };
  caml_startup(argv);

  ml_init = caml_named_value("ml_init");
  ml_eval = caml_named_value("ml_eval");

  if (ml_init == NULL || ml_eval == NULL) {
    fprintf(stderr,
      "shim: caml_named_value(\"ml_init\"/\"ml_eval\") returned NULL -- "
      "kffi.exe.o not linked in?\n");
    abort();
  }

  ml_init_call(spec);

  return 1;
}

/* ---- the one call: JSON request -> JSON reply ---------------------------
 * Returns a malloc'd C string, which K must free via `ml_free_c`.
 * `CAMLparam0`/`CAMLlocal2` register the intermediates as GC roots. */

char* ml_eval_c(const char* req) {
  CAMLparam0();
  CAMLlocal2(arg_req, res);

  arg_req = caml_copy_string(req);
  res = caml_callback_exn(*ml_eval, arg_req);

  if (Is_exception_result(res)) {
    static const char msg[] =
      "{\"error\": \"ml_eval raised through the FFI boundary\"}";
    char* out = (char*)malloc(sizeof(msg));
    if (out != NULL) memcpy(out, msg, sizeof(msg));
    fprintf(stderr, "shim: %s\n", msg);
    CAMLreturnT(char*, out);
  }

  mlsize_t n = caml_string_length(res);
  char* out = (char*)malloc(n + 1);
  if (out != NULL) {
    memcpy(out, String_val(res), n);
    out[n] = '\0';
  }
  CAMLreturnT(char*, out);
}

/* Length of a C string, so K can size its read-back buffer. */
int64_t ml_cstrlen_c(const char* s) {
  return (int64_t)strlen(s);
}

/* Free a buffer previously returned by ml_eval_c. */
void ml_free_c(char* p) {
  free(p);
}
