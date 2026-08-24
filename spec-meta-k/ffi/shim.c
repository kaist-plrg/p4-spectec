/* Thin C shim between K's FFI and the OCaml implementation.
 *
 * K's FFI is a libffi wrapper: it can call any function with a plain C ABI.
 * OCaml cannot expose such a function on its own, so this sits in between --
 * a normal C signature outwards, `caml_callback` inwards:
 *
 *   K rules  --#ffiCall-->  shim.c  --caml_callback-->  p4spec/bin/kffi.ml
 *
 * The shim is linked INTO `al-kompiled/interpreter`, which owns `main()` and
 * is a single long-lived process for the whole `krun`.  That is what makes
 * one-time initialization -- and hence the once-built runner and the
 * persistent builtin counter on the OCaml side -- possible at all.
 *
 * Initialization is explicit, following examples/k/shim.c: K must call
 * `ml_init_c(spec)` once, before any other entry point, naming the target
 * spec.  `al/4.2-extern-ffi.k` puts `initFFI()` at the head of the `<k>` cell
 * to guarantee that.  Removing it gives a segfault with no diagnostic; the
 * shim does not check, per the trade examples/k/shim.c documents. */

#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include <caml/callback.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>

 /* ---- resolved OCaml closure -------------------------------------------
    `caml_named_value` returns a pointer into OCaml's registered-roots table.
    The GC moves the closure it points at, but the table slot itself is stable
    for the life of the process, so resolving once and keeping the pointer is
    correct. */

static const value* ml_init = NULL;
static const value* ml_eval = NULL;

/* The part of init that touches OCaml values, split out so that the
   `CAMLparam0`/`CAMLlocal1` local-roots frame is entered only *after*
   `caml_startup` -- the roots machinery it registers with does not exist
   before the runtime is up. */
static void ml_init_call(const char* spec) {
  CAMLparam0();
  CAMLlocal1(arg_spec);

  /* `ml_init` is deliberately not total (kffi.ml): a bad spec path is a defect
     in the invocation, and there is no configuration worth dumping yet.  So an
     exception here aborts rather than being turned into a value. */
  arg_spec = caml_copy_string(spec);
  caml_callback(*ml_init, arg_spec);

  CAMLreturn0;
}

/* Start the OCaml runtime, resolve the callbacks, and build the runner for
   `spec` -- the target spec path this run answers calls against.  K must call
   this once, before `ml_eval_c`.  Returns 1. */
int64_t ml_init_c(const char* spec) {
  /* K owns main(), so caml_startup() never runs on its own. */
  static char* argv[] = { "k_interpreter", NULL };
  caml_startup(argv);

  ml_init = caml_named_value("ml_init");
  ml_eval = caml_named_value("ml_eval");

  /* Unlike the example, check these: a NULL here means kffi.exe.o was not
     linked into the interpreter, and without the check the failure surfaces
     far away as a NULL dereference inside ml_eval_c. */
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
 * The spec is not passed: `ml_init_c` already built the runner for it.
 * Returns a malloc'd C string; K must free it via `ml_free_c`.  We cannot hand
 * back a pointer into the OCaml heap: the GC may move it.  `CAMLparam0` /
 * `CAMLlocal2` register the intermediates as GC roots. */

char* ml_eval_c(const char* req) {
  CAMLparam0();
  CAMLlocal2(arg_req, res);

  arg_req = caml_copy_string(req);

  /* `ml_eval` is total by construction (kffi.ml catches everything and
     returns an `{"error": ...}` value), so this is defence in depth for the
     one remaining path that would otherwise abort the interpreter with no
     configuration dump. */
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
