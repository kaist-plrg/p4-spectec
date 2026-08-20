/* Thin C shim: plain C ABI on the outside (so libffi/K can call it),
   OCaml callbacks on the inside.

   The shim is linked INTO the K interpreter, which is a single long-lived
   process, so global state persists across FFI calls.

   Initialization is explicit: K must call ml_init_c() once, before any other
   entry point. It starts the OCaml runtime and resolves every registered
   closure up front. No entry point checks that this happened -- calling one
   first dereferences a NULL closure pointer. */

#include <stdint.h>
#include <string.h>
#include <stdlib.h>

#include <caml/callback.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>

/* ---- resolved OCaml closures ------------------------------------------
   caml_named_value returns a pointer into OCaml's registered-roots table.
   That pointer stays valid for the life of the process even though the GC
   moves the closure it points at, so it is safe to resolve once and keep. */

static const value *ml_add      = NULL;
static const value *ml_fib      = NULL;
static const value *ml_upper    = NULL;
static const value *ml_describe = NULL;

/* Start the OCaml runtime and resolve all closures.
   K must call this once before any other function here. Returns 1. */
int64_t ml_init_c(void) {
    /* K owns main(), so caml_startup() never runs on its own. */
    static char *argv[] = { "k_interpreter", NULL };
    caml_startup(argv);

    ml_add      = caml_named_value("ml_add");
    ml_fib      = caml_named_value("ml_fib");
    ml_upper    = caml_named_value("ml_upper");
    ml_describe = caml_named_value("ml_describe");

    return 1;
}

/* ---- int -> int -> int ------------------------------------------------- */

int64_t ml_add_c(int64_t a, int64_t b) {
    return (int64_t) Int_val(caml_callback2(*ml_add, Val_long(a), Val_long(b)));
}

/* ---- int -> int -------------------------------------------------------- */

int64_t ml_fib_c(int64_t n) {
    return (int64_t) Int_val(caml_callback(*ml_fib, Val_long(n)));
}

/* ---- string -> string --------------------------------------------------
   Returns a malloc'd C string. The caller (K) must free it via ml_free_c.
   We cannot hand back a pointer into the OCaml heap: the GC may move it. */

static char *call_string_fn(const value *f, const char *s) {
    CAMLparam0();
    CAMLlocal2(arg, res);
    arg = caml_copy_string(s);
    res = caml_callback(*f, arg);
    mlsize_t n = caml_string_length(res);
    char *out = (char *) malloc(n + 1);
    if (out != NULL) {
        memcpy(out, String_val(res), n);
        out[n] = '\0';
    }
    CAMLreturnT(char *, out);
}

char *ml_upper_c(const char *s) {
    return call_string_fn(ml_upper, s);
}

char *ml_describe_c(const char *s) {
    return call_string_fn(ml_describe, s);
}

/* Length of a C string, so K can size its read-back buffer. */
int64_t ml_cstrlen_c(const char *s) {
    return (int64_t) strlen(s);
}

/* Free a buffer previously returned by ml_upper_c / ml_describe_c. */
void ml_free_c(char *p) {
    free(p);
}
