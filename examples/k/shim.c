/* Thin C shim: plain C ABI on the outside (so libffi/K can call it),
   OCaml callbacks on the inside.

   The shim is linked INTO the K interpreter, which is a single long-lived
   process: static state persists across FFI calls, so the OCaml runtime is
   started once and reused. */

#include <stdint.h>
#include <string.h>
#include <stdlib.h>

#include <caml/callback.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>

/* K owns main(), so caml_startup() never runs on its own. Start it on first
   use. caml_startup is itself idempotent, but calling it once is measurably
   cheaper than calling it per FFI call. Single-threaded: the LLVM backend
   calls FFI hooks from one thread. */
static int ocaml_ready = 0;

static void ensure_ocaml(void) {
    if (!ocaml_ready) {
        static char *argv[] = { "k_interpreter", NULL };
        caml_startup(argv);
        ocaml_ready = 1;
    }
}

/* ---- int -> int -> int ------------------------------------------------- */

int64_t ml_add_c(int64_t a, int64_t b) {
    ensure_ocaml();
    static const value *f = NULL;
    if (f == NULL) f = caml_named_value("ml_add");
    return (int64_t) Int_val(caml_callback2(*f, Val_long(a), Val_long(b)));
}

/* ---- int -> int -------------------------------------------------------- */

int64_t ml_fib_c(int64_t n) {
    ensure_ocaml();
    static const value *f = NULL;
    if (f == NULL) f = caml_named_value("ml_fib");
    return (int64_t) Int_val(caml_callback(*f, Val_long(n)));
}

/* ---- string -> string --------------------------------------------------
   Returns a malloc'd C string. The caller (K) must free it via ml_free_c.
   We cannot hand back a pointer into the OCaml heap: the GC may move it. */

static char *call_string_fn(const char *name, const value **cache,
                            const char *s) {
    CAMLparam0();
    CAMLlocal2(arg, res);
    if (*cache == NULL) *cache = caml_named_value(name);
    arg = caml_copy_string(s);
    res = caml_callback(**cache, arg);
    mlsize_t n = caml_string_length(res);
    char *out = (char *) malloc(n + 1);
    if (out != NULL) {
        memcpy(out, String_val(res), n);
        out[n] = '\0';
    }
    CAMLreturnT(char *, out);
}

char *ml_upper_c(const char *s) {
    ensure_ocaml();
    static const value *f = NULL;
    return call_string_fn("ml_upper", &f, s);
}

char *ml_describe_c(const char *s) {
    ensure_ocaml();
    static const value *f = NULL;
    return call_string_fn("ml_describe", &f, s);
}

/* Length of a C string, so K can size its read-back buffer. */
int64_t ml_cstrlen_c(const char *s) {
    return (int64_t) strlen(s);
}

/* Free a buffer previously returned by ml_upper_c / ml_describe_c. */
void ml_free_c(char *p) {
    free(p);
}
