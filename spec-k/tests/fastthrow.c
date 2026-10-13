/* Preloaded into the LLVM backend's interpreter by difftest.py: a hook given
   an invalid argument throws a C++ exception that nothing catches (the
   runtime and the generated code have no handlers), and unwinding the
   interpreter's stack to find out takes about a minute before it aborts.
   Exiting with the status of abort at the throw gives the same outcome at
   once. Programs the interpreter starts do not get it. */
#include <stdlib.h>
#include <unistd.h>

__attribute__((constructor)) static void only_this_process(void) {
  unsetenv("LD_PRELOAD");
}

void __cxa_throw(void *object, void *type, void (*destructor)(void *)) {
  static const char message[] = "interpreter: uncaught exception (hook given an invalid argument)\n";
  (void)object, (void)type, (void)destructor;
  if (write(2, message, sizeof message - 1) < 0) {
  }
  _exit(134);
}
