/* Preloaded into the interpreter: it exits at once at an uncaught C++
   exception of a hook, where unwinding takes up to a minute before the abort. */
#include <stdlib.h>
#include <unistd.h>

__attribute__((constructor)) static void only_this_process(void) {
  unsetenv("LD_PRELOAD");
}

void __cxa_throw(void *object, void *type, void (*destructor)(void *)) {
  static const char message[] = "interpreter: uncaught exception\n";
  (void)object, (void)type, (void)destructor;
  if (write(2, message, sizeof message - 1) < 0) {
  }
  _exit(134);
}
