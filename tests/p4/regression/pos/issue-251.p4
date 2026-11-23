#include <core.p4>

typedef enum E { A } Alias;

Alias f() { return Alias.A; }
