#include <core.p4>

// A derived type nested in a typedef introduces its own inner type name, not
// only the alias (issue #251): E, SE, S, H must all resolve as type names.

typedef enum E { A, B } Alias;
typedef enum bit<8> SE { X = 8w1, Y = 8w2 } SEAlias;
typedef struct S { bit<8> f; } SAlias;
typedef header H { bit<16> v; } HAlias;

E f() { return E.A; }

control C() {
    apply {
        E e = E.B;                  // inner enum name resolves
        bit<8> x = (bit<8>) SE.X;   // inner serializable-enum name resolves
        S s = { f = 8w7 };          // inner struct name as a type
        H h = { v = 16w1 };         // inner header name as a type
    }
}
