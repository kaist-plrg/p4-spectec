#include <core.p4>
struct S { bit<8> a; }
control C() {
    apply {
        S s = { a = 8w1, a = 8w2 };   // duplicate field name in record literal
    }
}
