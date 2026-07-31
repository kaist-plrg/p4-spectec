#include <core.p4>

// Functions: nested calls, out/inout args, generic instantiation.

bit<8> add(in bit<8> a, in bit<8> b) { return a + b; }
bit<8> twice(in bit<8> a) { return add(a, a); }

void swap(inout bit<8> a, inout bit<8> b) { bit<8> t = a; a = b; b = t; }
void setit(out bit<8> a) { a = 8w9; }

T id<T>(in T x) { return x; }

control UseFuncs() {
    apply {
        bit<8> x = 8w1;
        bit<8> y = 8w2;
        swap(x, y);
        setit(x);
        bit<8> z = id<bit<8>>(twice(x));
    }
}
