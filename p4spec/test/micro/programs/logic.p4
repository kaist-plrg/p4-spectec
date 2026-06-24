#include <core.p4>

// Interpreter logic: nested iteration, overload resolution, generic substitution, deep binding.

struct pair<T> { T fst; T snd; }

bit<8> g(in bit<8> a) { return a; }
bit<8> g(in bit<8> a, in bit<8> b) { return a + b; }

struct inner { bit<8> a; bit<8> b; }
struct outer { inner i; pair<bit<8>> p; }

control Logic(inout bit<16> x) {
    apply {
        for (bit<4> i in 0 .. 3) {
            for (bit<4> j in 0 .. 3) { x = x + (bit<16>)(i & j); }
        }
        bit<8> a = g(8w1) + g(8w1, 8w2);
        pair<pair<bit<8>>> pp = { { 8w1, 8w2 }, { 8w3, 8w4 } };
        outer o = { { 8w1, 8w2 }, { 8w3, 8w4 } };
        bit<8> z = pp.fst.snd + o.i.a + o.p.fst;
        x = x + (bit<16>)(a + z);
    }
}
