#include <core.p4>

// Declarations: error type, tuple, struct literals.

error { Overflow, BadHeader }

struct pair_t { bit<8> lo; bit<8> hi; }

control D() {
    apply {
        tuple<bit<8>, bool> t = { 8w1, true };
        pair_t p = { lo = 8w1, hi = 8w2 };
        bit<8> s = p.lo + p.hi;
        error e = error.Overflow;
    }
}
