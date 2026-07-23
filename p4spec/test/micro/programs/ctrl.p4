#include <core.p4>

// Statements: if/else, for-in, C-style for, blocks, actions.

control C(inout bit<16> x, in bit<8> sel) {
    action incr() { x = x + 1; }
    action setto(bit<16> v) { x = v; }
    apply {
        if (x > 16w10) { x = x - 1; } else { x = x + 1; }
        for (bit<4> i in 0 .. 5) { x = x + (bit<16>)i; }
        bit<8> n = 0;
        for (bit<8> j = 0; j < 4; j = j + 1) { n = n + j; }
        incr();
        setto(16w5 + (bit<16>)n);
    }
}
