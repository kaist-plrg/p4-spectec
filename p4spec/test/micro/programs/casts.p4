#include <core.p4>

// Casts: bit<->int<N>, widen/narrow.

control Casts() {
    apply {
        bit<8> b = 8w200;
        int<8> s = (int<8>) b;     // bit -> signed
        bit<8> b2 = (bit<8>) s;    // signed -> bit
        bit<16> w = (bit<16>) b;   // widen
        bit<4> n = (bit<4>) b;     // narrow
    }
}
