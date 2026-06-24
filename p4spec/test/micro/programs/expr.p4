#include <core.p4>

// Expressions: arithmetic, bitwise, shifts, slice, concat, cast, ternary, logical.

bit<16> bit_ops(in bit<16> a, in bit<16> b, in bool p) {
    bit<16> r = a + b;
    r = r - b;
    r = r * 16w3;
    r = r & b;
    r = r | (a << 2);
    r = r ^ (b >> 1);
    r = ~r;
    bit<8> hi = a[15:8];
    bit<8> lo = b[7:0];
    r = r + (hi ++ lo);
    bool c = (a < b) && (a != b) || !(a >= b);
    return (p && c) ? r : (bit<16>)((bit<32>)r >> 1);
}

int<16> int_ops(in int<16> a, in int<16> b) {
    int<16> r = a + b;
    r = r - b;
    r = (a < b) ? -r : r;
    return r;
}

// division / modulo are restricted to compile-time-known operands
const bit<16> Q = 1000 / 7;
const bit<16> M = 1000 % 7;
