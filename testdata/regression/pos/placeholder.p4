#include <core.p4>

// Harness placeholder: keeps this dir non-empty (see task-1-report.md).
control C() {
    apply {
        bit<8> b = 8w1;
        bit<16> w = (bit<16>) b;
    }
}
