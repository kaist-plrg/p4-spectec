#include <core.p4>

// Permanent placeholder: the run/sim harness fails on an empty corpus dir;
// do not remove.
control C() {
    apply {
        bit<8> x = 8w1;
        bit<16> y = x;
    }
}
