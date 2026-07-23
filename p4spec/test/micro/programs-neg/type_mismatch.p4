#include <core.p4>

// Negative: bit<8> assigned to bit<16> without a cast (must be rejected).
control C() {
    apply {
        bit<8> x = 8w1;
        bit<16> y = x;
    }
}
