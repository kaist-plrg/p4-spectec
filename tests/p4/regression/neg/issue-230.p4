#include <core.p4>

extern E { E(bit<8> x, bit<8> y = x); }

E(8w1) e;
