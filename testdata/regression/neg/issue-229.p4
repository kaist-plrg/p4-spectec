#include <core.p4>

bit<8> f(bit<8> a, bit<8> b = a) {
    return b;
}
