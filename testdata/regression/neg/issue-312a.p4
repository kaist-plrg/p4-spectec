#include <core.p4>
parser P(out bit<8> x);
package Top(P p);
parser MyP(out bit<8> x) {
    state start {
        {
            const bit<8> k = 1;
            { }
            const bit<8> k = 2;
            x = k;
        }
        transition accept;
    }
}
Top(MyP()) main;
