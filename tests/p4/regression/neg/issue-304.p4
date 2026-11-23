#include <core.p4>
control C();
package Top(C c);
control MyC() {
    apply {
        for (bit<8> i = 0; false; i = i + 1) ;
        break;
    }
}
Top(MyC()) main;
