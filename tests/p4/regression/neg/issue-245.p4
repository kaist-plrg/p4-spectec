#include <core.p4>
control C();
package Top(C c);
control MyC() {
    bit<8> n = 1;
    action a() { }
    table t {
        actions = { a; }
        default_action = a();
        size = n;
    }
    apply { t.apply(); }
}
Top(MyC()) main;
