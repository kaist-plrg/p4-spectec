#include <core.p4>
extern E { E(bit<4> x); }
control C(in bit<3> off) {
    E(8w255[off +: 4]) e;   // dynamic-offset slice as a constructor argument
    apply { }
}
control Cx(in bit<3> off);
package top(Cx c);
top(C()) main;
