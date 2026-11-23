#include <core.p4>
extern E { E(bit<3> x); }
control C() {
    E(8w255[1 +: 3]) e;   // offset-slice constructor argument
    apply { }
}
control Cx();
package top(Cx c);
top(C()) main;
