#include <core.p4>
void f<T>() { }
control C() {
    apply {
        f<NotDeclared>();   // undeclared identifier as a type argument
    }
}
