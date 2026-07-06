#include <core.p4>

// Header builtins: validity, stack push_front/pop_front/index/size.

header h_t { bit<16> a; bit<16> b; }
struct hdrs { h_t h; h_t[3] stack; }

control H(inout hdrs hs) {
    apply {
        hs.h.setValid();
        if (hs.h.isValid()) { hs.h.a = 16w1; }
        hs.h.setInvalid();

        hs.stack.push_front(1);
        hs.stack.pop_front(1);
        hs.stack[0].setValid();
        hs.stack[0].a = (bit<16>) hs.stack.size;
    }
}
