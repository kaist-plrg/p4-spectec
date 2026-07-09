#include <core.p4>

// Tables: exact/ternary/lpm keys, const entries, action_run switch, .hit.

control T(inout bit<16> x, in bit<8> k1, in bit<8> k2) {
    action drop() { x = 0; }
    action setx(bit<16> v) { x = v; }
    action nop() {}

    table t {
        key = {
            k1 : exact;
            k2 : ternary;
        }
        actions = { drop; setx; nop; }
        default_action = nop();
        const entries = {
            (8w1, 8w2 &&& 8w0xF0) : setx(16w5);
            (8w3, _)              : drop();
        }
        size = 16;
    }

    table lpm_t {
        key = { k1 : lpm; }
        actions = { setx; nop; }
        default_action = nop();
    }

    apply {
        switch (t.apply().action_run) {
            setx: { x = x + 1; }
            drop: { x = x - 1; }
            default: {}
        }
        if (lpm_t.apply().hit) { x = x + 16w100; }
    }
}
