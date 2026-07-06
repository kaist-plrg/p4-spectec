#ifndef _NANO_CORE_P4_
#define _NANO_CORE_P4_

header Nanonet {
    bool drop;
    bit<7> packetType;
    bit<8> src;
    bit<8> dst;
}

struct Header {
    Nanonet nanonet;
}

extern packet_in {
    /// Read a header from the packet into a fixed-sized header @hdr and advance the cursor.
    /// May trigger a packet drop
    /// @T must be a fixed-size header type
    void extract(out Nanonet hdr);
}

/// Built-in action that does nothing.
action NoAction() {}

match_kind {
    /// Match bits exactly.
    exact
}

#endif /* _NANO_CORE_P4_ */
