# --------------------------------------
# Stage 1: System dependencies
# --------------------------------------
FROM ubuntu:22.04 AS base

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update && \
    apt-get install -y git make curl && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

WORKDIR /home

# --------------------------------------
# Stage 2: Clone repo
# --------------------------------------
FROM base AS source

RUN git clone https://github.com/kaist-plrg/p4-spectec.git && \
    cd p4-spectec && \
    git checkout ntt-syntax && \
    git submodule update --init --recursive

WORKDIR /home/p4-spectec

# ---------------------------------------
# Stage 3: P4-SpecTec dependencies
# ---------------------------------------
FROM source AS opambase

ARG DEBIAN_FRONTEND=noninteractive
ENV TZ=Asia/Seoul

RUN apt-get update && \
    apt-get install -y opam libgmp-dev pkg-config && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

# Initialize opam
RUN opam init --disable-sandboxing --auto-setup && \
    opam switch create 5.1.0 && \
    eval $(opam env) && \
    opam install dune menhir bignum core core_unix bisect_ppx -y

# Set opam environment permanently
ENV OPAM_SWITCH_PREFIX=/root/.opam/5.1.0
ENV PATH=$OPAM_SWITCH_PREFIX/bin:$PATH
ENV CAML_LD_LIBRARY_PATH=$OPAM_SWITCH_PREFIX/lib/stublibs:$OPAM_SWITCH_PREFIX/lib/ocaml/stublibs:$OPAM_SWITCH_PREFIX/lib/ocaml

# ---------------------------------------
# Stage 4: Build P4-SpecTec
# ---------------------------------------
FROM opambase AS p4specbase

RUN make build-spec && \
    chmod a+x ./p4spectec

# --------------------------------------
# Stage 5: Fuzzer & Reducer dependencies
# --------------------------------------
FROM p4specbase AS reducebase

RUN apt-get update && \
    apt-get install -y clang creduce python3 pip && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

RUN python3 -m pip install psutil
COPY patches/creduce /usr/bin/creduce
RUN chmod +x /usr/bin/creduce

ENV P4SPECTEC_PATH=/home/p4-spectec
