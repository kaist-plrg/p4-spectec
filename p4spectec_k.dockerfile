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
# Stage 2: Clone repo (brings the k submodule)
# --------------------------------------
FROM base AS source

RUN git clone https://github.com/kaist-plrg/p4-spectec.git && \
    cd p4-spectec && \
    git checkout meta2-cross && \
    git submodule update --init --recursive

WORKDIR /home/p4-spectec

# --------------------------------------
# Stage 3: P4-SpecTec dependencies
# --------------------------------------
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
    opam install dune 'menhir=20240715' 'menhirLib=20240715' bignum core core_unix bisect_ppx yojson ppx_deriving_yojson -y

# Set opam environment permanently
ENV OPAM_SWITCH_PREFIX=/root/.opam/5.1.0
ENV PATH=$OPAM_SWITCH_PREFIX/bin:$PATH
ENV CAML_LD_LIBRARY_PATH=$OPAM_SWITCH_PREFIX/lib/stublibs:$OPAM_SWITCH_PREFIX/lib/ocaml/stublibs:$OPAM_SWITCH_PREFIX/lib/ocaml

# --------------------------------------
# Stage 4: Build P4-SpecTec
# --------------------------------------
FROM opambase AS p4specbase

RUN make build && \
    chmod a+x ./p4spectec

# --------------------------------------
# Stage 5: K framework dependencies
#
# Ubuntu 22.04 does not ship LLVM 17, so it is pulled from apt.llvm.org. The
# K LLVM backend builds and runs against clang/lld 17; the unversioned clang,
# clang++ and ld.lld are pointed at 17 because kompile invokes them by their
# unversioned names to compile and link the generated interpreter.
# --------------------------------------
FROM p4specbase AS kdeps

ARG DEBIAN_FRONTEND=noninteractive

# LLVM 17 toolchain
RUN apt-get update && \
    apt-get install -y lsb-release wget software-properties-common gnupg && \
    wget -O /tmp/llvm.sh https://apt.llvm.org/llvm.sh && \
    chmod +x /tmp/llvm.sh && \
    /tmp/llvm.sh 17 && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

# Remaining K build dependencies (canonical set from upstream install-build-deps)
RUN apt-get update && \
    apt-get install -y \
    bison build-essential cmake flex g++ gcc \
    libboost-test-dev libfmt-dev libgmp-dev libjemalloc-dev libmpfr-dev \
    libsecp256k1-dev libunwind-dev libyaml-dev libz3-dev \
    m4 maven openjdk-17-jdk pkg-config python3 python3-dev xxd z3 zlib1g-dev \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

# Point unversioned clang / clang++ / ld.lld at the LLVM 17 tools kompile uses.
RUN update-alternatives --install /usr/bin/clang   clang   /usr/bin/clang-17   100 && \
    update-alternatives --install /usr/bin/clang++ clang++ /usr/bin/clang++-17 100 && \
    update-alternatives --install /usr/bin/ld.lld  ld.lld  /usr/bin/ld.lld-17  100

# The maven package pulls a default JRE that differs from the installed JDK;
# force Java 17 as the default so java and javac agree, and pin JAVA_HOME to the
# JDK that owns javac via an arch-agnostic symlink (java-17-openjdk-<arch>).
RUN update-java-alternatives -s java-1.17.0-openjdk-$(dpkg --print-architecture) || true; \
    ln -sfn "$(dirname "$(dirname "$(readlink -f "$(command -v javac)")")")" /usr/lib/jvm/java-17
ENV JAVA_HOME=/usr/lib/jvm/java-17

# --------------------------------------
# Stage 6: Build K (LLVM backend only)
#
# -Dhaskell.backend.skip: skip the Haskell/symbolic-execution backend (no
#   Haskell toolchain installed).
# -DskipTests: the frontend unit tests need the mpfr_java JNI native library,
#   which is not built for every platform; tests are not needed to produce the
#   K distribution.
# --------------------------------------
FROM kdeps AS kbuild

WORKDIR /home/p4-spectec/k
RUN mvn package -Dhaskell.backend.skip -DskipTests

# Put kompile / krun / kast on PATH
ENV PATH=/home/p4-spectec/k/k-distribution/target/release/k/bin:$PATH

ENV P4SPECTEC_PATH=/home/p4-spectec
WORKDIR /home/p4-spectec
