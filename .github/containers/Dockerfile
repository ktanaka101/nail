FROM ubuntu:18.04

RUN apt update && apt -y upgrade

# See: https://apt.llvm.org/
RUN apt install -y gnupg2
RUN apt install -y wget

RUN wget -O - https://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add -

# LLVM
RUN apt install -y libllvm-10-ocaml-dev libllvm10 llvm-10 llvm-10-dev llvm-10-doc llvm-10-examples llvm-10-runtime
# Clang and co
# Replaced python-clang-10 to python3-clang-10
RUN apt install -y clang-10 clang-tools-10 clang-10-doc libclang-common-10-dev libclang-10-dev libclang1-10 clang-format-10 python3-clang-10 clangd-10
# libfuzzer
RUN apt install -y libfuzzer-10-dev
# lldb
RUN apt install -y lldb-10
# lld (linker)
RUN apt install -y lld-10
# libc++
RUN apt install -y libc++-10-dev libc++abi-10-dev
# OpenMP
RUN apt install -y libomp-10-dev

# Building error for rust: note: /usr/bin/ld: cannot find -lz
# Required zlib1g-dev
RUN apt install -y zlib1g-dev

ENV PATH $PATH:/usr/lib/llvm-10/bin/

# Rust
RUN apt install -y curl
RUN curl https://sh.rustup.rs -sSf | sh -s -- -y

ENV PATH $PATH:/root/.cargo/bin

RUN rustup toolchain install nightly
