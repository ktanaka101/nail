FROM rust:slim

RUN apt-get update && apt-get -y upgrade

# See: https://apt.llvm.org/
RUN apt-get install -y gnupg2
RUN apt-get install -y wget

RUN wget -O - https://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add -

RUN echo 'deb http://apt.llvm.org/buster/ llvm-toolchain-buster-10 main' >> /etc/apt/sources.list

RUN apt-get update

# LLVM
RUN apt-get install -y libllvm-10-ocaml-dev libllvm10 llvm-10 llvm-10-dev llvm-10-doc llvm-10-examples llvm-10-runtime
# Clang and co
# Replaced python-clang-10 to python3-clang-10
RUN apt-get install -y clang-10 clang-tools-10 clang-10-doc libclang-common-10-dev libclang-10-dev libclang1-10 clang-format-10 python3-clang-10 clangd-10
# libfuzzer
RUN apt-get install -y libfuzzer-10-dev
# lldb
RUN apt-get install -y lldb-10
# lld (linker)
RUN apt-get install -y lld-10
# libc++
RUN apt-get install -y libc++-10-dev libc++abi-10-dev
# OpenMP
RUN apt-get install -y libomp-10-dev

# Building error for rust: note: /usr/bin/ld: cannot find -lz
# Required zlib1g-dev
RUN apt install -y zlib1g-dev

ENV PATH $PATH:/usr/lib/llvm-10/bin/

# fast build by the rust in docker
ENV CARGO_BUILD_TARGET_DIR=/tmp/target
