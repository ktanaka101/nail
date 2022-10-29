FROM rust:slim-bullseye AS base

RUN apt-get update && apt-get -y upgrade

# See: https://apt.llvm.org/
RUN apt-get install -y gnupg2
RUN apt-get install -y wget

RUN wget -O - https://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add -

RUN echo 'deb http://apt.llvm.org/bullseye/ llvm-toolchain-bullseye-14 main' >> /etc/apt/sources.list

RUN apt-get update

# LLVM
RUN apt-get install -y libllvm-14-ocaml-dev libllvm14 llvm-14 llvm-14-dev llvm-14-doc llvm-14-runtime
# Clang and co
# Replaced python-clang-10 to python3-clang-10
RUN apt-get install -y clang-14 clang-tools-14 clang-14-doc libclang-common-14-dev libclang-14-dev libclang1-14 clang-format-14 python3-clang-14 clangd-14
# libfuzzer
RUN apt-get install -y libfuzzer-14-dev
# lldb
RUN apt-get install -y lldb-14
# lld (linker)
RUN apt-get install -y lld-14
# libc++
RUN apt-get install -y libc++-14-dev libc++abi-14-dev
# OpenMP
RUN apt-get install -y libomp-14-dev

# Building error for rust: note: /usr/bin/ld: cannot find -lz
# Required zlib1g-dev
RUN apt install -y zlib1g-dev

ENV PATH $PATH:/usr/lib/llvm-14/bin/

# fast build by the rust in docker
ENV CARGO_BUILD_TARGET_DIR=/tmp/target

# llvm
ENV LLVM_SYS_140_STRICT_VERSIONING=140
ENV LLVM_SYS_140_PREFIX=/usr/lib/llvm-14


# for development
FROM base AS development

RUN apt-get install -y git
RUN rustup component add rustfmt clippy rls rust-analysis rust-src

# for CI
FROM base AS ci

RUN rustup component add rustfmt clippy