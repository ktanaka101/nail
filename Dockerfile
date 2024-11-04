FROM rust:1.82-slim-bookworm AS base

RUN apt update && apt -y upgrade && \
  apt install -y gnupg2 wget curl && \
  # llvm
  wget -O - https://apt.llvm.org/llvm-snapshot.gpg.key | gpg --dearmor > /usr/share/keyrings/llvm-snapshot.gpg && \
  echo 'deb [signed-by=/usr/share/keyrings/llvm-snapshot.gpg] http://apt.llvm.org/bookworm/ llvm-toolchain-bookworm-18 main' >> /etc/apt/sources.list && \
  apt update && \
  apt install -y libllvm-18-ocaml-dev libllvm18 llvm-18 llvm-18-dev llvm-18-doc llvm-18-examples llvm-18-runtime \
  clang-18 clang-tools-18 clang-18-doc libclang-common-18-dev libclang-18-dev libclang1-18 clang-format-18 python3-clang-18 clangd-18 clang-tidy-18 \
  libpolly-18-dev \
  libfuzzer-18-dev \
  lldb-18 \
  lld-18 \
  libc++-18-dev libc++abi-18-dev \
  libomp-18-dev \
  libclc-18-dev \
  libunwind-18-dev \
  libmlir-18-dev mlir-18-tools \
  # llvm dependency (99bc465)
  zlib1g-dev \
  zstd \
  libzstd-dev

# Environment variables are required by llvm-sys
# LLVM-18.0 needs LLVM_SYS_180_STRICT_VERSIONING=180, LLVM_SYS_181_PREFIX=/usr/lib/llvm-18/
# LLVM-18.1 needs LLVM_SYS_181_STRICT_VERSIONING=181, LLVM_SYS_181_PREFIX=/usr/lib/llvm-18/
ENV LLVM_SYS_181_STRICT_VERSIONING=181 \
  LLVM_SYS_181_PREFIX=/usr/lib/llvm-18/ \
  PATH=$PATH:/usr/lib/llvm-18/bin/

# Node
RUN curl -sL https://deb.nodesource.com/setup_18.x | bash - && \
  apt install -y nodejs && \
  npm install -g yarn && \
  ## Required for the `vscode-test`
  apt install -y libglib2.0-dev libnss3 libdbus-1-3	libatk1.0-0 libatk-bridge2.0-0 libgtk-3-0	libgbm1 libasound2 xvfb && \
  /etc/init.d/dbus restart && \
  # Rust
  rustup default nightly-2024-11-04 && \
  ## install cargo-nextest
  curl -L --proto '=https' --tlsv1.2 -sSf https://raw.githubusercontent.com/cargo-bins/cargo-binstall/main/install-from-binstall-release.sh | bash && \
  cargo binstall cargo-nextest@^0.9 --secure --no-confirm && \
  cargo binstall cargo-make@^0.37 --secure --no-confirm

# ---------------------------------------
FROM base AS development

ENV CARGO_BUILD_TARGET_DIR=/tmp/target \
  NAIL_LANGUAGE_SERVER_PATH=/tmp/target/debug/nail-language-server

RUN apt install -y git g++ && \
  apt clean && \
  rm -rf /var/lib/apt/lists/* && \
  # fuzzing
  cargo binstall cargo-fuzz@^0.12 --no-confirm && \
  # for rust development
  rustup component add rustfmt clippy \
  # for rust-analyzer
  rust-src

# ---------------------------------------
FROM base AS ci

RUN apt clean && \
  rm -rf /var/lib/apt/lists/* && \
  rustup component add rustfmt clippy
