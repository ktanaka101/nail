#!/bin/sh

cd `dirname $0`

SRC_DIR=`pwd`

docker run --rm -it -v ${SRC_DIR}:/usr/src/nail -w /usr/src/nail ktanaka101/rust-and-llvm10-on-ubuntu18.04:latest
