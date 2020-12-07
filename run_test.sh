#!/bin/sh

cd `dirname $0`

SRC_DIR=`pwd`

docker run --rm -v ${SRC_DIR}:/usr/src/nail -w /usr/src/nail ktanaka101/nail-builder:latest cargo test
