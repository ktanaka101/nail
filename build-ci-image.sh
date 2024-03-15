#!/bin/bash

tag_name=""
should_push=false

usage() {
  echo "Usage: $0 [-t tagname] [-p(push)]"
}

while getopts pt:h OPT
do
  case $OPT in
    "t" ) tag_name=$OPTARG ;;
    "p" ) should_push=true ;;
    "h" ) usage; exit 1 ;;
  esac
done

cmd_args=""
if [ "$tag_name" = "" ]; then
  echo "Please set '-t TAG_NAME'"
  exit 1
else
  cmd_args+="-t $tag_name"
fi

if $should_push; then
  cmd_args+=" --push"
fi

docker buildx build --platform linux/arm64,linux/amd64 --target ci $cmd_args .
