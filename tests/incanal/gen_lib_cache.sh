#!/bin/bash

_base=$(e=$0;while test -L "$e";do d=$(dirname "$e");e=$(readlink "$e");\
                                   cd "$d";done;cd "$(dirname "$e")";pwd -P)

pushd "$_base" > /dev/null

# TODO: hardwired directory
cached_assertions="../../../../build/data/ciaopp_lib_cache"
echo "Generating cached assertions for libraries $cached_assertions ..."

mkdir -p "../../../../build/data"
mkdir -p $cached_assertions

gen_lib_cache $cached_assertions

popd > /dev/null
