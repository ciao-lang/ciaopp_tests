#!/bin/bash

_base=$(e=$0;while test -L "$e";do d=$(dirname "$e");e=$(readlink "$e");\
                                   cd "$d";done;cd "$(dirname "$e")";pwd -P) || return

editions=(add del)
domains=(pdb gr def shfr)

pushd "$_base"

mkdir -p "$_base"/csv

for edit in "${editions[@]}" ; do
    for absint in "${domains[@]}" ; do
        ./generate_csv_config.sh $edit $absint
    done
done

popd
