#!/bin/bash

_base=$(e=$0;while test -L "$e";do d=$(dirname "$e");e=$(readlink "$e");\
        cd "$d";done;cd "$(dirname "$e")";pwd -P)

tests=(hanoi aiakl qsort progeom bid rdtok cleandirs prolog_read warplan boyer peephole witt ann manag_proj_simple check_links)
configs=(add del)

if [ "$#" -ne 1 ] && [ "$#" -ne 2 ]; then
          echo "Usage: $0 <domain> [\"extra arguments\"]"
    exit
fi

ciaopp --gen-lib-cache

pushd $_base > /dev/null 2>&1

echo "Compiling tests..."
./compile.sh

mkdir -p test_results/

echo "Running tests..."

for k in "${configs[@]}" ; do
    for i in "${tests[@]}" ; do
        ./run_configs.sh $i $k "$1" "$2"
    done
done

# for i in "${tests[@]}" ; do
#     for j in "${domains[@]}" ; do
#         ./check_config.sh $i $j
#         errors=$($errors+$?) # add errors
#     done
# done

popd > /dev/null 2>&1

