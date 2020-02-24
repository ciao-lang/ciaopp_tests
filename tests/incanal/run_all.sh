#!/bin/bash

_base=$(e=$0;while test -L "$e";do d=$(dirname "$e");e=$(readlink "$e");\
        cd "$d";done;cd "$(dirname "$e")";pwd -P)

tests=(hanoi aiakl qsort progeom bid rdtok cleandirs prolog_read warplan boyer peephole witt ann)
configs=(add del)

if [ "$#" -ne 1 ]; then
          echo "Usage: $0 <domain> [\"extra arguments\"]"
    exit
fi

# TODO: hardwired directory
cached_assertions="../../../../build/data/ciaopp_lib_cache"
echo "Generating cached assertions for libraries $cached_assertions ..."
gen_lib_cache $cached_assertions

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

domains=($1)

errors=0

for i in "${tests[@]}" ; do
    for j in "${domains[@]}" ; do
        ./check_config.sh $i $j
        errors=$($errors+$?) # add errors
    done
done

popd > /dev/null 2>&1

echo "$errors BENCHMARKS WITH ERROR(S) FOUND."

if [ "$errors" -ne 0 ]; then
    errors=1
else
    errors=0
fi

exit $errors
