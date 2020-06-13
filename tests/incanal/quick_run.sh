#!/bin/bash

_base=$(e=$0;while test -L "$e";do d=$(dirname "$e");e=$(readlink "$e");\
        cd "$d";done;cd "$(dirname "$e")";pwd -P)

set -x

./gen_lib_cache.sh

pushd $_base > /dev/null 2>&1

tests=(qsort hanoi aiakl boyer peephole)
configs=(add del)
domains=(shfr)

total_checks=5

echo "Compiling tests..."
./compile.sh

mkdir -p test_results/

echo "Running tests..."

for k in "${configs[@]}" ; do
    for i in "${tests[@]}" ; do
        for j in "${domains[@]}" ; do
            rm -rf test_results/$k*$j*quick-run
            ./run_configs.sh $i $k $j "--user_tag quick-run"
        done
    done
done

errors=0

for i in "${tests[@]}" ; do
    for j in "${domains[@]}" ; do
        ./check_config.sh $i $j "-quick-run"
        errors=$(expr $errors + $?) # add errors
    done
done

noerrors=$(expr $total_checks - $errors)

echo "$noerrors/$total_checks passed."

popd > /dev/null 2>&1

exit $errors
