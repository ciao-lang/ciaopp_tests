#!/bin/bash

_base=$(e=$0;while test -L "$e";do d=$(dirname "$e");e=$(readlink "$e");\
        cd "$d";done;cd "$(dirname "$e")";pwd -P)

tests=(ann aiakl bid boyer cleandirs hanoi peephole progeom warplan prolog_read  witt qsort rdtok)
configs=(add del)

if [ "$#" -ne 1 ]; then
	  echo "Usage: ./run_all.sh <domain>"
    exit
fi

pushd $_base > /dev/null 2>&1

echo "Compiling tests..."
./compile.sh

mkdir -p test_results/

echo "Running tests..."

for k in "${configs[@]}" ; do
    for i in "${tests[@]}" ; do
	./run_configs.sh $i $k "$1"
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
