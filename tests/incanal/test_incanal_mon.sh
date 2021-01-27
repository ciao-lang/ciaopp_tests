#!/bin/bash

_base=$(e=$0;while test -L "$e";do d=$(dirname "$e");e=$(readlink "$e");\
                                   cd "$d";done;cd "$(dirname "$e")";pwd -P)

tests=(hanoi_mon)

if [ "$#" -ne 1 ]; then
    echo "Usage: ./test_incanal_assrts.sh <domain>"
    exit
fi

pushd $_base > /dev/null 2>&1

res_dir=test_results

./gen_lib_cache.sh

domain=$1
tag=incanal-mon

total_checks=1

inc_configs=("" "incremental")

rm -rf $res_dir/*$domain*$tag*

echo "Running tests $domain (monolithic analysis)..."

terrors=0
errors=0

for i in "${tests[@]}" ; do
    echo "Running $i"
    for k in "${inc_configs[@]}" ; do
        log_file="$res_dir"/logs/"$i"_"$k"_"$domain"_assertions.log
        echo "COMMAND ciaopp-test incanal $i $domain $k monolithic_driver"
        ciaopp-test incanal "$i" $domain $k monolithic_driver --user_tag $tag
    done

    bench_res_dir=$(find $res_dir -name "$i*$domain*$tag*")
    echo "Checking $i"
    for f in "$bench_res_dir"/mon-noninc/detailed_step_results/*; do
        base="$(basename -- $f)"
        ciaopp-dump cmp --sequence "$f"/*.dump "$bench_res_dir"/mon-inc/detailed_step_results/$base/*.dump $domain
        errors=$(($errors+$?)) # add errors
    done
    if [ "$errors" -ne 0 ]; then
        terrors=$(expr $terrors + 1)
    fi
done

popd > /dev/null 2>&1

noerrors=$(expr $total_checks - $terrors)

echo "Total: $total_checks Passed: $noerrors"

if [ "$terrors" -ne "0" ]; then
    terrors=1
fi

exit $terrors
