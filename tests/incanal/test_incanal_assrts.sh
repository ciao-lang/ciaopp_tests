#!/bin/bash

_base=$(e=$0;while test -L "$e";do d=$(dirname "$e");e=$(readlink "$e");\
    cd "$d";done;cd "$(dirname "$e")";pwd -P)

tests=(trust_C1_+ trust_C1_+bot trust_C2_+ trust_C3_+ trust_C1_- trust_C1_-bot trust_C2_- trust_C3_- trust_S1 trust_S2 trust_S3 trust_S4 trust_C_exported trust_C_internal)
# TODO: keep up to date w.r.t. test_dirs:inc_trust_call_test/1 and test_dirs:inc_trust_succ_test/1

if [ "$#" -ne 1 ]; then
          echo "Usage: ./test_incanal_assrts.sh <domain>"
    exit
fi

pushd $_base > /dev/null 2>&1

bench_driver=incanal_intermod_bench_driver
ana_checker=ciaopp-dump-cmp
res_dir=test_results

# TODO: hardwired directory
cached_assertions="../../../../build/data/ciaopp_lib_cache"

domain=$1
tag=incanal-assrts
inc_configs=("" "incremental")

rm -rf $res_dir/*$domain*$tag*

if [ ! -d "$cached_assertions" ]; then
    echo "Generating cached assertions for libraries $cached_assertions ..."
    gen_lib_cache $cached_assertions
fi


echo "Running tests $domain (monolithic analysis)..."

errors=0

mkdir -p "$res_dir"/logs/

for i in "${tests[@]}" ; do
    echo "Running $i"
    for k in "${inc_configs[@]}" ; do
        log_file="$res_dir"/logs/"$i"_"$k"_"$domain"_assertions.log
        ./$bench_driver "$i" add 1 $domain $k assertions --user_tag $tag &> "$log_file"
    done

    bench_res_dir=$(find $res_dir -name "$i*$domain*$tag")
    echo "Checking $i"
    $ana_checker "$bench_res_dir"/mon-noninc/detailed_step_results "$bench_res_dir"/mon-inc/detailed_step_results $domain
    errors=$(($errors+$?)) # add errors
done

popd > /dev/null 2>&1

echo "$errors BENCHMARKS WITH ERROR(S) FOUND."

if [ "$errors" -ne 0 ]; then
    errors=1
else
    errors=0
fi

exit $errors
