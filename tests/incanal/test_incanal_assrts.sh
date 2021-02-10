#!/bin/bash

_base=$(e=$0;while test -L "$e";do d=$(dirname "$e");e=$(readlink "$e");\
    cd "$d";done;cd "$(dirname "$e")";pwd -P)

tests=(trust_C1_+ trust_C1_+bot trust_C2_+ trust_C3_+ trust_C1_- trust_C1_-bot trust_C2_- trust_C3_- trust_S1 trust_S2 trust_S3 trust_S4 trust_C_exported trust_C_internal)
# TODO: keep up to date w.r.t. test_dirs:inc_trust_call_test/1 and test_dirs:inc_trust_succ_test/1
res_dir=test_results
trace=""
trace=trace # comment to remove tracing

domain=$1
tag=incanal-assrts
inc_configs=("" "incremental")

total_checks=14

if [ "$#" -ne 1 ]; then
    echo "Usage: ./test_incanal_assrts.sh <domain>"
    exit
fi

pushd $_base > /dev/null 2>&1

rm -rf $res_dir/*$domain*$tag*

ciaopp --gen-lib-cache

echo "Running tests $domain (monolithic analysis)..."

errors=0

mkdir -p "$res_dir"/logs/

for i in "${tests[@]}" ; do
    echo "Running $i"
    for k in "${inc_configs[@]}" ; do
        # log_file="$res_dir"/logs/"$i"_"$k"_"$domain"_assertions.log
        echo "CMD: ciaopp-test incanal $i $domain $k assertions $trace --user_tag $tag"
        ciaopp-test incanal "$i" $domain $k assertions $trace --user_tag $tag
    done

    bench_res_dir=$(find $res_dir -name "$i*$domain*$tag")
    echo "Checking $i"
    echo "CMD: ciaopp-dump cmp --sequence $bench_res_dir/mon-noninc/detailed_step_results $res_dir/mon-inc/detailed_step_results $domain"
    ciaopp-dump cmp --sequence "$bench_res_dir"/mon-noninc/detailed_step_results "$bench_res_dir"/mon-inc/detailed_step_results $domain
    errors=$(($errors+$?)) # add errors
done

popd > /dev/null 2>&1

noerrors=$(expr $total_checks - $errors)

echo "$noerrors/$total_checks passed."

if [ "$errors" -ne 0 ]; then
    errors=1
else
    errors=0
fi

exit $errors
