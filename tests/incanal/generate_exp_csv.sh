#!/bin/bash

_base=$(e=$0;while test -L "$e";do d=$(dirname "$e");e=$(readlink "$e");\
                                   cd "$d";done;cd "$(dirname "$e")";pwd -P) || return

if [ "$#" -ne 3 ]; then
    echo "Usage: $0 <bench> <absint> <edit>"
    exit
fi

tmp_dir=ciaopp-syn
bench=$1
absint=$2
edit=$3
tests_location="test_results"
test_dir="$_base"/$tests_location/"$bench-$edit-not_rand-1-$absint-dd"
tmp_bench_dir=/tmp/$tmp_dir/$absint-$edit-$bench
common_stat_file="$tmp_bench_dir"/common_"$bench"_"$absint"_$edit.csv
final_csv="$tmp_bench_dir"/"$bench"_"$absint"_$edit.csv

if [ ! -d "$test_dir" ]; then
    echo "$test_dir not found"
    exit
fi

mkdir -p /tmp/$tmp_dir

# Structure of the results directory:
# bench-add|del-not_rand-1-absint-dd
#  - inc settings (mon|mod-noninc|inc)
#      - state_X # the sources w.r.t. the previous iteration
#      - detailed_step_results # results per it
#           - inc_reg_X  (.dump_inc & .reg)

rm -rf "$tmp_bench_dir" # remove previous results
mkdir "$tmp_bench_dir"

# syntactic characteristics
i=0
curr_dir=$(ls -d "$test_dir"/mon-noninc*/state_$i)
if [ ! -d "$curr_dir" ]; then
    echo "Test directory not found $curr_dir"
    exit 1
fi

test_dir_base=${curr_dir%/state_0}

tmp_stat_csv=$bench-$absint-$edit-stat.csv

ciaopp-dump syntactic --print-header "$tmp_bench_dir"/$tmp_stat_csv
echo -n "bench,step,mods," > "$common_stat_file" # overwriting
cat "$tmp_bench_dir"/$tmp_stat_csv >> "$common_stat_file"
ciaopp-dump stats --print-header "$tmp_bench_dir"/$tmp_stat_csv
cat "$tmp_bench_dir"/$tmp_stat_csv >> "$common_stat_file"
echo "" >> "$common_stat_file"

nmods=$(ls -1 "$curr_dir"/*.pl | wc -l)
nmods=$(expr $nmods + 0)

while [ -d "$curr_dir" ]; do
    cp -r "$curr_dir"/*.pl "$tmp_bench_dir"/
    ciaopp-dump syntactic "$tmp_bench_dir"/$tmp_stat_csv "$tmp_bench_dir"/*.pl &> /dev/null
    echo -n "$bench,$i,$nmods," >> "$common_stat_file"
    cat "$tmp_bench_dir"/$tmp_stat_csv >> "$common_stat_file"
    let i=i+1

    ciaopp-dump stats "$tmp_bench_dir"/$tmp_stat_csv "$test_dir_base"/detailed_step_results/inc_reg_"$i"/*.dump_inc &> /dev/null
    cat "$tmp_bench_dir"/$tmp_stat_csv >> "$common_stat_file"
    echo "" >> "$common_stat_file"
    curr_dir="$test_dir_base"/state_$i
done

echo "Common statistics generated in $common_stat_file"

# generate experiments result for each
"$_base"/generate_result_summary.sh "$tests_location" "$bench-$edit-*$absint*" "--nopdf"

# First we need to generate the headers for the csv files of the experiments
for test in "$_base"/"$tests_location"/"$bench"-"$edit"-not_rand-1-"$absint"-dd/*/stats/stats.pl_norm.csv
do
    config=${test%/stats/stats.pl_norm.csv}
    config="$(basename "$config")"
    echo -e "$config-it,$config-load,$config-compDiff,$config-restore,$config-procDiff,$config-procAssrts,$config-preProc,$config-incAct,$config-analyze,$config-GAT,$config-SaveGAT\n$(cat "$test")" > "$test"
done

# Paste all generated csv
paste -d "," "$common_stat_file" "$_base"/$tests_location/"$bench"-"$edit"-not_rand-1-"$absint"-dd/*/stats/stats.pl_norm.csv > "$final_csv"

echo "csv generated in $final_csv"
