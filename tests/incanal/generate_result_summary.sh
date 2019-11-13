#!/bin/bash

_base=$(e=$0;while test -L "$e";do d=$(dirname "$e");e=$(readlink "$e");\
        cd "$d";done;cd "$(dirname "$e")";pwd -P) || return

# Fixed parameters of the script

sum_driver="$_base"/summarize_stat
gather_driver="$_base"/gather_stats
norm_driver="$_base"/prolog_to_table

stats_dir_filter="stats"
bsumfile=summ
norm_opts="norm t01010111100"
mask_all="t11111111111"
empty_row="- - 0 0 0 0 0 0 0 0 0 0 0"

function check_args {
    #if [ "$1" -ne 1 ]; then
    #echo "TODO: [Not implemented yet] Cleaning previous results..."
    if [ "$1" -ne 2 ]; then
        echo "Wrong arguments"
        show_help
        exit
    fi
}

function show_help {
    echo "Usage: ./generate_result_summary.sh <res_dir> <filter>"
}

function summarize_benchs_directory {

    test_dir=$1
    echo "Summarizing... "

    benchs=$(ls -d "$test_dir"/$filter/)
    for b in ${benchs[@]} ; do
        echo "Summarizing $b"
        summarize_test "$b$bsumfile" "$b"
        echo "$empty_row" >> $plot_file
    done
}

function summarize_test { # execute this function for each of the filtered tests
    # results file composed with the name of the directory (base) ++ '.pl'
    results_base=$1
    test_dir=$2

    results_file="$results_base".pl
    rm -f "$results_file"

    bench=$(basename "$test_dir")
    bench_name=$(echo "$bench" | awk -F'-' '{print $1}') #benchmark name

    config_dirs=$(ls -dtr "$test_dir"/*/)

    echo "$bench_name 0 0 0 0 0 0 0 0 0 0 0 0" >> "$plot_file"
    echo "$bench_name 0 0 0 0 0 0 0 0 0 0 0 0" >> "$abs_file"
    echo "$bench_name & 0 & 0 & 0 & 0 & 0 & 0 & 0 & 0 & 0 & 0 & 0 & 0" >> "$paper_file"

    echo "summary(benchName, load, compDiff, restore, procDiff, procAssrts, preProc, incAct, analyze, 'GAT', 'SaveGAT', contextChs)." >> "$results_file"

    for dir in ${config_dirs[@]} ; do
        stats_dir=$dir/stats
        bench_config=$(basename "$dir")

        echo "Processing $stats_dir..."
        echo ""
        echo "summary('$bench_config'," >> "$results_file"

        $sum_driver "$stats_dir" >> "$results_file"
        $gather_driver "$stats_dir"

        # get stats file
        gather_file=$(ls "$stats_dir"/stats.pl)
        $norm_driver "$gather_file" "$gather_file"_norm.data gnuplot "$mask_all"
    done

    #Generate summarized data
    $norm_driver "$results_file" "$results_base"_norm.data gnuplot $norm_opts
    cat "$results_base"_norm.data >> "$plot_file"

    $norm_driver "$results_file" "$results_base"_abs.data gnuplot "$mask_all"
    cat "$results_base"_abs.data >> "$abs_file"

    $norm_driver "$results_file" "$results_base".tex latex "$mask_all"
    cat "$results_base".tex >> "$paper_file"

    $norm_driver "$results_file" "$results_base".csv csv "$mask_all"

    # Generate detailed graph
    export det_graph_title="$test_dir"
    export output_dgraph_file="$details_dir"_det_graph_title

    pushd "$test_dir" > /dev/null 2>&1
    gnuplot "$detail_plot_script"

    mv details.pdf ../../"$details_dir"/"$bench".pdf
    popd > /dev/null 2>&1

}

check_args $#

res_dir=$1
filter=$2
plot_script="$_base"/summ_graph.plg
detail_plot_script="$_base"/detailed_graph.plg
graph_dir_name=graphs

date=$(date +"%Y%m%d%H%M%S")

graph_dir="$res_dir"/$graph_dir_name
details_dir="$graph_dir"/details_$date
plot_file="$graph_dir"/to_plot.data
abs_file="$details_dir"/abs_numbers.data # raw numbers
paper_file="$details_dir"/abs_numbers.tex # raw numbers

pushd "$_base" > /dev/null 2>&1 || exit

mkdir -p "$graph_dir"
mkdir -p "$details_dir"

# Clean plot file
rm -f "$plot_file"

export graph_title="$filter"

echo "bench load compDiff restore procDiff procAssrts preProc incAct analyze GAT SaveGAT memory contextChs" >> "$plot_file"

# Processing benchmarks raw data
summarize_benchs_directory "$res_dir"

# Plot summaries
gnuplot "$plot_script"

cp statistics.pdf "$graph_dir"/statistics_"$date".pdf

cp "$plot_file" "$details_dir"/

open "$graph_dir"/statistics_"$date".pdf

popd > /dev/null 2>&1 || exit
