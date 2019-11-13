#!/bin/bash

_base=$(e=$0;while test -L "$e";do d=$(dirname "$e");e=$(readlink "$e");\
        cd "$d";done;cd "$(dirname "$e")";pwd -P) || exit

function show_help {
    echo "Usage: ./run_configs.sh <bench_name> <add/del> <domain>"
    echo
}

function check_args {
    if [ "$1" -ne 3 ]; then
        if [ "$1" -ne 4 ]; then
            echo "Wrong arguments"
            show_help
            exit
        fi
    fi
    if [ "$2" != "add" ]; then
              if [ "$2" != "del" ]; then
                  echo "Wrong simulation option"
                  show_help
                  exit
              fi
    fi

    case $3 in
        shfr|def|pdb|gr)
        ;;
        *)
            echo "Wrong domain option"
                  show_help
                  exit
            ;;
    esac
}

pushd "$_base" > /dev/null 2>&1 || exit

check_args $# $2 $3

bench_name=$1
test_type=$2 #add or del
domain=$3
user_opts=$4

bench_driver=incanal_intermod_bench_driver
ana_checker=analysis_results_checker

mod_config=(monolithic modular)
inc_config=("" "incremental")
del_config=("top_down" "bottom_up")

echo "Performing tests..."

res_dir=test_results

# rm -rf "$res_dir"/"$bench_name"-"$test_type"-*"$domain"*

bench_opts="under_all"

echo "Bench options: $domain $bench_opts $user_opts"

mkdir -p "$res_dir"/logs

function run_config {
    mod=$1
    inc=$2
    log_file_end=$3

    log_file="$res_dir"/logs/"$bench_name"_"$test_type"_"$log_file_end".log

    echo "Testing $bench_name for $mod $inc ..."
    rm -f "$log_file"
    echo "Logs are being printed in $log_file"

    ./$bench_driver "$bench_name" "$test_type" 1 $domain $rand "$mod" $inc $bench_opts $user_opts &> "$log_file"

}

if [ "$test_type" == "add" ]; then
   for i in "${mod_config[@]}" ; do
       for j in "${inc_config[@]}" ; do
           log_file_end="$i"_"$j"
           run_config "$i" "$j" "$log_file_end"
        done
   done
else
   for i in "${mod_config[@]}" ; do
       for j in "${inc_config[@]}" ; do
                 if [ "$j" == "incremental" ]; then
                     for k in "${del_config[@]}" ; do
                               log_file_end="$i"_"$j"_"$k"
                               run_config "$i" "$j $k" "$log_file_end"
                     done
                 else
                     log_file_end="$i"_"$j"
                     run_config "$i" "$j" "$log_file_end"
                 fi
             done
   done
fi


popd > /dev/null 2>&1 || exit
