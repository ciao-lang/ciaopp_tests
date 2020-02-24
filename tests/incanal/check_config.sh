#!/bin/bash

_base=$(e=$0;while test -L "$e";do d=$(dirname "$e");e=$(readlink "$e");\
                                   cd "$d";done;cd "$(dirname "$e")";pwd -P)

function show_help {
    echo "Usage: $0 <bench_name> <domain> [<extra (tags)>]"
    echo
}

function check_args {
    case $1 in
        2|3|4)
        ;;
        *)
            echo "Wrong arguments"
	          show_help
	          exit
	  esac
}

function compare_directories_add {
    i=$1
    k=$2
    j=$3
    mon=$4
    extra=$5

    echo "CHECKING $k $i for $j $mon ... "
          ciaopp-dump cmp "test_results/$k-$i-not_rand-1-$j-dd$extra/$mon-noninc/detailed_step_results" "test_results/$k-$i-not_rand-1-$j-dd$extra/$mon-inc/detailed_step_results" "$j"
}

function compare_directories_del {
    i=$1
    k=$2
    j=$3
    mon=$4
    bu=$5
    extra=$6

    echo "CHECKING $k $i for $j $mon $bu... "
    ciaopp-dump cmp "test_results/$k-$i-not_rand-1-$j-dd$extra/$mon-noninc-top_down/detailed_step_results" "test_results/$k-$i-not_rand-1-$j-dd$extra/$mon-inc-$bu/detailed_step_results" "$j"
}

pushd $_base > /dev/null 2>&1

check_args $# $2 $3

bench_name=$1
domain=$2
extra=$3

j=$domain
k=$bench_name

total_checks=6

i=add
compare_directories_add $i $k $j mon $extra
exit_add_mon=$?
compare_directories_add $i $k $j mod $extra
exit_add_mod=$?

i=del
compare_directories_del $i $k $j mon top_down $extra
exit_del_mon_td=$?
compare_directories_del $i $k $j mon bottom_up $extra
exit_del_mon_bu=$?

compare_directories_del $i $k $j mod top_down $extra
exit_del_mod_td=$?
compare_directories_del $i $k $j mod bottom_up $extra
exit_del_mod_bu=$?

errors=$(expr $exit_add_mon + $exit_add_mod + $exit_del_mon_td + $exit_del_mon_bu + $exit_del_mod_td + $exit_del_mod_bu)

echo "$bench_name;$domain;$exit_add_mon;$exit_add_mod;$exit_del_mon_td;$exit_del_mon_bu;$exit_del_mod_td;$exit_del_mod_bu" >> test_checking_log.csv

noerrors=$(expr $total_checks - $errors)
echo "$bench_name $domain $noerrors/$total_checks passed."

popd > /dev/null 2>&1

if [ "$errors" -ne 0 ]; then
    errors=1
else
    errors=0
fi

exit $errors
