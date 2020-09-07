#!/bin/bash

_base=$(e=$0;while test -L "$e";do d=$(dirname "$e");e=$(readlink "$e");\
                                   cd "$d";done;cd "$(dirname "$e")";pwd -P)

pushd $_base > /dev/null 2>&1

tests=(hanoi aiakl qsort progeom bid rdtok cleandirs prolog_read warplan boyer peephole witt ann manag_proj_simple check_links)
configs=(add del)

if [ "$#" -ne 1 ]; then
    echo "Usage: $0 <domain>"
    exit
fi

domain=$1

function compare_directories_add {
    i=$1
    k=$2
    j=$3
    mon=$4
    echo "%%%%%%%%%%%%%%%%%%%%%%%% COMPARING $i $k for $j $mon %%%%%%%%%%%%%% "
    echo
    echo
    ciaopp-dump cmp --sequence "test_results/$k-$i-not_rand-1-$j-dd/$mon-noninc/detailed_step_results" "test_results/$k-$i-not_rand-1-$j-dd/$mon-inc/detailed_step_results" "$j"
    echo "%%%%%%%%%%%%%%%%%%% END OF COMPARISON $i $k for $j mon %%%%%%%%%%%% "
    echo
    echo
}

function compare_directories_del {
    i=$1
    k=$2
    j=$3
    mon=$4
    bu=$5

    echo "%%%%%%%%%%%%%%%%%%%%%%%% COMPARING $i $k for $j $mon $bu %%%%%%%%%%%%%% "
    echo
    echo
    ciaopp-dump cmp --sequence "test_results/$k-$i-not_rand-1-$j-dd/$mon-noninc-top_down/detailed_step_results" "test_results/$k-$i-not_rand-1-$j-dd/$mon-inc-$bu/detailed_step_results" "$j"
    echo "%%%%%%%%%%%%%%%%%%% END OF COMPARISON $i $k for $j $mon $bu %%%%%%%%%%%% "
    echo
    echo
}


for k in "${tests[@]}" ; do
    i=add
    compare_directories_add $i $k $domain mon
    compare_directories_add $i $k $domain mod

    i=del
    compare_directories_del $i $k $domain mon top_down
    compare_directories_del $i $k $domain mon bottom_up

    compare_directories_del $i $k $domain mod top_down
    compare_directories_del $i $k $domain mod bottom_up
done

popd > /dev/null 2>&1
