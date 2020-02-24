#!/bin/bash

_base=$(e=$0;while test -L "$e";do d=$(dirname "$e");e=$(readlink "$e");\
                                   cd "$d";done;cd "$(dirname "$e")";pwd -P)

pushd $_base > /dev/null 2>&1

#tests=(hanoi)
tests=(aiakl qsort hanoi rdtok)
#tests=(ann aiakl bid boyer cleandirs hanoi peephole progeom warplan prolog_read  witt qsort rdtok)
configs=(add del)
#configs=(add del)
domains=(pdb)
#domains=(shfr def)


function compare_directories_add {
    i=$1
    k=$2
    j=$3
    mon=$4
    echo "%%%%%%%%%%%%%%%%%%%%%%%% COMPARING $i $k for $j $mon %%%%%%%%%%%%%% "
    echo
    echo
          ciaopp-dump cmp "test_results/$k-$i-not_rand-1-$j-dd-quick-run/$mon-noninc/detailed_step_results" "test_results/$k-$i-not_rand-1-$j-dd-quick-run/$mon-inc/detailed_step_results" "$j"
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
    ciaopp-dump cmp "test_results/$k-$i-not_rand-1-$j-dd-quick-run/$mon-noninc-top_down/detailed_step_results" "test_results/$k-$i-not_rand-1-$j-dd-quick-run/$mon-inc-$bu/detailed_step_results" "$j"
    echo "%%%%%%%%%%%%%%%%%%% END OF COMPARISON $i $k for $j $mon $bu %%%%%%%%%%%% "
    echo
    echo
}


for j in "${domains[@]}" ; do
    for k in "${tests[@]}" ; do
        i=add
        compare_directories_add $i $k $j mon
        compare_directories_add $i $k $j mod

        i=del
        compare_directories_del $i $k $j mon top_down
        compare_directories_del $i $k $j mon bottom_up

        compare_directories_del $i $k $j mod top_down
        compare_directories_del $i $k $j mod bottom_up
    done
done

popd > /dev/null 2>&1
