#!/bin/bash

_base=$(e=$0;while test -L "$e";do d=$(dirname "$e");e=$(readlink "$e");\
                                   cd "$d";done;cd "$(dirname "$e")";pwd -P) || exit

function show_help {
    echo "Usage: $0 <domain> \"[user_opts]\" "
    echo
}

function check_args {
    if [ "$1" -ne 1 ]; then
        if [ "$1" -ne 2 ]; then
            echo "Wrong arguments"
            show_help
            exit
        fi
    fi
}

pushd "$_base" > /dev/null 2>&1 || exit

check_args $#

dom=$1
user_opts=$2

mod=monolithic
res_dir=test_results
fixpos_dir=$dom-fixpos

fixpo_config=(plai dd di)
benchs=(hanoi aiakl qsort progeom bid rdtok cleandirs prolog_read warplan boyer peephole witt ann manag_proj_simple check_links)

rm -rf $res_dir/*fixpo*

for j in "${benchs[@]}" ; do
    bench_dir=$j-$fixpos_dir
    mkdir -p $bench_dir
    for i in "${fixpo_config[@]}" ; do
        mkdir -p $bench_dir/$i
        ciaopp-test incanal $j $dom --edit_type del $i $mod $user_opts --steps 0 --user_tag fixpo
        mv $res_dir/$j-del-not_rand-1-$dom-$i-fixpo/mon-noninc-top_down/* $res_dir/$bench_dir/$i/
        rm -rf $res_dir/$j-del-not_rand-1-$dom-$i-fixpo
    done
done

popd
