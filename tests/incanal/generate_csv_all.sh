#!/bin/bash

_base=$(e=$0;while test -L "$e";do d=$(dirname "$e");e=$(readlink "$e");\
                                   cd "$d";done;cd "$(dirname "$e")";pwd -P) || return

benchs=(ann aiakl bid boyer cleandirs hanoi peephole progeom warplan prolog_read witt qsort rdtok)
editions=(add del)
domains=(gr def shfr pdb)

mkdir -p "$_base"/csv

for edit in "${editions[@]}" ; do
    for absint in "${domains[@]}" ; do
        rm -f "$_base"/csv/"$absint"_$edit.csv
        for bench in "${benchs[@]}" ; do
            ./generate_exp_csv.sh $bench $absint $edit
            cat /tmp/ciaopp-syn/$bench/"$bench"_"$absint"_"$edit".csv >> "$_base"/csv/"$absint"_$edit.csv
        done
        echo "Results generated in $_base/csv/$absint\_$edit.csv"
    done
done
