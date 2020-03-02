#!/bin/bash

_base=$(e=$0;while test -L "$e";do d=$(dirname "$e");e=$(readlink "$e");\
                                   cd "$d";done;cd "$(dirname "$e")";pwd -P) || return

benchs=(hanoi aiakl qsort progeom bid rdtok cleandirs prolog_read warplan boyer peephole witt ann manag_proj_simple check_links)

edit=$1
absint=$2

rm -f "$_base"/csv/"$absint"_$edit.csv

for bench in "${benchs[@]}" ; do
    ./generate_exp_csv.sh $bench $absint $edit
    cat /tmp/ciaopp-syn/$absint-$edit-$bench/"$bench"_"$absint"_"$edit".csv >> "$_base"/csv/"$absint"_$edit.csv
done
echo "Results generated in $_base/csv/$absint\_$edit.csv"
