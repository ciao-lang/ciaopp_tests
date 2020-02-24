#!/bin/bash

_base=$(e=$0;while test -L "$e";do d=$(dirname "$e");e=$(readlink "$e");\
                                   cd "$d";done;cd "$(dirname "$e")";pwd -P)

pushd $_base > /dev/null 2>&1

settings_add=(mon-noninc mon-inc mod-noninc)
settings_del=(mon-noninc-top_down mon-inc-top_down mon-inc-bottom_up mod-noninc-top_down mon-inc-top_down)

mkdir -p graphs

domains=(pdb gr def shfr)

for domain in "${domains[@]}"; do
    for set in "${settings_add[@]}"; do

        gnuplot_domain=$domain gnuplot_edit_type=add gnuplot_exp_setting1=$set gnuplot_exp_setting2=mod-inc gnuplot scatter_graph.plg
        mv total-scatter.pdf graphs/total-scatter-$set-$domain.pdf
        mv fixpoint-scatter.pdf graphs/fixpoint-scatter-$set-$domain.pdf
    done

    for set in "${settings_del[@]}"; do
        gnuplot_domain=$domain gnuplot_edit_type=del gnuplot_exp_setting1=$set gnuplot_exp_setting2=mod-inc-bottom_up gnuplot scatter_graph.plg
        mv total-scatter.pdf graphs/total-scatter-$set-$domain.pdf
        mv fixpoint-scatter.pdf graphs/fixpoint-scatter-$set-$domain.pdf
    done
done

popd > /dev/null 2>&1
