#!/bin/bash

_base=$(e=$0;while test -L "$e";do d=$(dirname "$e");e=$(readlink "$e");\
                                   cd "$d";done;cd "$(dirname "$e")";pwd -P)

tests=(hanoi aiakl qsort progeom bid rdtok cleandirs prolog_read warplan boyer peephole witt ann manag_proj_simple check_links)

pushd $_base > /dev/null 2>&1
ciaopp --gen-lib-cache

ulimit -t 600

domain=terms

for i in "${tests[@]}" ; do
    ciaopp-test incanal "$i" $domain --edit_type del --steps 0 --user_tag montypes
    ciaopp-test incanal "$i" $domain --edit_type del modular --steps 0 --user_tag montypes

    echo "Checking $i ---------"
    ciaopp-dump cmp --no-missing test_results/$i-del-not_rand-1-$domain-dd-montypes/mon-noninc-top_down/detailed_step_results/inc_reg_1/*.dump_inc test_results/$i-del-not_rand-1-$domain-dd-montypes/mod-noninc-top_down/detailed_step_results/inc_reg_1/ $domain

done

popd > /dev/null 2>&1

# "errors" are not relevant here, they can mean that modular is more precise
exit 0
