#!/bin/bash

tests=(hanoi aiakl qsort progeom bid rdtok cleandirs prolog_read warplan boyer peephole witt ann manag_proj_simple check_links)

./gen_lib_cache.sh

for i in "${tests[@]}" ; do
    timeout 300s ciaopp-test incanal "$i" terms --edit_type del --steps 0 --user_tag montypes
    timeout 300s ciaopp-test incanal "$i" terms --edit_type del modular --steps 0 --user_tag montypes

    echo "Checking $i ---------"
    ciaopp-dump cmp --no-missing test_results/$i-del-not_rand-1-terms-dd-montypes/mon-noninc-top_down/detailed_step_results/inc_reg_1/*.dump_inc test_results/$i-del-not_rand-1-terms-dd-montypes/mod-noninc-top_down/detailed_step_results/inc_reg_1/ terms

done

# "errors" are not relevant here, they can mean that modular is more precise
exit 0
