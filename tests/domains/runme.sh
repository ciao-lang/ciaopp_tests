#!/bin/sh

# Dummy script to test domains

mode=check

# set_pp_flag(output_lang, raw).
function trydom() { # module domain
    local mod dom
    mod=$(basename "$1" .pl)
    dom=$2
    out=$mod.co_$dom.pl
    outlog=$mod.co_$dom.log
    out_good=$out-good
    outlog_good=$outlog-good
    if [ $mode == "check" ]; then
        cat <<EOF
DOMAIN: $2; MODULE: $mod
EOF
        ciaopp > "$outlog" 2>&1 <<EOF
module('$1').
analyze($dom).
output('${mod}.co_${dom}.pl').
halt.
EOF
        diff "$out" "$out_good"
        diff "$outlog" "$outlog_good"
    elif [ $mode == "save" ]; then
        cp "$out" "$out_good"
        cp "$outlog" "$outlog_good"
    fi
}
dir=../../../ciaopp_extra/tests/benchs/modes
#trydom "$dir"/mmatrix-w ptypes
#trydom "$dir"/peephole depthk
#trydom "$dir"/deriv path
# trydom shfr_exp shfr # (does not finish)
#trydom "$dir"/shfr_exp sharefree_clique
#

case $1 in
    save) mode=save ;;
    *) mode=check ;;
esac

sharing_doms=
sharing_doms="$sharing_doms gr" # gr
sharing_doms="$sharing_doms def" # def
sharing_doms="$sharing_doms share" # sharing
sharing_doms="$sharing_doms shfr" # sharefree
sharing_doms="$sharing_doms shfrnv" # sharefree_non_var
sharing_doms="$sharing_doms shfret" # shfret
sharing_doms="$sharing_doms shareson" # shareson
sharing_doms="$sharing_doms shfrson" # shfrson
sharing_doms="$sharing_doms son" # sondergaard
sharing_doms="$sharing_doms share_amgu" # sharing_amgu
sharing_doms="$sharing_doms sharefree_amgu" # sharefree_amgu
sharing_doms="$sharing_doms shfrlin_amgu" # shfrlin_amgu
sharing_doms="$sharing_doms share_clique" # sharing_clique
sharing_doms="$sharing_doms share_clique_1" # sharing_clique_1
sharing_doms="$sharing_doms sharefree_clique" # sharefree_clique
sharing_doms="$sharing_doms share_clique_def" # sharing_clique_def
sharing_doms="$sharing_doms sharefree_clique_def" # sharefree_clique_def
sharing_doms="$sharing_doms bshare" # bshare
for prg in peephole; do
    for dom in $sharing_doms; do
        trydom "$dir"/$prg.pl $dom
    done
done

# Domains and their module
# aeq.pl --> aeq
# def.pl --> def
# deftypes.pl --> deftypes
# depthk.pl --> depthk
# eterms.pl --> eterms
# etermsvar.pl --> etermsvar
# fd.pl --> frdef
# fr_top.pl --> fr
# gr.pl --> gr
# lsign.pl --> lsign
# lsigndiff.pl --> difflsign
# nonrel_intervals.pl --> nonrel_intervals
# pd.pl --> pd
# pdb.pl --> pdb
# polyhedra.pl --> polyhedra
# ptypes.pl --> ptypes
# sharefree.pl --> shfr
# sharefree_amgu.pl --> sharefree_amgu
# sharefree_clique.pl --> sharefree_clique
# sharefree_clique_def.pl --> sharefree_clique_def
# sharefree_non_var.pl --> shfrnv
# shareson.pl --> shareson
# sharing.pl --> share
# sharing_amgu.pl --> share_amgu
# sharing_clique.pl --> share_clique
# sharing_clique_1.pl --> share_clique_1
# sharing_clique_def.pl --> share_clique_def
# shfret.pl --> shfret
# shfrlin_amgu.pl --> shfrlin_amgu
# shfrson.pl --> shfrson
# sondergaard.pl --> son
# svterms.pl --> svterms
# termsd.pl --> terms
# top_path_sharing.pl --> path

