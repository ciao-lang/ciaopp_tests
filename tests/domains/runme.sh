#!/bin/bash

# Dummy script to test domains

# set_pp_flag(output_lang, raw).
function trydom() { # module domain
    local mod dom
    mod=$(basename "$1" .pl)
    dom=$2
    out=$mod.co_$dom.pl
    outlog=$mod.co_$dom.log
    out_good=$out-good
    outlog_good=$outlog-good
    cat <<EOF
DOMAIN: $2; MODULE: $mod
EOF
# set_pp_flag(output_lang, raw).
    if [ $mode == "check" ]; then
        ciaopp > "$outlog" 2>&1 <<EOF
module('$1').
analyze($dom).
output('${mod}.co_${dom}.pl').
halt.
EOF
    fi
    if [ $mode == "check" ] || [ $mode = "compare" ]; then
        diff "$out" "$out_good"
        diff <(notime "$outlog") <(notime "$outlog_good")
    elif [ $mode == "save" ]; then
        cp "$out" "$out_good"
        cp "$outlog" "$outlog_good"
    fi
}
# Erase time measures
notime() { # file
    sed -e "s/[0-9.]* msec\./msec/" "$1"
}
function tryprgs() {
    for prg in $prgs; do
        for dom in $doms; do
            trydom "$dir"/$prg.pl $dom
        done
    done
}
function add_dom() {
    doms="$doms $1"
}

# ===========================================================================

function try_sharing() {
    echo "SHARING DOMAINS" # TODO: mode domains?
    doms=
    add_dom "fr" # fr_top.pl | var+posdeps
    add_dom "def" # def.pl | ground+covered
    add_dom "frdef" # fd.pl | var+posdeps+ground+covered
    add_dom "gr" # gr.pl | ground
    add_dom "share" # sharing.pl | ground+mshare
    add_dom "shfr" # sharefree.pl | ground+mshare+var
    add_dom "shfrnv" # sharefree_non_var.pl | ground+mshare+var+nonvar
    add_dom "shfret" # shfret.pl | ground+mshare+var+regtype
    add_dom "shareson" # shareson.pl | ground+mshare+linear
    add_dom "shfrson" # shfrson.pl | ground+mshare+var+linear
    add_dom "son" # sondergaard.pl | ground+mshare+linear
    add_dom "share_amgu" # sharing_amgu.pl | ground+mshare
    add_dom "sharefree_amgu" # sharefree_amgu.pl | ground+mshare+var
    add_dom "shfrlin_amgu" # shfrlin_amgu.pl | ground+mshare+var+linear
    add_dom "share_clique" # sharing_clique.pl | ground+mshare+clique
    add_dom "share_clique_1" # sharing_clique_1.pl | ground+mshare+clique_1
    add_dom "sharefree_clique" # sharefree_clique.pl | ground+mshare+var+clique
    add_dom "share_clique_def" # sharing_clique_def.pl | ground+mshare+clique+not_in_output(covered)
    add_dom "sharefree_clique_def" # sharefree_clique_def.pl | ground+mshare+clique+not_in_output(covered)
    # add_dom "bshare" # bshare.pl # TODO: not working
    
    prgs="peephole"
    dir=../../../ciaopp_extra/tests/benchs/modes
    tryprgs
}

# ===========================================================================

function try_types() {
    echo "TYPES DOMAINS"
    doms=
    add_dom "deftypes" # deftypes.pl
    add_dom "eterms" # eterms.pl
    # add_dom "etermsvar" # etermsvar.pl # TODO: buggy
    add_dom "ptypes" # ptypes.pl
    add_dom "svterms" # svterms.pl
    add_dom "terms" # termsd.pl

    # prgs="deriv" # TODO: too expensive
    # dir=../../../ciaopp_extra/tests/benchs/types
    prgs="witt"
    dir=../../../ciaopp_extra/tests/benchs/modes
    tryprgs
}

# ===========================================================================

#trydom "$dir"/mmatrix-w ptypes
#trydom "$dir"/peephole depthk
#trydom "$dir"/deriv path
# trydom shfr_exp shfr # (does not finish)
#trydom "$dir"/shfr_exp sharefree_clique

case $1 in
    save) mode=save ;;
    compare) mode=compare ;;
    check) mode=check ;;
    *) mode=check ;; # (default)
esac
# try_sharing
try_types

# ---------------------------------------------------------------------------
# Domains and their module

# ---------------------------------------------------------------------------
# (other domains)
# aeq.pl --> aeq
# depthk.pl --> depthk
# lsign.pl --> lsign
# lsigndiff.pl --> difflsign
# nonrel_intervals.pl --> nonrel_intervals
# pd.pl --> pd
# pdb.pl --> pdb
# polyhedra.pl --> polyhedra
# top_path_sharing.pl --> path

# ---------------------------------------------------------------------------
# (includes sharing and types domains)
# nfdet.pl -> nfdet
# nfplai.pl -> nf
# detplai.pl -> det

# ---------------------------------------------------------------------------
# (depends on PPL)
# polyhedra.pl --> polyhedra [depends on PPL]

# ---------------------------------------------------------------------------
# (ciaopp_fpnum bundle)
# nonrel_fintervals.pl --> nonrel_fintervals

# ---------------------------------------------------------------------------
# (ciaopp_cost bundle)
# res_plai.pl -> res_plai
# res_plai_stprf.pl -> res_plai_stprf
# sized_types.pl -> sized_types

# ---------------------------------------------------------------------------
# (ciaopp_java bundle)
# java_nullity -> java_nullity
# oo_son -> oo_son
# oo_shnltau -> oo_shnltau
# oo_types -> oo_types
# java_cha -> java_cha



